package org.scalajs.core.tools.javascript.util

import java.io.{File, PrintWriter}

import org.junit.Assert._
import org.scalajs.core.compiler.ScalaJSPlugin
import org.scalajs.core.compiler.test.util.DirectTest
import org.scalajs.core.ir.{Infos, Trees => js}
import org.scalajs.core.tools.io.VirtualScalaJSIRFile
import org.scalajs.core.tools.javascript.Printers.JSTreePrinter
import org.scalajs.core.tools.javascript.{OutputMode, Traverser, Trees => jsc}
import org.scalajs.core.tools.logging.{Level, ScalaConsoleLogger}
import org.scalajs.core.tools.classpath.builder.PartialClasspathBuilder
import org.scalajs.core.tools.optimizer._
import org.scalajs.core.tools.sem.Semantics

import scala.language.implicitConversions
import scala.reflect.internal.SomePhase
import scala.reflect.internal.util.NoSourceFile
import scala.tools.nsc._
import scala.util.control.ControlThrowable

abstract class JavaScriptASTTest extends DirectTest {
  val semantics = Semantics.Defaults
  val logger = new ScalaConsoleLogger(Level.Info)
  val outputMode = OutputMode.ECMAScript51Isolated

  lazy val resourceClasses = {
    import scala.collection.JavaConversions._

    val resources = this.getClass.getClassLoader.getResources("").toList
    val paths = resources.map(_.getPath).filter(isWatchedClassPath).map(new File(_))

    PartialClasspathBuilder.build(paths)
  }

  val commonHeader = "import scala.scalajs.js\nimport js.annotation.JSExport\n\n"

  private var lastAST: List[jsc.Tree] = _

  class JSAST(val clDefs: List[jsc.Tree]) {
    type Pat = PartialFunction[jsc.Tree, Unit]

    class PFTraverser(pf: Pat, justOne: Boolean = true) extends Traverser {
      private case object Found extends ControlThrowable

      private[this] var finding = false
      private[this] var _found = 0

      def found = {
        find
        _found
      }

      def find: Boolean = {
        finding = true
        try {
          clDefs.map(traverse)
          false
        } catch {
          case Found => true
        }
      }

      def traverse(): Unit = {
        finding = false
        clDefs.map(traverse)
      }

      override def traverse(tree: jsc.Tree): Unit = {
        if (finding && pf.isDefinedAt(tree)) {
          _found += 1
          if(justOne) throw Found
        }

        if (!finding)
          pf.lift(tree)

        super.traverse(tree)
      }
    }

    def has(num: Int, trgName: String)(pf: Pat): this.type = {
      val tr = new PFTraverser(pf, justOne = false)
      val found = tr.found
      assertTrue(s"AST should have $num $trgName, found $found", found == num)
      this
    }

    def has(trgName: String)(pf: Pat): this.type = {
      val tr = new PFTraverser(pf)
      assertTrue(s"AST should have $trgName", tr.find)
      this
    }

    def hasNot(trgName: String)(pf: Pat): this.type = {
      val tr = new PFTraverser(pf)
      assertFalse(s"AST should not have $trgName", tr.find)
      this
    }

    def traverse(pf: Pat): this.type = {
      val tr = new PFTraverser(pf)
      tr.traverse()
      this
    }

    def show: this.type = {
      val writer = new PrintWriter(System.out)
      val printer = new JSTreePrinter(writer)
      clDefs.foreach { x =>
        printer.printTree(x, false)
        printer.println()
      }
      writer.flush()
      this
    }

  }

  def isWatchedClassPath(libPath: String) =
    libPath.contains("javalanglib/target") || libPath.contains("library/target") ||
        libPath.contains("javalib/target") || libPath.contains("library-aux/target") ||
        libPath.contains("scalalib/target")

  override def newScalaJSPlugin(global: Global) = new ScalaJSPlugin(global) {

    override def generatedJSAST(cld: List[js.Tree]): Unit = {
      val optimizer = IncOptimizer.factory(semantics, false)
      val refiner = new Refiner(semantics)
      val factory = IncOptimizer.factory
      val emitter = new Emitter(semantics, outputMode)

      val extraClasses = resourceClasses.scalaJSIR.toList

      val virtualClassFiles: List[VirtualScalaJSIRFile] = cld.collect {
        case x: js.ClassDef =>
          new VirtualScalaJSIRFile {
            val infoAndTree = (Infos.generateClassInfo(x), x)

            def path: String = "out.sjsir"
            def exists: Boolean = false
          }
        case x => sys.error("unknown compiled tree " + x)
      }

      val fullClassPath = extraClasses ::: virtualClassFiles

      val builder = new JSTreeBuilder {
        var trees: List[jsc.Tree] = Nil

        def addJSTree(tree: jsc.Tree): Unit = synchronized(trees :+= tree)
      }

      val linker = new Linker(semantics, considerPositions = false)
      val linkResult = linker.link(fullClassPath, logger,
        reachOptimizerSymbols = true,
        bypassLinkingErrors = true,
        noWarnMissing = Nil,
        checkInfos = true
      )

      val checker = new IRChecker(linkResult, logger)
      if (!checker.check())
        sys.error(s"There were ${checker.errorCount} IR checking errors.")

      val rawOptimized = optimizer.update(linkResult, logger)
      val finalResult = refiner.refine(rawOptimized, logger)
      emitter.emit(finalResult, builder, logger)

      lastAST = builder.trees
    }
  }

  def showTree(tree: global.Tree) =
    global.showRaw(tree, printTypes = true, printIds = true, printKinds = true, printMirrors = true)

  implicit def expr2ast(expr: global.Expr[Any]): JSAST = expr.tree

  implicit def ast2ast(classTree: global.Tree): JSAST = {
    import global._

    val stats = classTree match {
      case Block(stats, _) => stats
      case x => sys.error("Unknown reify results " + x)
    }

    val tree = global.PackageDef(Ident(rootMirror.EmptyPackage), stats)

    val unit = new CompilationUnit(NoSourceFile) {
      override lazy val isJava = false
      override def exists = false
      override def toString() = "ast2ast-unit"

      body = tree
    }

    withRun(global) { r =>
      global.phase = r.parserPhase
      r.compileUnits(List(unit), r.namerPhase)
      global.phase = r.parserPhase
    }

    new JSAST(lastAST)
  }

  implicit def string2ast(str: String): JSAST = stringAST(str)

  def stringAST(code: String): JSAST = stringAST(defaultGlobal)(code)
  def stringAST(global: Global)(code: String): JSAST = {
    compileString(global)(commonHeader + code)
    new JSAST(lastAST)
  }

  def trap(f: => Unit): Unit = {
    try f catch {
      case t: Throwable =>
        t.printStackTrace()
        throw t
    }
  }
}
