package org.scalajs.core.compiler.test.util

import java.io.PrintWriter

import org.scalajs.core.ir.Printers.IRTreePrinter

import language.implicitConversions

import scala.tools.nsc._
import scala.reflect.internal.util.{NoSourceFile, SourceFile}

import scala.util.control.ControlThrowable

import org.junit.Assert._

import org.scalajs.core.compiler.{ScalaJSPlugin}
import org.scalajs.core.ir
import org.scalajs.core.ir.{Trees => js}

abstract class JSASTTest extends DirectTest {

  private var lastAST: JSAST = _

  class JSAST(val clDefs: List[js.Tree]) {
    type Pat = PartialFunction[js.Tree, Unit]

    class PFTraverser(pf: Pat, justOne: Boolean = true) extends ir.Traversers.Traverser {
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

      override def traverse(tree: js.Tree): Unit = {
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
      val printer = new IRTreePrinter(new PrintWriter(System.out))
      clDefs foreach printer.printTree
      this
    }

  }

  implicit def string2ast(str: String): JSAST = stringAST(str)
  implicit def expr2ast(expr: global.Expr[Any]): JSAST = expr.tree

  implicit def ast2ast(classTree: global.Tree): JSAST = {
    import global._

    val stats = classTree match {
      case Block(stats, _) => stats
      case x => sys.error("Unknown reify results " + x)
    }

    val tree = PackageDef(Ident(rootMirror.EmptyPackage), stats)

    val unit = new CompilationUnit(NoSourceFile) {
      override lazy val isJava = false
      override def exists = false
      override def toString() = "ast2ast-unit"

      body = tree
    }

    withRun(global) { r =>
      r.compileUnits(List(unit), r.namerPhase)
    }

    lastAST
  }

  override def newScalaJSPlugin(global: Global) = new ScalaJSPlugin(global) {
    override def generatedJSAST(cld: List[js.Tree]): Unit = {
      lastAST = new JSAST(cld)
    }
  }

  def stringAST(code: String): JSAST = stringAST(defaultGlobal)(code)
  def stringAST(global: Global)(code: String): JSAST = {
    compileString(global)(code)
    lastAST
  }

  def sourceAST(source: SourceFile): JSAST = sourceAST(defaultGlobal)(source)
  def sourceAST(global: Global)(source: SourceFile): JSAST = {
    compileSources(global)(source)
    lastAST
  }

}
