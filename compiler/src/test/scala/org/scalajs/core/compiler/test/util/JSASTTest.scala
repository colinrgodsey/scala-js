package org.scalajs.core.compiler.test.util

import language.implicitConversions

import scala.tools.nsc._
import scala.reflect.internal.util.SourceFile

import scala.util.control.ControlThrowable

import org.junit.Assert._

import org.scalajs.core.compiler.{ScalaJSPlugin, JSTreeExtractors}
import JSTreeExtractors.jse
import org.scalajs.core.ir
import ir.{Trees => js}

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

    def prettyPrint(value: Any, nsp: Int = 0): String = {
      val sp = (for(_ <- 0 until nsp) yield " ").mkString

      value match {
        case js.Block(trees) =>
          val subs = trees.map(prettyPrint(_, nsp + 1))
          val finalSubs = subs.mkString(",\n")
          sp + "Block(" + finalSubs + ")"
        case product: Seq[_] if product.length < 2 =>
          val finalSubs = product.map(prettyPrint(_, nsp + 1)).mkString
          sp + "Seq(" + finalSubs + ")"
        case product: Seq[_] =>
          val subs = product.map(prettyPrint(_, nsp + 1))
          val finalSubs = subs.mkString(",\n")
          sp + "Seq(\n" + finalSubs + ")"
        case product: Product =>
          val subs = product.productIterator.map(prettyPrint(_, nsp + 1))
          val finalSubs = subs.mkString(",\n")
          sp + product.productPrefix + "(" + finalSubs + ")"
        case x => sp + s"'$x'"
      }
    }

    def show: this.type = {
      //clDefs foreach println _
      clDefs.foreach(x => println(prettyPrint(x)))
      this
    }

  }

  /*
  Infos.generateClassInfo -> ClassDefs
  val linkingUnit = Linker.link
  val treebuilder = ...
  replicate ScalaJSOptimizer#optimizeIR
  emit to treebuilder
  get tree
   */

  implicit def string2ast(str: String): JSAST = stringAST(str)

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
