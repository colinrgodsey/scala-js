package org.scalajs.core.compiler.test

import util._

import org.junit.Test

import org.scalajs.core.ir.{Trees => jst, Types => jstpe}

import scala.scalajs.js

class OptimizationTest extends JSASTTest {
  import global.reify

  @Test
  def unwrapScalaFunWrapper: Unit = {

    // Make sure we do not wrap and unwrap right away
    reify {
      class A {
        val jsFun: js.Function = (x: Int) => x * 2
      }
    }.
    hasNot("runtime.AnonFunction ctor") {
      case jst.New(jstpe.ClassType("sjsr_AnonFunction1"), _, _) =>
    }

    // Make sure our wrapper matcher has the right name
    reify {
      class A {
        val scalaFun = (x: Int) => x * 2
        val jsFun: js.Function = scalaFun
      }
    }.
    has("runtime.AnonFunction ctor") {
      case jst.New(jstpe.ClassType("sjsr_AnonFunction1"), _, _) =>
    }

    reify {
      object A {
        final val a = 1

        def b = a
      }

      class B {
        val x = A.b + A.a + A.a
      }
    }
    .has(1, "LoadModule") {
      case jst.LoadModule(_) =>
    }
    .hasNot("subsequent LoadModule calls") {
      case jst.Block(jst.LoadModule(_) :: _) =>
    }

    reify {
      object A {
        final val a = 1
      }

      class B {
        val x = A.a + A.a + A.a
      }
    }
    .hasNot("LoadModule") {
      case jst.LoadModule(_) =>
    }

    /* Make sure js.Array(...) is optimized away completely for several kinds
     * of data types.
     */
    """
      import scala.scalajs.js

      class VC(val x: Int) extends AnyVal

      class A {
        val a = js.Array(5, 7, 9, -3)
        val b = js.Array("hello", "world")
        val c = js.Array('a', 'b')
        val d = js.Array(Nil)
        val e = js.Array(new VC(151189))
      }
    """.
    hasNot("any of the wrapArray methods") {
      case jst.Apply(_, jst.Ident(name, _), _)
          if name.startsWith("wrap") && name.endsWith("__scm_WrappedArray") =>
    }

    //we cant use AnyVals with reify.... so we use text still here
    /* Make sure js.Array(...) is optimized away completely for several kinds
     * of data types.
     */
    """
      import scala.scalajs.js

      class VC(val x: Int) extends AnyVal

      class A {
        val a = js.Array(5, 7, 9, -3)
        val b = js.Array("hello", "world")
        val c = js.Array('a', 'b')
        val d = js.Array(Nil)
        val e = js.Array(new VC(151189))
      }
    """.show.
    hasNot("any of the wrapArray methods") {
      case jst.Apply(_, jst.Ident(name, _), _)
        if name.startsWith("wrap") && name.endsWith("__scm_WrappedArray") =>
    }

    /* Make sure varargs are optimized to use js.WrappedArray instead of
     * scm.WrappedArray, for various data types.
     */
    """
      import scala.scalajs.js
      class VC(val x: Int) extends AnyVal

      class A {
        val a = List(5, 7, 9, -3)
        val b = List("hello", "world")
        val c = List('a', 'b')
        val d = List(Nil)
        val e = List(new VC(151189))
      }
    """.
    hasNot("any of the wrapArray methods") {
      case jst.Apply(_, jst.Ident(name, _), _)
          if name.startsWith("wrap") && name.endsWith("__scm_WrappedArray") =>
    }

    // Make sure our wrapper matcher has the right name
    reify {
      class A {
        val a: Seq[Int] = new Array[Int](5)
      }
    }.
    has("one of the wrapArray methods") {
      case jst.Apply(_, jst.Ident(name, _), _)
          if name.startsWith("wrap") && name.endsWith("__scm_WrappedArray") =>
    }

    // Verify the optimized emitted code for 'new js.Object' and 'new js.Array'
    reify {
      class A {
        val o = new js.Object
        val a = new js.Array
      }
    }.
    hasNot("any reference to the global scope") {
      case jst.JSEnvInfo() =>
    }

  }

}
