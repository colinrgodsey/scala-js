package org.scalajs.core.tools.javascript

import org.junit.Test
import org.scalajs.core.ir.Trees.JSUnaryOp
import org.scalajs.core.tools.javascript.util.JavaScriptASTTest
import org.scalajs.core.tools.javascript.{ Trees => jsc }

import scala.scalajs.js
import js.annotation.JSExport

class OptimizerTest extends JavaScriptASTTest {
  import global.reify

  @Test
  def testTripBang: Unit = trap {
    reify {
      object A {
        val eh = math.random

        @inline def not(a: Boolean) = !a
      }

      object testTripBang extends js.JSApp {

        def main(): Unit = {
          val a = A.not(math.random == 0)
          println(!A.not(A.not(a)))
        }
      }
    }.show
        //EXPECT only 1 $m_LA$()
    .has("testTripBang") {
      case jsc.VarDef(jsc.Ident("$c_LtestTripBang$", None), _) =>
    }
    .has(1, "LoadModule(A$)") { //VarRef(Ident($m_LA$,None))
      case jsc.Apply(jsc.VarRef(jsc.Ident("$m_LA$", _)), _) =>
    }
    .hasNot("!!!") {
      case jsc.UnaryOp(JSUnaryOp.!, jsc.UnaryOp(JSUnaryOp.!, jsc.UnaryOp(JSUnaryOp.!, _))) =>
    }
    .has("!") {
      case jsc.UnaryOp(JSUnaryOp.!, _) =>
    }
  }
/*
  @Test
  def testInlineModuleAccessor: Unit = {
    reify {
      object TestObject {
        def add(x: Int, y: Int) = x + y
      }

      object testInlineModuleAccessor extends js.JSApp {
        val x = TestObject.add(1, 2)
        val y = TestObject.add(x, 5)

        def main(): Unit = {}
      }
    }
    .show
  }*/
}
