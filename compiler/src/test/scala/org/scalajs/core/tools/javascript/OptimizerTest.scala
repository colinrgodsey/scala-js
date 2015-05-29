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
          println(A.eh)
        }
      }
    }
    .has("testTripBang") {
      case jsc.VarDef(jsc.Ident("$c_LtestTripBang$", None), _) =>
    }
    .has(1, "LoadModule(A$)") {
      case jsc.Apply(jsc.VarRef(jsc.Ident("$m_LA$", _)), _) =>
    }
    .hasNot("!!!") {
      case jsc.UnaryOp(JSUnaryOp.!, jsc.UnaryOp(JSUnaryOp.!, jsc.UnaryOp(JSUnaryOp.!, _))) =>
    }
    .has("!") {
      case jsc.UnaryOp(JSUnaryOp.!, _) =>
    }
  }
}
