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
      object testTripBang extends js.JSApp {
        @JSExport def a(x: Boolean) = !(!(!(x)))

        def main() {}
      }
    }
    .hasNot("!!!") {
      case jsc.UnaryOp(JSUnaryOp.!, jsc.UnaryOp(JSUnaryOp.!, jsc.UnaryOp(JSUnaryOp.!, _))) =>
    }
    .has("!") {
      case jsc.UnaryOp(JSUnaryOp.!, _) =>
    }.show
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
