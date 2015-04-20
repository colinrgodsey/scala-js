package org.scalajs.core.compiler.test.util

import scala.tools.nsc._
import reporters.{Reporter, ConsoleReporter}
import scala.reflect.internal.util.{ SourceFile, BatchSourceFile }

import org.scalajs.core.compiler.ScalaJSPlugin

import scala.collection.mutable

/** This is heavily inspired by scala's partest suite's DirectTest */
abstract class DirectTest {
  //see notes on defaultGlobal below. Should only be used for reify purposes.
  lazy val global = {
    val x = newScalaJSCompiler()

    withRun(x)(r => x.phase = r.parserPhase)

    x
  }

  /** these arguments are always added to the args passed to newSettings */
  def extraArgs: List[String] = Nil

  /** create settings objects for test from arg string */
  def newSettings(args: List[String]) = {
    val s = new Settings
    s processArguments (args, true)
    s
  }

  def newScalaJSCompiler(args: String*): Global = {
    val settings = newSettings(
        List(
            "-d", testOutputPath,
            "-bootclasspath", scalaLibPath,
            "-classpath", scalaJSLibPath) ++
        extraArgs ++ args.toList)

    lazy val global: Global = new Global(settings, newReporter(settings)) {
      override lazy val plugins = newScalaJSPlugin(global) :: Nil
    }

    global
  }

  def newScalaJSPlugin(global: Global): ScalaJSPlugin =
    new ScalaJSPlugin(global)

  def newReporter(settings: Settings) = new ConsoleReporter(settings)

  def newSources(codes: String*) = codes.toList.zipWithIndex map {
    case (src, idx) => new BatchSourceFile(s"newSource${idx + 1}.scala", src)
  }

  def withRun[T](global: Global)(f: global.Run => T): T = {
    global.reporter.reset()
    val r = new global.Run
    global.phase = r.parserPhase
    val out = f(r)
    //reset for reify
    global.phase = r.parserPhase
    out
  }

  def compileSources(global: Global)(sources: SourceFile*): Boolean = {
    withRun(global)(_ compileSources sources.toList)
    !global.reporter.hasErrors
  }

  def compileString(global: Global)(sourceCode: String): Boolean =
    compileSources(global)(newSources(sourceCode): _*)

  def compileString(sourceCode: String): Boolean =
    compileString(defaultGlobal)(sourceCode)

  // Cannot reuse global, otherwise compiler crashes with Scala >= 2.11.5
  // on following tests:
  // - org.scalajs.core.compiler.test.JSExportTest
  // - org.scalajs.core.compiler.test.JSDynamicLiteralTest
  // Filed as #1443
  def defaultGlobal = newScalaJSCompiler()
  //def defaultGlobal = global

  def testOutputPath = sys.props("scala.scalajs.compiler.test.output")
  def scalaJSLibPath = sys.props("scala.scalajs.compiler.test.scalajslib")
  def scalaLibPath   = sys.props("scala.scalajs.compiler.test.scalalib")

}
