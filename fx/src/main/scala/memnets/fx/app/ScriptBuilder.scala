package memnets.fx.app

import javax.script._
import memnets.core._
import memnets.model._

/**
 * not putting ScriptBuilder in api module b/c don't want dependencies on scala compiler
 */
object ScriptBuilder {

  /** NOTE : not assigning to a val b/c that would increase the line number returned by exception */
  val HEADER: String =
    """
      |new memnets.core.ScriptEval(bldr.asInstanceOf[memnets.core.Model]) {
      |import model._
      |import memnets.core._
      |import memnets.model._
      |import memnets.model.GREEK._
      |import memnets.ml._
      |import memnets.ui._
      |import memnets.utils._
      |import Activation._
      |import Config._
      |import Y._
      |import breeze.linalg._
      |import breeze.numerics._
      |import scala.collection.mutable.{ArrayBuffer, ListBuffer}
      |import scala.util.Random
      |import memnets.models._
      |import memnets.models.biology._
      |import memnets.models.chaos._
      |import memnets.models.control._
      |import memnets.models.neuro._
      |import memnets.models.neuro.dft._
      |import memnets.models.neuro.spiking._
      |import memnets.models.neuro.swta._
      |import memnets.models.neuro.swta.fsm._
      |import memnets.awt._
      | """.stripMargin

}
class ScriptBuilder extends Logging {
  private val engine = new ScriptEngineManager().getEngineByName("scala")

  def header: String = ScriptBuilder.HEADER
  def apply(script: String, name: String = "script") = {
    ModelBuilder(name) { model =>
      val effScript = header + script.stripMargin + "}"
      val bnds = engine.createBindings()
      bnds.put("bldr", model)
      engine.eval(effScript, bnds)
    }
  }

  def scriptErrorToPos(se: ScriptException): (Int, Int) = {
    val line = se.getLineNumber
    val lineAdj = line - headerOffSet
    val col = se.getColumnNumber - 1
    (lineAdj, col)
  }

  /**
   * magic # that depends on imports but is not just header line count???
   * subclasses must override w/ correct # if use in editor
   */
  protected def headerOffSet: Int = 42
}

/**
 * JavaFX 11+ modules don't play well w/ embedded classpath.
 * have to re-run script several times to "get past" error about missing fx runtime
 * ...but, it will still build model after some bogus fx error messages
 */
class ScriptBuilderWithFX extends ScriptBuilder {
  val fxheader =
    """
      |import memnets.fx._
      |import memnets.fx.fx3d._
      |import memnets.fx.games._
      |import memnets.fx.games.physics._
      |import memnets.fx.games.wta._
      |import scalafx.scene.paint._
      |import scalafx.scene.effect._
      |import scalafx.scene.shape._
      |import scalafx.animation._
      |import scalafx.scene._
      |import scalafx.scene.transform._
      |import scalafx.geometry._
      |import scalafx.Includes._
      |import scala.language.postfixOps
      |import scala.language.dynamics
      |import scala.language.implicitConversions
      |""".stripMargin

  val fxHeaderLineCount = fxheader.linesIterator.size
  override def header: String = super.header + fxheader
  override def headerOffSet: Int = {
    val scalaVersion = util.Properties.versionString
    // magic # time...
    val off213 = if (scalaVersion.startsWith("version 2.13")) 15 else 0
    val off = super.headerOffSet + fxHeaderLineCount - off213
    logger.debug("off = " + off)
    off
  }
}

/*
  import scala.tools.nsc.interpreter.Results
  import tools.nsc.interpreter.{IMain, Results}
  import sys.error
  val interpret = engine.asInstanceOf[tools.nsc.interpreter.IMain]
  val settings = interpret.settings
  settings.embeddedDefaults[DynamicSystem]
  // todo : -optimize??? plus register error callback, don't i need experimental now???
  settings.processArguments(List("-feature","-language:postfixOps"), true)
  settings.usejavacp.value = true
  def compilerError(msg : String) : Unit = {
    println("compile error_kj = "+msg)
    logger.warn(msg)
  }
  settings.withErrorFn(compilerError)  // todo: not working...

  def eval(expression: String)  =
    //interpret.bind("bldr", b)
    interpret.interpret( expression ) match {
    case Results.Error => error( "Failed" )
    case Results.Incomplete => error( "Incomplete" )
    case Results.Success => interpret.valueOfTerm( interpret.mostRecentVar )
      .getOrElse( error( "No result" ))
  }
 */
