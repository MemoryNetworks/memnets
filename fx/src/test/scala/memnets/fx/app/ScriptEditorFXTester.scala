package memnets.fx.app

import javafx.concurrent.Task
import javax.script.ScriptException
import memnets.core._
import memnets.fx.utils._
import memnets.model._
import org.fxmisc.richtext.Caret.CaretVisibility
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene

object ScriptEditorFXTester extends JFXApp with EditorOwnerBase with ScriptEditorOwner with Logging {

  val testCodeScala =
    """
      |/**
      | * multi-linecomment
      | *
      | */
      |
      |val frequency = Param("frequency", max = 2.0, init = 0.5, precision = 1e-4)
      |frequency.desc = s"Units in cycles/sec. A higher freq gives more cycles. Typically shown by ${GREEK.OMEGA}"
      |frequency.func = d => {
      |  val f = d.toFreq()
      |  - f * f
      |}
      |val damping = Param("damping", max = -0.2, init = -0.01, precision = 1e-4)
      |damping.desc = "The viscous damping coefficient. Typically shown by c.  Using negative c values"
      |damping.func = _ * system.tau/DynamicSystem.TAU_DEFAULT
      |
      |val osc = Osc(frequency, damping)
      |
      |// can specify what the time chart shows on top or let the system guess
      |track(osc)
      |
      |
      |system.onTick = { te =>
      |  if (te.modSec(2))
      |    logr.debug(f"y = ${osc.y.act}%.02f at ${te.secs} sec")
      |}
      |
      |// useful for debugging or testing results at the end
      |validator = { te =>
      |  if (te.modSec(8))
      |    logr.debug(f"validator: y = ${osc.y.act}%.02f at ${te.secs} sec")
      |}
      |
      |skin = Skin { x =>
      | x.backImageOn = false
      |}
      |
      |Trial(name = "IC by Step", time = 1 m)
      |Step(y = osc, on = 0, scale = 10.0)
      |
      |Trial(name = "IC by Osc.ics", time = 1 m)
      |osc.ics(phase = Math.PI, scale = 10.0)
      |YGoal(osc.y, 10.0)
      |
      |Trial(name = "IC by Osc.init", time = 1 m).onReset = {
      |  osc.init(phase = Math.PI/4.0, scale = 10.0)
      |}
      |
      |// just here for SAM testing
      |system.sparse.onSpike = t => {
      |
      |}
      |
      |val swta = new SoftWTA(n = 5)
      |swta.inhib.ui.color = Color.HOTPINK
      |swta.layout()
      |
      |val i = 102
      |val d = 1.2345
      |val d2 = 1.2345f
      |
      |// nothing guy just to test
      |system.elements += new ElementBase with TickableFX {
      |  def node: Option[Node] = None
      |  def tick(te: Tick): Unit = {
      |    if (te.t % 60 == 0) {
      |      logr.debug("test")
      |    }
      |    val gc = RenderContextFX.gc
      |    gc.globalAlpha = 0.7
      |  }
      |}
    """.stripMargin

  val config = ModelConfig()
  var model: BuiltModel = _
  object taskSupport extends JTaskSupport {
    def multiTaskError(): Unit = logger.warn("multiTaskError")
    override protected def startTaskHelper[T](task: Task[T]): Unit = {
      val oldSucceeded = task.getOnSucceeded
      task.onSucceeded = e => {
        logger.debug("onSucceeded")
        _currentTask.value = None
        if (null != oldSucceeded) {
          logger.debug("calling orig onSucceeded")
          oldSucceeded.handle(e)
        }
      }
    }
  }

  val editor = new ScriptEditorFX(this)
  editor.text = testCodeScala
  editor.bindActions()
  stage = new JFXApp.PrimaryStage {
    scene = new Scene(800, 600) {
      content = editor.fx
    }
  }
  stage.title <== editor.title
  stage.onShowing = { e =>
    println("on showing")
    editor.codeArea.showCaretProperty().setValue(CaretVisibility.ON)
    editor.codeArea.moveCaretTo(9, 10)
  }

  def showMsg(txt: String): Unit = {
    logger.debug(s"msg = $txt")
  }
  def openUrl(url: String): Unit = {
    hostServices.showDocument(url)
    showMsg(s"Default browser opening: $url")
  }
  def fxUsesModules: Boolean = true
  def setBuilder(b: BldType, resetCfg: Boolean): Unit = {
    try {
      logger.debug(s"setBuilder: $b, reset $resetCfg")
      model = b.build(config)
      logger.debug("BUILD SUCCESS!!!!")
    } catch {
      case se: ScriptException =>
        editor.scriptError(se)
      case th: Throwable =>
        logger.debug("error", th)
    }
  }
  def preScriptBuild(): Unit = {}
  def showScriptMsg(msg: String): Unit = {
    logger.error(s"SCRIPT ERROR: $msg")
  }
}
