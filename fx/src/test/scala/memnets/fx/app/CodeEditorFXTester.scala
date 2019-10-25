package memnets.fx.app

import memnets.core.ModelConfig
import memnets.models.DslExamples
import org.fxmisc.richtext.Caret.CaretVisibility
import scalafx.application.JFXApp
import scalafx.scene.Scene

object CodeEditorFXTester extends JFXApp with EditorOwnerBase {
  val testCodeScala =
    """
      |import memnets.net._
      |
      |/**
      | * multi-linecomment
      | *
      | */
      |
      |val swta = new SoftWTA(n = 5)
      |swta.inhib.ui.color = Color.HOTPINK
      |swta.layout()
      |
      |val y = Y()
      |val y2 = Y()
      |y --> y2 w = 0.2
      |
      |val i = 102
      |val d = 1.2345
      |val d2 = 1.2345f
      |      |
      |// just here for SAM testing
      |model.sparse.onSpike = t => {
      |}
      |
      |
      |
      |trait TestTickableFX  {
      |  def addAnim : Option[Transition] = None
      |  def delAnim : Option[Transition] = None
      |  def fxNode : Option[Node]
      |  def data : AnyRef = if (fxNode.isDefined) fxNode.get.userData else null
      |  def init : Unit = { reset } // called when first added and if changes
      |  def permanent : Boolean = true
      |  def reset : Unit = {}
      |  def tick(te: Tick, rc : RenderContext)
      |  private[memnets] var sub : Option[Subscription] = None
      |}
      |skin = Skin { x =>
      | x.backImageOn = false
      |}
      |Trial(name = "equal inputs")
      |YGoal(tgtswta(0))
      |Step(y=swta(0), on=400, dur=2000, act=5.0)
      |Step(y=swta(1), on=400, dur=2000, act=5.0)
    """.stripMargin
  val editor = new CodeEditorFX(this, DslEditorDef)
  editor.text = testCodeScala
  editor.bindActions()
  stage = new JFXApp.PrimaryStage {
    scene = new Scene(800, 600) {
      import scalafx.Includes._
      content = editor.fx
    }
  }
  stage.title <== editor.title
  stage.onShowing = { e =>
    println("on showing")
    editor.codeArea.showCaretProperty().setValue(CaretVisibility.ON)
    editor.codeArea.moveCaretTo(9, 10)
  }

  def openUrl(url: String): Unit = {
    hostServices.showDocument(url)
    showMsg(s"Default browser opening: $url")
  }
  def showMsg(txt: String): Unit = {
    println(s"msg = $txt")
  }
  lazy val model = DslExamples.dslOsc.build(ModelConfig())
}
