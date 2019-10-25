package memnets.fx.app

import memnets.fx._
import memnets.model.Logging
import scalafx.application._
import scalafx.scene.Scene

object SkinEditorFXTester extends JFXApp with Logging {
  val editor = new SkinEditorFX
  editor.setSkin(SkinFX())
  stage = new JFXApp.PrimaryStage {
    scene = new Scene(320, 600) {
      content = editor.fx
    }
  }
  editor.fx.prefWidth <== stage.scene.value.widthProperty
  editor.fx.prefHeight <== stage.scene.value.heightProperty
}
