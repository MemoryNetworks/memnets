package memnets.fx.app

import javafx.scene.paint.Color
import memnets.core._
import memnets.fx.fx3d.Scene3DFX
import memnets.model._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.control.Button
import scalafx.scene.layout.BorderPane

object ConfigEditorFXTester extends JFXApp with Logging {
  val editor = new ConfigEditorFX
  val config = new ModelConfig()
  editor.setConfig(config)
  val resetBtn = new Button()
  resetBtn.text = "Reset"
  resetBtn.onAction = { evt =>
    logger.debug("reset called")
    config.reset()
    editor.setConfig(config)
  }
  stage = new JFXApp.PrimaryStage {
    scene = new Scene(320, 600) {
      content = new BorderPane {
        top = resetBtn
        center = editor.fx

      }
    }
  }
  editor.fx.prefWidth <== stage.scene.value.widthProperty
  editor.fx.prefHeight <== stage.scene.value.heightProperty
}

object Scene3DEditorFXTester extends JFXApp with Logging {
  val editor = new Scene3DEditorFX
  val sc3d = new Scene3DFX()
  editor.setScene(Some(sc3d))
  val resetBtn = new Button()
  resetBtn.text = "Reset"
  resetBtn.onAction = { evt =>
    logger.debug("reset called")
    sc3d.ambientLight.color.value = Color.WHITE
    editor.setScene(Some(sc3d))
  }
  stage = new JFXApp.PrimaryStage {
    scene = new Scene(320, 600) {
      content = new BorderPane {
        top = resetBtn
        center = editor.fx

      }
    }
  }
  editor.fx.prefWidth <== stage.scene.value.widthProperty
  editor.fx.prefHeight <== stage.scene.value.heightProperty
}
