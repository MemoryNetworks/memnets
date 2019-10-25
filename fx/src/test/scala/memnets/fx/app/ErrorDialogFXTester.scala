package memnets.fx.app

import memnets.model.Logging
import scalafx.application._
import scalafx.scene.Scene

object ErrorDialogFXTester extends JFXApp with Logging {
  stage = new JFXApp.PrimaryStage {
    scene = new Scene(300, 300) {}
  }
  val dialog = ErrorDialogFX(new Exception("big ol' nasty exception"), noModels = false)
  dialog.onHidden = { e =>
    Platform.exit()
  }
  val result = dialog.showAndWait()
}
