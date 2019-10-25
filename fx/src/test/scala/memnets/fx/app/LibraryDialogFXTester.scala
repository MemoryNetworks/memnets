package memnets.fx.app

import memnets.fx.games.FreeGamesFX
import memnets.model.Logging
import memnets.models.StandardLibrary
import scalafx.application._
import scalafx.scene.Scene
import scalafx.scene.shape.Rectangle

object LibraryDialogFXTester extends JFXApp with Logging {
  stage = new JFXApp.PrimaryStage {
    scene = new Scene(300, 300) {
      content = Rectangle(100, 100)
    }
  }
  logger.debug("java version= " + System.getProperty("java.version"))
  val dialog = new LibraryDialogFX(stage, Seq(StandardLibrary, FreeGamesFX))
  dialog.onHidden = { e =>
    Platform.exit()
  }
  val result = dialog.showAndWait()
  result match {
    case Some(LibraryResult(b)) =>
      logger.debug("Builder=" + b)
    case None =>
      logger.debug("Dialog returned: None")
  }
}
