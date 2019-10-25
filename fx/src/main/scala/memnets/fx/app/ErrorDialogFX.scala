package memnets.fx.app

import java.io._

import memnets.fx._
import memnets.utils._
import scalafx.scene.control.Alert._
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.stage.StageStyle

object ErrorDialogFX {
  def apply(ex: Throwable, noModels: Boolean) = {
    val msgOpt = if (noModels) "  Hit Close to exit." else "  Hit 'Ok' to try another model or 'Close' to exit."
    val msg = "A possibly fatal error occurred." + msgOpt
    new ErrorDialogFX(msg, ex, showOk = !noModels)
  }
}
class ErrorDialogFX(msg: String, ex: Throwable, showOk: Boolean) extends Alert(AlertType.Error) {
  this.initStyle(StageStyle.Undecorated)
  buttonTypes = if (showOk) List(ButtonType.OK, ButtonType.Close) else List(ButtonType.Close)

  // do css after add buttons
  dialogPane().getStylesheets.add("progress-dialog.css".asURL.toExternalForm)
  dialogPane().setStyle("-fx-graphic: null;")
  dialogPane().darkButtons()
  dialogPane().setPrefWidth(680)
  dialogPane().setMinHeight(120)

  headerText = ""
  contentText = msg
  // Create expandable Exception.
  val sw = new StringWriter
  val pw = new PrintWriter(sw)
  if (ex != null)
    ex.printStackTrace(pw)
  else
    pw.println("no exception found")

  val textArea = new TextArea(sw.toString)
  textArea.setEditable(false)
  textArea.setWrapText(true)
  textArea.setMaxWidth(Double.MaxValue)
  textArea.setMaxHeight(Double.MaxValue)
  GridPane.setVgrow(textArea, Priority.Always)
  GridPane.setHgrow(textArea, Priority.Always)

  dialogPane().setExpandableContent(textArea)
  dialogPane().layout()
}
