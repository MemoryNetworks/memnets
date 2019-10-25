package memnets.fx

import com.typesafe.scalalogging.StrictLogging
import javafx.scene.input.ContextMenuEvent
import javafx.scene.input.KeyCode._
import memnets.core.{GameButton, GameButtons}
import scalafx.geometry.Pos
import scalafx.scene.control.ToggleButton
import scalafx.scene.input.KeyEvent
import scalafx.scene.layout._

import scala.collection.mutable.ArrayBuffer

class GameButtonFXAdapter(btn: GameButton) {
  import memnets.fx.utils.BeanUtils._
  val visible = bool(btn, "visible")
  val disabled = bool(btn, "disabled")
  val pressed = bool(btn, "pressed")
  val text = str(btn, "text")

}
class GameButtonsFX(val gameButtons: GameButtons) extends GameControlFX with StrictLogging {
  type TB = javafx.scene.control.ToggleButton
  val ui = new AnchorPane("gamebuttons.fxml".loadFXML)
  def pos = Pos.CenterRight

  ui.userData = gameButtons
  val fxNode = ui
//  val gridPane = new GridPane(ui.findById("gridPane"))
  val gridPane = new VBox(ui.findById[JVBox]("gameVbox"))
  val button1 = new ToggleButton(ui.findById[TB]("button1"))
  val button2 = new ToggleButton(ui.findById[TB]("button2"))
  val button3 = new ToggleButton(ui.findById[TB]("button3"))

  val buttons = Array(button1, button2, button3)
  // NOTE: critical to save off hard ref here, o.w. weak ref will lose fx binding below
  val adapters = ArrayBuffer[GameButtonFXAdapter]()
  for ((fx, i) <- buttons.zipWithIndex) {
    val b = gameButtons(i)
    if (b.visible) {
      fx.delegate.addEventFilter(ContextMenuEvent.CONTEXT_MENU_REQUESTED, (e: ContextMenuEvent) => e.consume())
      fx.onMousePressed = { e =>
        if (!e.isSynthesized) b.pressed = true
      }
      val adapter = new GameButtonFXAdapter(b)
      adapters += adapter
      fx.text <== adapter.text
      fx.visible <== adapter.visible
      fx.disable <== adapter.disabled
      fx.selected <==> adapter.pressed
    } else
      gridPane.children.remove(fx)
  }
  ui.layout()
  // NOTE : had issue w/ no size on dynamic layout version so using static buttons to force size
  // should re-implement much cleaner
  def tick: Unit = {}
  protected def triggerButton(index: Int, ke: KeyEvent): Unit = {
    import javafx.scene.input.KeyEvent._
    if (!gameButtons(index).disabled)
      gameButtons(index).pressed = ke.delegate.getEventType == KEY_PRESSED
  }
  def keyHandler(ke: KeyEvent): Unit = {
    val code = ke.delegate.getCode
    code match {
      case SPACE | NUMPAD1 | DIGIT1 =>
        triggerButton(0, ke)
      case NUMPAD2 | DIGIT2 =>
        triggerButton(1, ke)
      case NUMPAD3 | DIGIT3 =>
        triggerButton(2, ke)
      case default =>
    }
  }
}
