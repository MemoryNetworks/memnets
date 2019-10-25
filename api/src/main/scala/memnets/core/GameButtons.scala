package memnets.core

class GameButtons extends GameControl {
  val buttons: Array[GameButton] = Array.tabulate(3) { i =>
    GameButton(id = i + 1, "ABCD".charAt(i).toString)
  }
  def reset(): Unit = {
    for (b <- buttons) {
      b.disabled = false
      b.pressed = false
    }
  }
  def apply(i: Int): GameButton = buttons(i)
}
object GameButtons {
  // max = 4
  def apply(numButtons: Int = 3)(implicit model: Model): GameButtons = {
    val gb = new GameButtons()
    for ((b, i) <- gb.buttons.zipWithIndex)
      b.visible = i < numButtons
    model.controls += gb
    gb
  }
}
