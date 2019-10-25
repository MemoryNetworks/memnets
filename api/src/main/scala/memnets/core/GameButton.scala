package memnets.core

import memnets.utils.BooleanListener

case class GameButton(id: Int, private var _text: String) extends GameElementBase {
  private var _pressed = false

  /**
   * touch has 2 issues w/ long press :
   * 1) long press acts as right click/content menu event even if filter
   * 2) default javafx touch support only allows ONE control to receive touch at one time
   * so button acts as toggle if pressed on touch screen
   */
  def pressed = _pressed
  def pressed_=(value: Boolean): Unit = {
    val oldValue = this._pressed
    this._pressed = value
    this._pcs.firePropertyChange("pressed", oldValue, value)
  }
  def text = _text
  def text_=(value: String): Unit = {
    val oldValue = this._text
    this._text = value
    this._pcs.firePropertyChange("text", oldValue, value)
  }
  def onPressed(bl: BooleanListener): Unit = {
    addBooleanListener("pressed")(bl)
  }

  def getPressed = pressed
  def setPressed(value: Boolean) = pressed = value
  def getText = text
  def setText(value: String) = text = value
}
