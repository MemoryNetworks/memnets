package memnets.core

import memnets.utils.{BeanSupport, BooleanListener}

abstract class GameElementBase extends BeanSupport {
  private var _disabled = false
  private var _visible = true

  def disabled: Boolean = _disabled
  def disabled_=(value: Boolean): Unit = {
    val oldValue = this._disabled
    if (oldValue != value) {
      this._disabled = value
      this._pcs.firePropertyChange("disabled", oldValue, value)
    }
  }
  def visible: Boolean = _visible
  def visible_=(value: Boolean): Unit = {
    val oldValue = this._visible
    if (oldValue != value) {
      this._visible = value
      this._pcs.firePropertyChange("visible", oldValue, value)
    }
  }

  def onVisible(bl: BooleanListener): Unit = {
    addBooleanListener("visible")(bl)
  }
  def onDisabled(bl: BooleanListener): Unit = {
    addBooleanListener("disabled")(bl)
  }

  // Java
  def getDisabled: Boolean = disabled
  def setDisabled(value: Boolean): Unit = disabled = value
  def getVisible: Boolean = visible
  def setVisible(value: Boolean): Unit = visible = value
}
