package memnets.utils

/** NOTE: Java must use addBooleanListener on "dirty"  */
trait DirtySupport { self: BeanSupport =>
  protected var _dirty = false

  def dirty: Boolean = _dirty
  def dirty_=(value: Boolean): Unit = {
    val oldValue = this._dirty
    this._dirty = value
    this._pcs.firePropertyChange("dirty", oldValue, value)
  }
  def isDirty: Boolean = dirty
  def setDirty(value: Boolean): Unit = { dirty = value }

  def onDirty: Unit = {} // only here for IDE
  /** upon setting, function f will be immediately called regardless if dirty or not */
  def onDirty_=(f: => Any): Subscribed = {
    // initial call
    f
    addBooleanListener("dirty") { sel =>
      if (sel) f
    }
  }
}
