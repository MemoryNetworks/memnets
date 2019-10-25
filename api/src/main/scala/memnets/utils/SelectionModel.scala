package memnets.utils

import java.beans.PropertyChangeListener

import scala.collection.mutable.ArrayBuffer

trait SelectionListener[T <: AnyRef] {
  def selected(item: T): Unit
}
class SelectionModel[T <: AnyRef](items: Iterable[T] = List()) extends BeanSupport with DirtySupport {
  def this(items: T*) {
    this(items.toIterable)
  }
  private val _items = ArrayBuffer[T]()
  private var _selected: T = _
  def selected: T = _selected
  def selected_=(value: T): Unit = {
    val oldValue = this._selected
    if (oldValue != value) {
      this._selected = value
      this._pcs.firePropertyChange("selected", oldValue, value)
    }
  }
  def getSelected = _selected
  def setSelected(value: T): Unit = { selected = value }
  def select(i: Int): Unit = {
    if (i >= 0 && i < getItemCount)
      selected = _items(i)
  }
  def getSelectedItem = selected
  def getSelectedIndex = _items.indexOf(_selected)
  def getItemCount = _items.length
  def getItems: scala.collection.IndexedSeq[T] = _items
  def appendItems(items: T*): Unit = { appendItems(items.toIterable) }
  def appendItems(items: Iterable[T]): Unit = { modifyItems(items, append = true) }
  def setItems(items: T*): Unit = setItems(items.toIterable)
  def setItems(items: Iterable[T]): Unit = { modifyItems(items) }
  def selectedIndex = _items.indexOf(_selected)
  def selectPrevious(): Unit = {
    val prevIndex = selectedIndex - 1
    if (prevIndex >= 0)
      setSelected(_items(prevIndex))
  }
  def selectNext(): Unit = {
    val sel = selectedIndex
    if (sel >= 0) {
      val nextIndex = selectedIndex + 1
      if (nextIndex < _items.length)
        setSelected(_items(nextIndex))
    }
  }
  def selectFirst(): Unit = {
    if (_items.length > 0)
      setSelected(_items(0))
  }
  def addSelectionListener(listener: SelectionListener[T]): Unit = {
    onSelection(listener.selected)
  }
  def onSelection(listener: T => Unit): Subscribed = {
    listener(selected) // init
    onSelectionChange((_, t) => listener(t))
  }
  def onSelectionChange(listener: (T, T) => Unit): Subscribed = {
    val pe: PropertyChangeListener =
      pe => listener(pe.getOldValue.asInstanceOf[T], pe.getNewValue.asInstanceOf[T])
    addPropertyChangeListener("selected", pe)
    () =>
      removePropertyChangeListener("selected", pe)
  }

  protected def modifyItems(items: Iterable[T], append: Boolean = false, fireEvent: Boolean = true): Unit = {
    setSelected(null.asInstanceOf[T])
    if (!append)
      _items.clear()
    _items.appendAll(items)
    if (fireEvent) {
      dirty = true
      dirty = false
    }
  }

  def removeItem(item: T): Unit = {
    val i = _items.indexOf(item)
    if (i > 0) {
      _items.remove(i)
//      if (selected == item)
//        select(0)
      dirty = true
      dirty = false
    }
  }
  modifyItems(items, fireEvent = false)
}
