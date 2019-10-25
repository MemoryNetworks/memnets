package memnets.utils

import java.beans._

import scala.collection.mutable

trait BeanSupport {
  protected val _pcs = new PropertyChangeSupport(this)

  def addPropertyChangeListener(property: String, pl: PropertyChangeListener): Unit = {
    _pcs.addPropertyChangeListener(property, pl)
  }
  def removePropertyChangeListener(property: String, pl: PropertyChangeListener): Unit = {
    _pcs.removePropertyChangeListener(property, pl)
  }

  /** NOTE: unlike some other listeners, this guy doesn't do initial call upon add */
  def addBooleanListener(property: String)(listener: BooleanListener): Subscribed = {
    val pc: PropertyChangeListener = pe => listener.change(pe.getNewValue.asInstanceOf[Boolean])
    _pcs.addPropertyChangeListener(property, pc)
    // NOTE: must give property
    () =>
      _pcs.removePropertyChangeListener(property, pc)
  }

  /** NOTE: unlike some other listeners, this guy doesn't do initial call upon add */
  def addIntListener(property: String)(listener: IntListener): Subscribed = {
    val pc: PropertyChangeListener = pe => listener.change(pe.getNewValue.asInstanceOf[Integer])
    _pcs.addPropertyChangeListener(property, pc)
    // NOTE: must give property
    () =>
      _pcs.removePropertyChangeListener(property, pc)
  }

  def clearPropertyListeners(): Unit = {
    // create copy on purpose
    val copy = mutable.Buffer[PropertyChangeListener]()
    copy ++= _pcs.getPropertyChangeListeners
    for (pl <- copy)
      _pcs.removePropertyChangeListener(pl)

  }
}

trait BooleanListener {
  def change(value: Boolean): Unit
}
trait IntListener {
  def change(value: Int): Unit
}
