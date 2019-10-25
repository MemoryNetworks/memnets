package memnets.ui

import java.util.Optional

import memnets.model._

trait TickableUI[T] extends Tickable {
  def element: Element
  def element_=(r: Element): Unit
  def findTarget(x: Double, y: Double): Option[UserSource] = None
  def findTargetRaw(x: Double, y: Double): Option[UserSource] = {
    if (node.isDefined)
      findTarget(x, y)
    else
      None
  }
  def node: Option[T]
  def permanent: Boolean = true

  // Java
  def getElement: Element = element
  def setElement(d: Element): Unit = { element = d }
  import memnets.utils.JavaUtils._
  def getNode: Optional[T] = node
  def isPermanent: Boolean = permanent
}
