package memnets.fx

import javafx.event.EventType
import scalafx.scene.input.MouseEvent

// do not put in fx3d object b/cols of Drools
sealed case class MouseEventP(me: MouseEvent) {
  me.consume
  val node: JNode = me.source.asInstanceOf[JNode]
  def getUserData: AnyRef = node.getUserData
  def getMouseEvent: JMouseEvent = me.delegate
  def getNode: JNode = node
  def getEventType: EventType[_ <: JMouseEvent] = me.eventType.delegate
  override def toString =
    s"MouseEventP(data= $getUserData, type= ${me.eventType}, clicks= ${me.clickCount}, alt= ${me.altDown})"
}
