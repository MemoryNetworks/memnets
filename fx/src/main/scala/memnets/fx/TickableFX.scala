package memnets.fx

import memnets.model.Element
import memnets.ui.TickableUI
import scalafx.animation.Transition
import scalafx.scene.Node

trait TickableFX extends TickableUI[Node] {
  def addAnim: Option[Transition] = None
  def delAnim: Option[Transition] = None
  def node: Option[Node]
  def mouseNode: Option[Node] = node
  def element: Element = {
    if (node.isDefined)
      node.get.userData.asInstanceOf[Element]
    else
      null
  }
  def element_=(r: Element): Unit = {
    if (node.isDefined)
      node.get.userData = r
    if (mouseNode.isDefined)
      mouseNode.get.userData = r
  }
}
