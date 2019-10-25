package memnets.fx.fx3d

import memnets.model.Element
import memnets.ui.TickableUI
import scalafx.scene.Node

trait Tickable3DFX extends TickableUI[Node] {
  def element: Element = {
    if (node.isDefined)
      node.get.userData.asInstanceOf[Element]
    else
      null
  }
  def element_=(r: Element): Unit = {
    if (node.isDefined)
      node.get.userData = r
  }
}
