package memnets.awt

import javax.swing.JComponent
import memnets.model.Element
import memnets.ui._

trait JTickable extends TickableUI[JComponent] {
  def element: Element = if (node.isDefined) node.get.getClientProperty("JTickable").asInstanceOf[Element] else null
  def element_=(d: Element): Unit = {
    if (node.isDefined)
      node.get.putClientProperty("JTickable", d)
  }
}
