package memnets.lwjgl

import memnets.model.Element
import memnets.ui.TickableUI

trait TickableGL extends TickableUI[Unit] {
  def element: Element = null
  def element_=(d: Element): Unit = {}
  def node: Option[Unit] = None
}
