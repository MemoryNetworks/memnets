package memnets.fx

import memnets.model.Logging
import scalafx.beans.property.DoubleProperty
import scalafx.scene.Node

object FullSceneFX extends Logging {
  private[memnets] var _handler: FullSceneFX => Unit = fx => {
    logger.warn("no handler for fullScreenFX event!!!")
  }
  private[memnets] def fireEvent(fullFX: FullSceneFX) = _handler(fullFX)
}

trait FullSceneFX extends TickableFX {
  def fullSceneNode: Option[Node] = node
  def headerOpacity: Option[Double] = None
  def minusHeader: Boolean = true
  def toggleFullScene() = FullSceneFX.fireEvent(this)
  def fullWidth: DoubleProperty
  def fullHeight: DoubleProperty
  def unbind(): Unit = {
    fullWidth.unbind()
    fullHeight.unbind()
  }
  def relocate(x: scala.Double, y: scala.Double): scala.Unit
}
