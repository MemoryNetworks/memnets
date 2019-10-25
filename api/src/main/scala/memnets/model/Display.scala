package memnets.model

import java.beans.PropertyChangeListener

import memnets.utils._

/**
 * resolution for the canvas+ui.  right now, set at app startup.
 * 720p canvas scaled up still looks good if app running at 1080p.
 * not ideal, but embedded HD graphics start to feel 1080 much more...
 */
trait Resolution {
  def width: Int
  def height: Int
  def getWidth: Int = width
  def getHeight: Int = height
  def toTuple: (Double, Double) = (width, height)

  /** easy access in Java for objects */
  def getResolution: Resolution = this
  override def toString: String = { s"Resolution[w= $width, h= $height]" }
}
object HD720 extends Resolution {
  val width = 1280
  val height = 720
}
object HD1080 extends Resolution {
  val width = 1920
  val height = 1080
}
object HD900 extends Resolution {
  val width = 1600
  val height = 900
}
object Display extends BeanSupport with Logging {
  private var _resolution: Resolution = HD720
  def width: Int = resolution.width
  def height: Int = resolution.height
  def getWidth: Int = width
  def getHeight: Int = height
  // these should be set by app at startup
  def resolution: Resolution = _resolution
  def resolution_=(value: Resolution): Unit = {
    if (value != null) {
      val oldValue = this._resolution
      this._resolution = value
      this._pcs.firePropertyChange("resolution", oldValue, value)
    }
  }
  def onChange(listener: Resolution => Unit): Subscribed = {
    listener(resolution) // init call
    val pe: PropertyChangeListener = pe => listener(pe.getNewValue.asInstanceOf[Resolution])
    addPropertyChangeListener("resolution", pe)
    () =>
      removePropertyChangeListener("resolution", pe)
  }
  def getResolution: Resolution = resolution
  def setResolution(res: Resolution): Unit = { resolution = res }
}
