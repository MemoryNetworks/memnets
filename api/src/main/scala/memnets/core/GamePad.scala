package memnets.core

import memnets.utils.BooleanListener

import scala.beans.BeanProperty

/** GamePad are added to core.Model, so not in memnets.ui */
abstract class GamePadBase extends GameElementBase with GameControl {
  private var _active = false
  var keyAdj: Option[Double] = None

  def active: Boolean = _active
  def active_=(value: Boolean): Unit = {
    val oldValue = this._active
    if (oldValue != value) {
      this._active = value
      this._pcs.firePropertyChange("active", oldValue, value)
    }
  }
  def onActive(bl: BooleanListener): Unit = {
    addBooleanListener("active")(bl)
  }

  def reset(): Unit = {
    active = false
    disabled = false
    visible = true
  }
  // Java
  def getActive: Boolean = active
  def setActive(value: Boolean): Unit = active = value
}

object AnglePad {
  def apply()(implicit model: Model): AnglePad = {
    val gamePad = new AnglePad()
    model.controls += gamePad
    gamePad
  }
}
object DPad {
  def apply()(implicit model: Model): DPad = {
    val gamePad = new DPad()
    model.controls += gamePad
    gamePad
  }
}
object DPadNoDown {
  def apply()(implicit model: Model): DPadNoDown = {
    val gamePad = new DPadNoDown()
    model.controls += gamePad
    gamePad
  }
}
class AnglePad private[memnets] extends GamePadBase {
  private var _angle = 0.0
  private var _mag = 0.0
  private var _magScale = 1.0
  private var _releaseCount = 0

  def releaseCount = _releaseCount
  def releaseCount_=(value: Int): Unit = {
    val oldValue = this._releaseCount
    if (oldValue != value) {
      this._releaseCount = value
      this._pcs.firePropertyChange("releaseCount", oldValue, value)
    }
  }
  def angle: Double = _angle
  def angle_=(value: Double): Unit = {
    val oldValue = this._angle
    if (oldValue != value) {
      this._angle = value
      this._pcs.firePropertyChange("angle", oldValue, value)
    }
  }
  def mag: Double = _mag
  def mag_=(value: Double): Unit = {
    val oldValue = this._mag
    if (oldValue != value) {
      this._mag = value
      this._pcs.firePropertyChange("mag", oldValue, value)
    }
  }

  /** does not change mag, but if gamepad wants to display value, should use this scale  */
  def magScale: Double = _magScale
  def magScale_=(value: Double): Unit = {
    val oldValue = this._magScale
    if (oldValue != value) {
      this._magScale = value
      this._pcs.firePropertyChange("magScale", oldValue, value)
    }
  }
  override def reset(): Unit = {
    super.reset()
    releaseCount = 0
  }
  addBooleanListener("active") { rel =>
    if (!rel) releaseCount = releaseCount + 1
  }

  // Java
  def getAngle = angle
  def setAngle(value: Double) = angle = value
  def getMag = mag
  def setMag(value: Double) = mag = value
  def getMagScale = magScale
  def setMagScale(value: Double) = magScale = value
  def getReleaseCount = releaseCount
  def setReleaseCount(value: Int) = releaseCount = value

}
class DPadNoDown private[memnets] extends GamePadBase {
  @BeanProperty var right: Double = 0.0
  @BeanProperty var left: Double = 0.0
  @BeanProperty var up: Double = 0.0
}

class DPad private[memnets] extends GamePadBase {
  @BeanProperty var right: Double = 0.0
  @BeanProperty var left: Double = 0.0
  @BeanProperty var up: Double = 0.0
  @BeanProperty var down: Double = 0.0
}
