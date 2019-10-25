package memnets.fx

import memnets.model._
import scalafx.scene.Group
import scalafx.scene.shape._
import scalafx.scene.text.Text

class OscSkinFX extends SkinFX {
  class SimpleOscFX(osc: Osc) extends Arc with TickableFX {
    def node = Some(this)
    val loc = osc.x.ui.loc
    centerX = loc.x
    centerY = loc.y
    radiusX = 15.0
    radiusY = 15.0
    fill = yColorMap(osc.x)
    `type` = ArcType.Round
    startAngle = 0.0
    def tick(te: Tick): Unit = {
      length = Math.toDegrees(osc.phase)
    }
  }
  override def createOsc(o: Osc) = new SimpleOscFX(o)
  def createY(y: Y) = None

  object OscFX {
    val maxArcRadius = 40.0
    val minArcRadius = 25.0
    val diffArcRadius = maxArcRadius - minArcRadius
    val oscFXPrec = Precision(0.25)
  }

  // todo : this isn't done...
  class OscFX(osc: Osc) extends TickableFX with Logging {
    val fx = new Group("osc.fxml".loadFXML[JGroup])
    val node = Some(fx)
    fx.managed = false
    var viz = Viz.Default
    val arc = new Arc(fx.findById("dial"))
    val text = new Text(fx.findTextById("score"))
    override def init(): Unit = {
      arc.fill.value = yColorMap(osc.x)
      val locR = osc.loc
      val bnds = fx.getBoundsInLocal
      fx.relocate(locR.x - bnds.getWidth / 2.0, locR.y - bnds.getHeight / 2.0 - 15) // magic # for reflect
    }
    import OscFX._

    final def tick(te: Tick): Unit = {
      val radians = osc.phase
      arc.length = -Math.toDegrees(radians)
      if (radians.~(0.5 * Math.PI)(oscFXPrec)) {
        //      logger.debug("osc act sample")
        val act = osc.y.act
        text.text = f"${act}%.1f"
        var perc = Math.abs(act) / YRange.scaleF
        if (perc > 2.0) {
          fx.scaleX.value = perc / 2.0
          fx.scaleY.value = perc / 2.0
          perc = 2.0
        } else {
          fx.scaleX.value = 1.0
          fx.scaleY.value = 1.0
        }
        val rad = minArcRadius + perc / 2.0 * diffArcRadius
        arc.radiusX.value = rad
        arc.radiusY.value = rad
      }
    }
  }
}
