package memnets.fx.fx3d

import memnets.fx._
import memnets.model._
import scalafx.scene.Group
import scalafx.scene.paint.Color
import scalafx.scene.shape._
import scalafx.scene.transform._

object PendulumSkin3DFX {
  def apply() = new Skin3DFXAdapter(new PendulumSkin3DFX())
}
class PendulumSkin3DFX extends BatterySkin3DFX {
  name = "Pendulum3D"
  protected var oscCount = 0
  protected var oscStartZ = 0.0
  override def layout(model: DynamicSystem): Unit = {
    super.layout(model)
    oscCount = model.oscCount
    oscStartZ = 2.0 * Pendulum3DFX.RADIUS * (oscCount + 1) / 2.0
  }
  override def createY(y: Y) = None
  override def createYGoal(yg: YGoal) = {
    yg.linkable match {
      case Some(osc: Osc) =>
        new Goal3DFX(goal = yg, z = calcZ(osc), rad = 10.0)
      case default =>
        super.createYGoal(yg)
    }
  }
  override def createOsc(osc: Osc) = new Pendulum3DFX(osc, z = calcZ(osc), color = yColorMap(osc))
  def calcZ(osc: Osc): Double = -oscStartZ + 2.0 * Pendulum3DFX.RADIUS * osc.oscId
}
object Pendulum3DFX {
  val HEIGHT = 200.0
  val SCALE = 30.0
  val RADIUS = 10.0
}
class Pendulum3DFX(val x: Y, val z: Double, val color: Color, val rad: Double)
    extends Group
    with Tickable3DFX
    with Logging {
  userData = x
  def this(osc: Osc, z: Double, color: Color, rad: Double = Pendulum3DFX.RADIUS) {
    this(osc.y, z, color, rad)
  }
  def node = this
  val sphere = new Sphere(radius = rad)
  sphere.userData = x
  val rod = new Cylinder(radius = 1.0, height = Pendulum3DFX.HEIGHT, divisions = 8) with Phong
  val anchor = new Box(width = 10.0, height = 5.0, depth = 2 * rad)

  Textures(Textures.SILVER, sphere)
  Textures(Textures.WOOD, anchor)
  rod.phong.diffuseColor = color
  rod.phong.specularColor = color.brighter
  rod.phong.specularPower = 20.0

  children.addAll(sphere, rod, anchor)
  val h = rod.height.value / 2.0
  sphere.translateY = -h + 0.99 * rad // up from floor
  anchor.translateY = h
  val trans = new Translate(0, h, z)
  val rot = new Rotate(-90.0, 0, h, z)
  transforms.addAll(trans, rot)
  def tick(te: Tick): Unit = {
    rot.delegate.angleProperty
      .set(-90.0 + Math.toDegrees(Math.atan2(rod.delegate.heightProperty.get, -Pendulum3DFX.SCALE * x.act)))
  }
  def zOff_=(z: Double): Unit = {
    trans.z = z
    rot.pivotZ = z
  }
  def zOff = trans.z.value
}

class Goal3DFX(val goal: YGoal, val z: Double, val rad: Double) extends Group with Tickable3DFX with Logging {
  override def permanent = false
  def node = this
  val block = new Cylinder(radius = rad, height = 2.0 * rad) with Phong
  userData = goal
  val color = if (goal.isGood) WIN_COLOR else LOSE_COLOR
  Textures(Textures.SILVER, block, share = false)
  val col = color.deriveColor(hueShift = 0.0, saturationFactor = 0.7, brightnessFactor = 1.0, opacityFactor = 1.0)
  block.phong.diffuseColor = col
  block.phong.specularColor = Color.Gray
  block.phong.specularPower = 5.0

  block.userData = goal // for pick
  block.translateY <== block.height * block.scaleY / 2.0
  children.addAll(block)
  translateZ = z
  val goalAngle = Math.atan2(Pendulum3DFX.HEIGHT, Pendulum3DFX.SCALE * goal.expected)
  translateX = block.radius.value + Math.sin(goalAngle) * Pendulum3DFX.SCALE * goal.expected
  def tick(te: Tick): Unit = {
    if (te.even) {
      val act = goal.tgt.act
      if (act > 0.0)
        block.delegate.setScaleY(1.0 + 4.0 * act / goal.expected)
      else
        block.delegate.setScaleY(0.05)
    }
  }

  override def toString(): String = s"Goal3DFX[goal= $goal]"
}
