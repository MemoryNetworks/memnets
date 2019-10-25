package memnets.fx.games.physics

import memnets.model._
import scalafx.beans.property.IntegerProperty
import scalafx.scene.paint.Color

class BouncingBall(
    override val env: Newton2D,
    var dampen: Double = 0.75,
    val minBounceVel: Double = 2.0,
    val maxVel: Double = 20.0,
    override val color: Color = Color.Yellow,
    override val show: Boolean = true)(implicit sys: DynamicSystem)
    extends Projectile2D(env, color, show, scale = 2) {

  val dampenDefault = dampen
  def dampenReset(): Unit = { dampen = dampenDefault }
  var lastCollision = -1
  val bounces = IntegerProperty(0)
  override def reset: Unit = {
    super.reset
    lastCollision = -1
    bounces.value = 0
    dampenReset()
  }
  def bounce(yPos: Option[Double] = None): Unit = {
    logger.debug("bounce")
    val vy = velY.act
    for (ypos <- yPos) y.update(ypos)
    val v = -dampen * vy
    if (v < 0.0)
      velY.update(Math.max(-maxVel, v))
    else
      velY.update(Math.min(maxVel, v))
    bounces.value += 1
  }
  def reverse(xPos: Option[Double] = None): Unit = {
    val vx = velX.act
    velX.update(-vx)
    for (xpos <- xPos) x.update(xpos)
  }
  // doing in UI
  override def checkConstraints: Unit = {}
}
