package memnets.fx.games.physics

import memnets.fx._
import memnets.model._
import scalafx.beans.property.BooleanProperty
import scalafx.scene.paint.Color

class Projectile2D(
    val env: Newton2D,
    val color: Color = Color.Yellow,
    val show: Boolean = true,
    val scale: Double = 8.0)(implicit system: DynamicSystem)
    extends Logging {

  val id = env.add(this)
  import env._
  val velX = Y("Vx", scale = velScale)
  val velY = Y("Vy", scale = velScale)
  val gT = Param("gravity", max = -1.0, system = true)
  env.g --> velY tie = gT
  val x = Y("X", scale = scale)
  val y = Y("Y", act = Activation.Relu, scale = scale)
  val vT = Param("velocity", max = 1.0, init = 1.0, system = true)
  velX --> x tie = vT
  velY --> y tie = vT
  for (v <- List(velY, velX, x, y)) {
    if (!show) v.ui.skip()
    v.ui.showText = true
  }
  //  g.color = YColorMap.INH_COLOR.interpolate(Color.Black, 0.4)
  g.ui.color = USER_COLOR
  velX0.ui.color = Color.DarkSlateBlue
  velY0.ui.color = Color.Orange
  vel.ui.color = ACC_COLOR
  angle.ui.color = ACC_COLOR.hueShift(20)
  //  angle.ui.color = Color.DarkViolet
  //  vel.ui.color = Color.DeepPink

  var timeCount = 0.0
  var fireAngle = 0.0
  var fireVel = 0.0
  val fire = BooleanProperty(false)
  val landed = BooleanProperty(false)
  fire ==> { b =>
    if (b) onFire
  }
  def onFire: Unit = {
    timeCount = 0.0
    fireAngle = env.angle.act
    fireVel = env.vel.act
    velX.update(env.velX0)
    velY.update(env.velY0)
    vT.value = 1.0
    gT.value = -1.0
  }
  def onLanded: Unit = {
    logger.debug(s"landed : $this")
    tiesOff
    env.landedCount.value = env.landedCount.value + 1
  }
  def tiesOff: Unit = {
    gT.value = 0.0
    vT.value = 0.0
  }
  landed ==> { newVal =>
    if (newVal) onLanded
  }
  def reset: Unit = {
    if (!fire.isBound) fire.value = false
    landed.value = false
    tiesOff
    timeCount = 0.0
    fireAngle = 0.0
  }
  protected val tau: Double = system.tau
  def t = timeCount / tau
  def tick(te: Tick): Unit = {
    if (fire.value && !landed.value) {
      checkConstraints
      timeCount += 1.0
    }
  }
  def checkConstraints: Unit = {
    if (y.act <= 0.0)
      landed.value = true
  }
  def layout(env: Newton2D): Unit = {
    // layout, do b4 trials
    velX.ui.loc = env.velX0.ui.loc.right(60)
    velY.ui.loc = env.velY0.ui.loc.left(60)
    x.ui.loc = velX.ui.loc.up(ySpacing)
    y.ui.loc = velY.ui.loc.up(ySpacing)
  }
}
