package memnets.fx.games.physics

import memnets.fx._
import memnets.model.Activation.Relu
import memnets.model._
import scalafx.beans.property.BooleanProperty
import scalafx.geometry.Point3D

// realistic velScale ~ 120, but don't want to go so far...
abstract class Projectile3D(
    val velScale: Double = 120.0,
    val worldScale: Double = 2000.0,
    val ySpacing: Int = 150,
    val show: Boolean = false,
    val showText: Boolean = false,
    val startY: Double = 3.0)(implicit system: DynamicSystem)
    extends Logging {

  val g = Y("g", decay = -1.0, tau = 10.0, threshold = -9.8)
  g.ui.viz = Viz.User

  def sampleVel0: Point3D
  // Y will use system.tauDefault
  val tau: Double = system.tau
  val velX = Y("Vx", scale = velScale)
  val velY = Y("Vy", scale = velScale)
  val velZ = Y("Vz", scale = velScale)
  val gT = Param("gravity", max = -1.0)
  g --> velY tie = gT
  val x = Y("X", scale = worldScale)
  val y = Y("Y", act = Relu, scale = 2.0 * velScale)
  val z = Y("Z", scale = worldScale)
  val vT = Param("velocity", max = 1.0, init = 1.0)
  val velVec = Array(velX, velY, velZ)
  val coordVec = Array(x, y, z)
  for ((v, c) <- velVec.zip(coordVec)) v --> c tie = vT
  val variables = List(velY, velX, velZ, x, y, z)
  for (v <- variables) {
    if (!show) v.ui.skip()
    v.ui.showText = showText
  }
  var origX, origZ = 0.0
  protected var _timeCount, _maxY = 0.0
  val fire = BooleanProperty(false)
  val landed = BooleanProperty(false)
  fire ==> { b =>
    if (b) {
      _timeCount = 0.0
      landedTick = -1
      origX = x.act
      origZ = z.act
      val sample = sampleVel0
      velX.update(sample.x)
      velY.update(sample.y)
      velZ.update(sample.z)
      vT.value = 1.0
      gT.value = -1.0
    }
  }
  val inFlight = fire && !landed
  def tiesOff: Unit = {
    gT.value = 0.0
    vT.value = 0.0
  }
  var landedTick = -1
  landed ==> { b =>
    if (b) {
      tiesOff
      landedTick = system.now.t
    }
  }
  def reset: Unit = {
    if (!fire.isBound) fire.value = false
    landed.value = false
    tiesOff
    y.update(startY) // default height
    _timeCount = 0.0
    _maxY = 0.0
  }
  def distance = euclid(origX, origZ, x.act, z.act)
  def t = _timeCount / tau
  def maxY = _maxY
  def tick(te: Tick): Unit = {
    if (inFlight.get) {
      checkConstraints
      _timeCount += 1.0
    }
  }
  def checkConstraints: Unit = {
    val yNow = y.act
    if (yNow <= 0.0)
      landed.value = true
    else if (yNow > _maxY) _maxY = yNow
  }
}
