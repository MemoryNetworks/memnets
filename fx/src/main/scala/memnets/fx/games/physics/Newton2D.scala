package memnets.fx.games.physics

import java.lang.Math._

import memnets.fx._
import memnets.model.Activation.Relu
import memnets.model._
import scalafx.beans.property.IntegerProperty
import scalafx.scene.paint.Color

import scala.collection.mutable.ArrayBuffer

class Newton2D(val velScale: Double = 16.0, val ySpacing: Int = 150)(implicit sys: DynamicSystem) extends Logging {
  val GRAVITY = 9.8
  val velX0 = Y("Vx0", decay = -1.0, tau = 1.0, act = Relu, scale = velScale)
  val velY0 = Y("Vy0", decay = -1.0, tau = 1.0, act = Relu, scale = velScale)
  val g = Y("g", decay = -1.0, tau = 10.0, threshold = -GRAVITY)
  g.ui.viz = Viz.User

  val angle = Y("Angle", decay = -1.0, tau = 5.0, act = Relu, scale = 90.0)
  val vel = Y("Vel", decay = -1.0, tau = 5.0, act = Relu, scale = 20.0)
  velX0.f("F*cos(ang)", vel, angle) { t =>
    vel * cos(toRadians(angle))
  }
  velY0.f("F*sin(ang)", vel, angle) { t =>
    vel * sin(toRadians(angle))
  }

  for (v <- List(vel, velX0, velY0, g, angle)) v.ui.showText = true

  g.ui.color = USER_COLOR
  velX0.ui.color = Color.DarkSlateBlue
  velY0.ui.color = Color.Orange
  vel.ui.color = ACC_COLOR
  angle.ui.color = ACC_COLOR.hueShift(20)

  val landedCount = IntegerProperty(0)
  protected val _projectiles = ArrayBuffer[Projectile2D]()
  private[memnets] def add(p: Projectile2D): Int = {
    _projectiles += p
    _projectiles.size - 1
  }
  def projectiles: scala.collection.IndexedSeq[Projectile2D] = _projectiles
  def reset: Unit = {
    for (p <- projectiles) p.reset
    landedCount.value = 0
  }
  def tick(te: Tick): Unit = { for (p <- projectiles) p.tick(te) }
  def layout(loc: Loc = Loc().down(60).right(350), ySpacing: Int = 230): Unit = {
    velX0.ui.loc = loc
    velY0.ui.loc = velX0.ui.loc.right(170)
    angle.ui.loc = velX0.ui.loc.right(20).down(ySpacing)
    vel.ui.loc = velY0.ui.loc.left(80).down(ySpacing)
    g.ui.loc = velY0.ui.loc.down(ySpacing)
  }
  override def toString: String = "Newton2D[]"
}
