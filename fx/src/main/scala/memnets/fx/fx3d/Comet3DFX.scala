package memnets.fx.fx3d

import com.sun.javafx.geom.Vec3d
import memnets.fx._
import memnets.model._
import scalafx.Includes._
import scalafx.scene.Group
import scalafx.scene.shape.Sphere

object Comet3DFX {
  type Ball = Sphere with Phong
}

/**
 * map mesh pts dynamically to fixed size spheres
 */
class Comet3DFX(
    val scale: Double = 1.0,
    val window: Int = 200,
    val temporalZ: Double = 1.0,
    val texture: String = Textures.SILVER)
    extends Group
    with Logging {
  import Comet3DFX._
  val radiusScale: Double = 4.0 / window.toDouble
  val spheres = Array.tabulate(window) { i =>
    val s = createBall(i)
    children += s
    s
  }
  def ballRadius(i: Int): Double = 1.0 + (window - i) * radiusScale
  def createBall(i: Int): Ball = {
    val s = new Sphere with Phong
    s.visible = false
    s.radius = ballRadius(i)
    Textures(texture, s, share = false)
    s.phong.specularPower = 10.0
    s
  }
  var temporal = false
  val pts = Array.fill(window) { new Vec3d() }
  var index = 0
  def reset(): Unit = {
    index = 0
    for (s <- spheres) s.visible = false
    for (s <- pts) s.clear
  }
  def tickHelper(i: Int, pt: Vec3d, ball: Ball): Unit = {}
  def tick(x: Double, y: Double, z: Double): Unit = {
    val pt = pts(index)
    pt.x = x
    pt.y = y
    pt.z = z
    if (index < window) spheres(index).visible = true
    // transfer mesh pts to spheres
    var i = 0
    while (i < window) {
      var j = index - i
      if (j < 0) j = window + j
      val pt = pts(j)
      val sphere = spheres(i)
      val del = sphere.delegate // to avoid boxing, use delegate
      del.translateXProperty.set(scale * pt.x)
      del.translateYProperty.set(scale * pt.y)
      del.translateZProperty.set(if (temporal) -temporalZ * i else scale * pt.z)
      tickHelper(j, pt, sphere)
      i += 1
    }
    index = index + 1
    if (index >= spheres.size) index = 0
  }
}
class YComet3DFX(
    tgt: Y,
    val startX: Double = 0.0,
    override val scale: Double = 1.0,
    override val temporalZ: Double = 9.0,
    override val window: Int = 140,
    override val texture: String = Textures.SILVER)
    extends Comet3DFX(scale = scale, window = window, temporalZ = temporalZ, texture = texture)
    with Tickable3DFX {
  temporal = true
  def node = this
  override def reset(): Unit = { super.reset() }
  def tick(te: Tick): Unit = { tick(startX, tgt.act, 0.0) }
}
