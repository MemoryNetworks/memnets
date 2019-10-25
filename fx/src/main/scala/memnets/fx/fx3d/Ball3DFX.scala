package memnets.fx.fx3d

import memnets.fx._
import memnets.model._
import scalafx.scene.shape.Sphere

class Ball3DFX(val x: Y, val y: Y, val z: Y, val rad: Double = 10.0)
    extends Sphere(radius = rad)
    with Phong
    with Tickable3DFX {
  layoutY = rad // move up when at zero
  def node = this
  val del = this.delegate
  var doXZ = true
  def tick(te: Tick) = {
    del.setTranslateY(y.act)
    if (doXZ) {
      del.setTranslateX(x.act)
      del.setTranslateZ(-z.act)
    }
  }
}
