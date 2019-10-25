package memnets.fx.fx3d

import scalafx.scene.shape._

object Plane3DFX {
  def apply(
      texture: String = "",
      h: Double = 20.0,
      yoffset: Double = 0.0,
      scale: Double = 1.0,
      stretchX: Option[Double] = None,
      stretchZ: Option[Double] = None,
      w: Double = 2048.0,
      d: Option[Double] = None): Plane3DFX = {
    val plane = new Plane3DFX(h, yoffset)
    import plane._
    width = w
    depth = d.getOrElse(w)
    scaleX.value = stretchX.getOrElse(scale)
    scaleZ.value = stretchZ.getOrElse(scale)
    pickOnBounds = true
    if (texture != "") Textures(name = texture, shape3d = plane)
    plane
  }
}

class Plane3DFX(val h: Double = 20.0, val yoffset: Double = 0.0) extends Box with Phong {
  height = h
  translateY.value = yoffset - h / 2.0
  pickOnBounds = true
}
