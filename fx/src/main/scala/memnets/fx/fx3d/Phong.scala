package memnets.fx.fx3d

import scalafx.scene.paint._

trait Phong { self: scalafx.scene.shape.Shape3D =>
  val phong: PhongMaterial = new PhongMaterial {
    specularColor = Color.web("#bbb")
    specularPower = 60
  }
  material = phong
}
