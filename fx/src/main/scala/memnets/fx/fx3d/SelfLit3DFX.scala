package memnets.fx.fx3d

import scalafx.scene._
import scalafx.scene.paint.Color
import scalafx.scene.shape.Shape3D

case class SelfLit3DFX[T <: Shape3D](shape: T) extends Group {
  val selfLit = new AmbientLight(Color.web("#fff"))
  selfLit.scope += shape
  children.addAll(selfLit, shape)
}
