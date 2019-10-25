package memnets.fx.fx3d

import org.fxyz3d.shapes.primitives.Text3DMesh
import scalafx.scene.Group
import scalafx.scene.paint.Color
import scalafx.scene.shape.Cylinder

object Axis3DFX {
  val DefaultColor: Color = Color.Silver
}
class Axis3DFX(val name: String, len: Double = 300.0) extends Group {
  import Axis3DFX._
  import scalafx.Includes._
  val pos = new Cylinder(radius = 2.0, height = len, divisions = 4) with Phong
  val neg = new Cylinder(radius = 2.0, height = len, divisions = 4) with Phong
  children ++= List(pos, neg)
  pos.translateY = len / 2
  neg.translateY = -len / 2
  // NOTE : don't add to children b/cols screws up bounds, have user add to parent group
  // lazy so OpenGL can use (loading font blows up javafx if no scene...)
  lazy val label: Text3DMesh = {
    val label = new Text3DMesh(name, "Tahoma", 16)
    label.setTextureModeNone(Color.Silver)
    label.setScaleZ(0.5)
    label.setTranslateZ(-label.getBoundsInParent.getDepth) // NOTE: should be depth/2.0 but scaleZ factors in
    label.rotateProperty.value = 180.0
    label.visibleProperty <== Axis3DFX.this.visible
    label
  }
  var posColor: Color = _
  var negColor: Color = _
  def color(c: Color): Unit = {
    val col = if (null != c) c else DefaultColor
    posColor = col
    negColor = col.interpolate(Color.Black, 0.7)
    pos.phong.diffuseColor = posColor
    neg.phong.diffuseColor = negColor
  }
  color(DefaultColor)
}
