package memnets.fx.fx3d

import scalafx.Includes._
import scalafx.scene.Group
import scalafx.scene.paint.Color
import scalafx.scene.transform.Rotate

class Axes3DFX(showLabels: Boolean = true, length: Double = 300.0) extends Group {
  val xAxis = new Axis3DFX("X", length)
  val yAxis = new Axis3DFX("Y", length)
  val zAxis = new Axis3DFX("Z", length)
  xAxis.rotate = -90
  zAxis.rotate = 90
  zAxis.rotationAxis = Rotate.XAxis
  xAxis.color(Color.OrangeRed)
  yAxis.color(Color.Lime)
  zAxis.color(Color.SlateBlue)
  if (showLabels) {
    children ++= List(xAxis, yAxis, zAxis, xAxis.label, yAxis.label, zAxis.label)
    val len = length
    yAxis.label.translateY = len + 15
    yAxis.label.translateX = -yAxis.label.boundsInLocal.value.getWidth / 2

    xAxis.label.translateX = len + 5
    xAxis.label.translateY = xAxis.label.boundsInLocal.value.getHeight / 2 - 2

    zAxis.label.rotationAxis.value = Rotate.XAxis
    zAxis.label.translateZ = len + 2
    zAxis.label.translateX = -zAxis.label.boundsInLocal.value.getWidth / 2
    zAxis.label.translateY = zAxis.label.boundsInLocal.value.getHeight / 2
  } else
    children ++= List(xAxis, yAxis, zAxis)
}
