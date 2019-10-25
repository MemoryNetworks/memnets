package memnets.fx.fx3d

import memnets.fx._
import memnets.model._
import scalafx.Includes._
import scalafx.scene._
import scalafx.scene.shape.Box

class Box3DFX(val grid: GridData, val colorMap: ColMap = HeatMapFX(), val rad: Double = 6.0)
    extends Group
    with Tickable3DFX
    with Logging {
  if (!(rad > 0.0)) logger.warn("radius not set correctly")
  private val boxW = 2.0 * rad
  val boxH = 1.5 * boxW
  val yOff = boxH / 2.0
  val scale = 1.5 * boxH / grid.yScale
  val boxes = Array.tabulate(grid.rows, grid.cols) { (r, c) =>
    object box extends Box with Phong {
      width = 1.9 * rad
      depth = 1.9 * rad
      height = boxH
      translateY.value = -yOff
      translateX.value = c * boxW
      translateZ.value = r * boxW
      phong.specularPower.value = 100.0
    }
    children += box
    box
  }
  managed = false
  autoSizeChildren = false
  translateX = -boxW * grid.rows / 2.0
  translateZ = -boxW * grid.cols / 2.0
  def node = this
  final def tick(te: Tick) = {
    var r = 0
    while (r < grid.rows) {
      var c = 0
      while (c < grid.cols) {
        val act = grid.act(te, r, c)
        val s = boxes(r)(c)
        s.delegate.translateYProperty.set(scale * act - yOff)
        s.phong.delegate.setDiffuseColor(colorMap(act))
        c += 1
      }
      r += 1
    }
  }
}
