package memnets.fx

import memnets.model._
import scalafx.scene.paint._

class CircleSkinFX(val rad: Double = 18.0, showEdges: Boolean = true) extends SkinFX {
  import scalafx.scene.shape._
  name = "Circle"
  backImageOn = false
  backColor = Colorf.gray(0.3)
  case class CircleFX(ys: Y) extends Circle with TickableFX {
    val baseCol = yColorMap(ys)
    val posCol = RadialGradient(
      0.0,
      0.0,
      0.5,
      0.5,
      0.6,
      true,
      CycleMethod.NoCycle,
      Stop(0.0, baseCol),
      Stop(1.0, Color.Transparent))
    val negCol = RadialGradient(
      0.0,
      0.0,
      0.5,
      0.5,
      0.6,
      true,
      CycleMethod.NoCycle,
      Stop(0.0, baseCol.darker.darker),
      Stop(1.0, Color.Transparent))
    val posEdgeCol = baseCol.brighter.brighter
    val negEdgeCol = baseCol.darker.darker
    val x = ys.ui.loc.x
    val y = ys.ui.loc.y
    val yScale = ys.ui.scale.getOrElseP(YRange.scaleF).asInstanceOf[Double]
    cache = true
    radius = rad
    opacity = 1.0
    fill = Color.Transparent
    strokeWidth = 0.0
    override def init(): Unit = { relocate(x - rad, y - rad) }
    def node = Some(this)
    def tick(te: Tick): Unit = {
      val act = ys.act
      val mag = Math.abs(act)
      if (mag > 0.001) {
        visible.value = true
        fill = if (act > 0.000) posCol else negCol
        val a = mag / yScale
        if (a > 1.0) {
          val scale = Math.min(2.0, a)
          scaleX = scale
          scaleY = scale
          opacity = 1.0
        } else {
          scaleX = 1.0
          scaleY = 1.0
          opacity = a
        }
        if (showEdges) {
          val edgeCol: Color = if (act < -0.01) negEdgeCol else posEdgeCol
          renderEdges(te, ys, mag, 0.0, edgeCol, alpha = 0.2)
        }
      } else
        visible.value = false
    }
    override def findTarget(x0: Double, y0: Double) = Some(new UserSource(ys, h = 0, xoff = rad))
  }
  override def createY(y: Y) = CircleFX(y)
  override def isSuitable(system: DynamicSystem): Boolean = {
    system.layers.length == 0 && system.variables.length < 4096
  }
}
