package memnets.lwjgl

import memnets.lwjgl.util._
import memnets.model._
import memnets.ui._

class BatterySkinGL extends SkinGLBase {
  name = "BatteryGL"
  def createY(y: Y) = new BatteryGL(y)
  override def createPhasePlot(pl: PhasePlot) = new PhasePlotGL(pl)

  class PhasePlotGL(pp: PhasePlot) extends TickableGL {
    logger.debug("PhasePlotGL[ph= " + pp + "]")
    // todo: only uses 1st for now
    val phase2D = pp.phases.head
    val ph3D: Option[Phase3D] = phase2D match {
      case ph3: Phase3D =>
        Some(ph3)
      case default =>
        None
    }
    val xAxis = new AxisGL(AxisType.XAxis)
    val yAxis = new AxisGL(AxisType.YAxis)
    val zAxis = new AxisGL(AxisType.ZAxis)
    object comet extends CometGL(scale = phase2D.scale.asInstanceOf[Float], window = phase2D.window) {
      override def tickHelper(i: Int, pt: Pt3Df, ball: SphereGL): Unit = {
        if (i % 7 != 0)
          ball.phong.diffuse = if (pt.x > 0.0) xAxis.posCol else xAxis.negCol
        else
          ball.phong.diffuse = if (pt.y > 0.0) yAxis.posCol else xAxis.negCol
      }
    }
    xAxis.color(yColorMap(phase2D.x))
    yAxis.color(yColorMap(phase2D.y))
    if (ph3D.isDefined) {
      zAxis.color(yColorMap(ph3D.get.z))
      zAxis.visible = true
      comet.temporal = false
    }
    def tick(te: Tick): Unit = {
      gc.globalAlpha = 1.0
      gc.lineWidth = 2.0

      xAxis.tick()
      yAxis.tick()
      zAxis.tick()

      import phase2D._
      if (te.t % sampling == 0)
        comet.tick(x.act, y.act, if (ph3D.isDefined) ph3D.get.z.act else 0.0)
    }
  }
  class BatteryGL(ys: Y) extends TickableGL {
    logger.debug("BatteryGL[y= " + ys + "]")
    val x = ys.ui.loc.x - ctrX
    val y = ctrY - ys.ui.loc.y // y is upside down...
    val yScale = ys.ui.scale.getOrElseP(YRange.scaleF).asInstanceOf[Double]
    val r = 15.0f
    val posCol = yColorMap(ys)
    val negCol = posCol.darker().darker()
    val phong = new PhongMaterialGL()
    val cylinder = new Cylinder(20)
    import org.lwjgl.opengl.GL11._
    def tick(te: Tick) = {
      val act = ys.act
      val barCol = if (act < -0.01) negCol else posCol
      val mag = Math.abs(act)
      renderOutEdges(te, ys, mag, 2.0 * r, barCol)
      phong.diffuse = barCol
      phong.applyGL11()
      glPushMatrix()
      glTranslated(x, y + r, 0.0)
      val barH = Math.max(0.1, mag * 100.0 / yScale)
      glScaled(r, r, barH)
      cylinder.draw()
      glPopMatrix()
    }
  }
}
