package memnets.lwjgl

import java.awt.Color

import memnets.awt._
import memnets.lwjgl.util.GLUtils
import memnets.model._
import memnets.ui._
import org.lwjgl.opengl.GL11._

abstract class SkinGLBase extends Skin[Unit, java.awt.Color] with Logging {
  type UI = TickableGL
  val ctrX = Loc().x
  val ctrY = Loc().y
  val xScaleHint: Double = 1.0
  val h = 87.0
  var showCanvas3d = true
  def xscale(yScale: Double) =
    xScaleHint * (if (yScale < YRange.scale) 0.8 else if (yScale > YRange.scale) 1.4 else 1.0)
  protected def convertToColHelper(colorf: Colorf) = new java.awt.Color(colorf.r, colorf.g, colorf.b, colorf.a)
  protected def heatMap = HEAT_COLORMAP
  // todo
  protected def opacityMap = heatMap
  protected def grayMap = heatMap
  protected def invertedGrayMap = heatMap
  protected def createYGradientMap(model: DynamicSystem) = YGradientAwt()

  object gc {
    var globalAlpha = 1.0

    def fill: Color = ???
    def fill_=(col: Color): Unit = {
      val comps = col.toArray
      glColor4d(comps(0), comps(1), comps(2), comps(3) * globalAlpha)
    }
    def stroke: Color = ???
    def stroke_=(col: Color): Unit = {
      val comps = col.toArray
      glColor4d(comps(0), comps(1), comps(2), comps(3) * globalAlpha)
    }
    def lineWidth: Double = ???
    def lineWidth_=(d: Double): Unit = { glLineWidth(d.asInstanceOf[Float]) }
    def strokeLine(x: Double, y: Double, x2: Double, y2: Double): Unit = {
      glBegin(GL_LINES)
      glVertex2d(x, y)
      glVertex2d(x2, y2)
      glEnd()
    }
  }
  def renderOutEdges(
      te: Tick,
      src: Y,
      mag: Double,
      h: Double,
      edgeCol: Color,
      lwMax: Double = 6.0,
      alpha: Double = 0.7
  ) = {

    if (edgeScale > 0) {
      val outScale = Math.max(0.1, 1.0 / edgeScale)
      val x = src.ui.loc.x - ctrX
      val y = ctrY - src.ui.loc.y // y is upside down...
      val viz = vizLevel
      gc.globalAlpha = alpha
      for (e <- src.outsNoLoops) {
        val act = te.applyF(e.tgt)
        if (act > 0.01f || act < -0.01f) {
          val tgt = te.system.variables(e.tgt)
          if (tgt.ui.viz >= viz) {
            val w = e.w
            val absmag = if (w > 0.0) {
              gc.stroke = edgeCol
              mag
            } else {
              gc.stroke = INH_EDGE_COLOR
              -mag
            }
            val lw = absmag * w / outScale
            gc.lineWidth = if (lw > lwMax) lwMax else lw
            val tx = tgt.ui.loc.x - ctrX
            val ty = ctrY - tgt.ui.loc.y
            /*
            if (ty < y && abs(tx - x) < 2.0) {
              // todo: logic is wrong
              val xMid = x - 20.0
              val yStart = y - 6.0
              val yEnd = ty + h + 6.0
              val yMid = yStart + (yEnd - yStart) / 2.0
              gc.strokeLine(x, yStart, xMid, yMid)
              gc.strokeLine(xMid, yMid - 2.0, tx, yEnd)
            }
            else

             */
            gc.strokeLine(x, y + h + 4.0, tx, ty - 4.0)
          }
        }
      }
    }
  }
  def clearCanvas(): Unit = {
    if (showCanvas3d) {
      val w = Display.width.asInstanceOf[Float] / 2.0f
      val h = Display.height.asInstanceOf[Float] / 2.0f
      glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, backColorConverted.toArray())
      glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, Color.WHITE.toArray)

      GLUtils.normal3f(0.0f, 0.0f, 1.0f)
      glBegin(GL_QUADS)
      glVertex3f(-w, -h, -0.1f)
      glVertex3f(w, -h, -0.1f)
      glVertex3f(w, h, -0.1f)
      glVertex3f(-w, h, -0.1f)
      glEnd()
//      if (canvasImage.isDefined) {
//        gc.drawImage(canvasImage.get, 0.0, 0.0, del.getWidth, del.getHeight)
//      }
    }
  }
}
