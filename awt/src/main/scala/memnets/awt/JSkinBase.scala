package memnets.awt

import java.awt._

import javax.imageio.ImageIO
import javax.swing.JComponent
import memnets.model._
import memnets.ui._

import scala.math.abs

abstract class JSkinBase extends Skin[JComponent, Color] {
  type UI = JTickable
  name = "JSkinBase"
  protected def convertToColHelper(colorf: Colorf): Col = new Color(colorf.r, colorf.g, colorf.b, colorf.a)
  protected def heatMap = HEAT_COLORMAP
  // todo
  protected def opacityMap = heatMap
  protected def grayMap = heatMap
  protected def invertedGrayMap = heatMap

  protected def createYGradientMap(model: DynamicSystem) = YGradientAwt()
  protected var backImg: Image = _
  protected var g2: Graphics2D = _
  protected val h = 72
  protected val hints = new RenderingHints(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
  protected val xScaleHint: Double = 1.0
  hints.put(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
  hints.put(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC)

  protected object gc {
    implicit def d2i(d: Double): Int = d.asInstanceOf[Int]
    var globalAlpha = 1.0

    def drawRect(x: Double, y: Double, x2: Double, y2: Double): Unit = { g2.drawRect(x, y, x2, y2) }
    def fill: Color = ???
    def fill_=(col: Color): Unit = {
      if (globalAlpha < 1.0)
        g2.setColor(new Color(col.getRed, col.getGreen, col.getBlue, (col.getAlpha * globalAlpha).asInstanceOf[Int]))
      else
        g2.setColor(col)
    }
    def lineWidth: Double = ???
    def lineWidth_=(d: Double): Unit = { g2.setStroke(new BasicStroke(d.asInstanceOf[Float])) }
    def stroke: Color = ???
    def stroke_=(col: Color): Unit = {
      if (globalAlpha < 1.0)
        g2.setColor(new Color(col.getRed, col.getGreen, col.getBlue, (col.getAlpha * globalAlpha).asInstanceOf[Int]))
      else
        g2.setColor(col)
    }
    def strokeLine(x: Double, y: Double, x2: Double, y2: Double): Unit = { g2.drawLine(x, y, x2, y2) }
  }

  def initStyle(): Unit = {
    if (backImageOn) {
      for (url <- imageLookupURL(backImage)) {
        try {
          backImg = ImageIO.read(url)
        } catch {
          case th: Throwable =>
            logger.error("could find: " + url, th)
        }
      }
    } else
      backImg = null
  }
  def drawBackground(w: Int, h: Int): Unit = {
    if (backImg != null)
      g2.drawImage(backImg, 0, 0, w, h, null)
    else {
      g2.setPaint(backColor)
      g2.fillRect(0, 0, w, h)
    }
  }
  def renderOutEdges(
      te: Tick,
      src: Y,
      mag: Double,
      h: Double,
      edgeCol: Color,
      lwMax: Double = 12.0,
      alpha: Double = 0.4
  ) = {
    val wScale = edgeScale
    if (wScale > 0.0) {
      val x = src.ui.loc.x
      val y = src.ui.loc.y
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
            val lw = wScale * absmag * w
            gc.lineWidth = if (lw > lwMax) lwMax else lw
            val tx = tgt.ui.loc.x
            val ty = tgt.ui.loc.y
            if (ty > y && abs(tx - x) < 2.0) {
              val xMid = x - 20.0
              val yStart = y - h + 5.0
              val yEnd = ty + 12.0
              val yMid = yStart + (yEnd - yStart) / 2.0
              gc.strokeLine(x, yStart, xMid, yMid)
              gc.strokeLine(xMid, yMid + 2.0, tx, yEnd)
            } else
              gc.strokeLine(x, y - h - 3.0, tx, ty + 12.0)
          }
        }
      }
    }
  }
  def xscale(yScale: Double) =
    xScaleHint * (if (yScale < YRange.scale) 0.8 else if (yScale > YRange.scale) 1.4 else 1.0)
  protected def setGraphics2D(g2: Graphics2D): Unit = {
    this.g2 = g2
    this.g2.setRenderingHints(hints)
  }
}
