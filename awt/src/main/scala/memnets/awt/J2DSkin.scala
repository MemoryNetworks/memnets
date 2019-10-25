package memnets.awt

import java.awt._
import java.awt.geom.AffineTransform

import javax.swing._
import memnets.model.{Plot, _}

import scala.math.abs

class J2DSkin extends JSkinBase {
  name = "J2DSKin"

  val borderCol = Color.DARK_GRAY
  val borderStroke = new BasicStroke(1.0f)
  val identity = new AffineTransform

  override def createSignal(sig: Signal) = new JSignal(sig)
  def createY(ys: Y): Option[JTickable] = new JBattery(ys)
  override def createImage(grid: GridData) = new JImage(grid)
  override def createPlot(plot: Plot) = new JPlot(plot)

  class JBattery(ys: Y) extends JTickable {
    def node: Option[JComponent] = None
    val x = ys.ui.loc.x.toInt
    val y = ys.ui.loc.y.toInt
    val yScale = ys.ui.scale.getOrElseP(YRange.scaleF).asInstanceOf[Double]
    val w = 26
    val posCol = yColorMap(ys)
    val negCol = yColorMap(ys).darker()
    def tick(te: Tick) = {
      val act = ys.act
      val barCol = if (act > 0.0) posCol else negCol
      val mag = abs(act)
      if (mag > 0.01)
        renderOutEdges(te, ys, mag, h, barCol)
      var scale = xscale(yScale)
      val bscale = mag / yScale
      var barH: Int = (bscale * h).toInt
      if (bscale > 1.0) {
        barH = h
        scale = scale * bscale
      }
      val barW: Int = Math.floor(w * scale).toInt
      val x2: Int = x - barW / 2

      g2.setColor(barCol)
      g2.fillRect(x2, y - barH, barW, barH)

      g2.setColor(borderCol)
      g2.setStroke(borderStroke)
      g2.drawRect(x2, y - h, barW, h)
      g2.drawString(ys.name, x - g2.getFontMetrics.stringWidth(ys.name) / 2, y - h / 2)
    }
  }
  class JSignal(val sig: Signal) extends JTickable {
    def node: Option[JComponent] = None
    val barCol = new Color(1.0f, 1.0f, 1.0f, 0.5f)
    val x = sig.tgt.ui.loc.x.toInt
    val y = sig.tgt.ui.loc.y.toInt
    val yScale = sig.tgt.ui.scale.getOrElseP(YRange.scaleF).asInstanceOf[Double]
    val barW = 10
    def tick(te: Tick): Unit = {
      val barH: Int = (h * sig.act / yScale).asInstanceOf[Int]
      g2.setColor(barCol)
      g2.fillRect(x - barW - 20, y - barH, barW, barH)
    }
  }
  class JImage(val grid: GridData) extends JTickable {
    val imageAwt = new ImageAwt(grid, colorMap)
    def node: Option[JComponent] = None
    val w = grid.hints.width
    val h = grid.hints.height
    override def reset(): Unit = {
      super.reset()
      imageAwt.reset()
    }
    override def destroy(): Unit = {
      super.destroy()
      imageAwt.destroy()
    }
    val transform = new AffineTransform
    transform.translate(grid.ui.loc.x, grid.ui.loc.y)
    transform.scale(grid.hints.scaleX, grid.hints.scaleY)
    transform.translate(-w / 2.0, -h / 2.0)
    def tick(te: Tick): Unit = {
      if (grid.preRender(te))
        imageAwt.tick(te)

      g2.setColor(borderCol)
      g2.setStroke(borderStroke)
      g2.setTransform(transform)
      g2.drawRect(-1, -1, w + 2, h + 2)
      g2.drawImage(imageAwt.bufferedImage, 0, 0, w, h, null)
      g2.setTransform(identity)
    }
  }
  class JPlot(val plot: Plot) extends JTickable {
    def node: Option[JComponent] = None
    val w = plot.width.toInt
    val h = plot.height.toInt
    val layImg = new PlotAwt(plot, w, h, yColorMap, edgeScale)
    layImg.init()
    val x: Int = (plot.loc.x - w / 2.0).toInt
    val y: Int = (plot.loc.y - h / 2.0).toInt
    def tick(te: Tick): Unit = {
      layImg.tick()
      g2.setColor(borderCol)
      g2.setStroke(borderStroke)
      g2.drawRect(x - 1, y - 2, w + 2, h + 2)
      g2.drawImage(layImg.bufferedImage, x, y, null)
    }
  }
}
