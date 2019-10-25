package memnets.awt

import java.awt.{Color, _}
import java.lang.Math.round

import memnets.model._
import memnets.ui._
import memnets.utils._

object PlotAwt {
  val TRANSPARENT_AWT = new java.awt.Color(0, 0, 0, 0)
}

/**
 *
 */
class PlotAwt(val plot: Plot, w: Int, h: Int, val yColorMap: YColorMap[Col], wScale: Double = 1.0) extends Logging {
  import java.awt.image._

  import PlotAwt._
  import VariableType._
  require(plot.layers.size > 0, "empty plot")

  protected def createColorsAwt(layer: LayerLike): Array[java.awt.Color] = {
    Array.tabulate(layer.length) { i =>
      yColorMap(layer, i)
    }
  }
  val bufferedImage = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
  val g2d = bufferedImage.createGraphics
  g2d.setBackground(TRANSPARENT_AWT)
// too expensive g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
  val hD = h.toDouble
  val stacked = Array.ofDim[Int](plot.layers.head.length)
  val zeroAwt = Color.GRAY
  val zeroStroke: BasicStroke = new BasicStroke(1.0f)
  val rangeAwt = Color.GRAY
  val format: FastFormatter = OneDigit
  val range = plot.range
  val yScale = range.range
  val hScale = hD / yScale
  val off: Int = (hD * (range.max / range.range)).toInt - 1
  val zero: Int = h - 1 - off
  val maxYStr = format.format(range.max)
  val minYStr = format.format(range.min)

  abstract class LayerRender(val layer: LayerLike, val index: Int) {
    def render(): Unit
    val wScale = PlotAwt.this.wScale
    val showText = layer.ui.showText
    val isSoftMax = layer.isInstanceOf[SoftMax]
    val size = layer.length
    val spacing = layer.ui.spacing(w)
    val xArray = Array.ofDim[Int](size)
    val yArray = Array.ofDim[Int](size)
    val awt = yColorMap(layer)
    val nameAwt = awt.trans(0.7)
    // NOTE : had performance issue when stroke width > 1 at one point, re-eval
    val stroke: BasicStroke = if (layer.length <= 2000) new BasicStroke(2.0f) else new BasicStroke(1.0f)
    val nameStart: Int = {
      val txt_w = g2d.getFontMetrics.stringWidth(layer.name)
      val txt_spacing = 80
      val len = plot.layers.size
      val start = if (len % 2 == 0) -txt_spacing * (len - 1) / 2 else -txt_spacing * Math.floor(len / 2).toInt
      -txt_w / 2 + start + index * txt_spacing
    }
    def init(): Unit = {
      if (plot.stackedBars || layer.ui.numericalType == Continuous) {
        var i = 0
        while (i < size) {
          xArray(i) = (i * spacing).asInstanceOf[Int]
          i += 1
        }
      } else {
        if (smallLayer(layer)) {
          var i = 0
          while (i < size) {
            xArray(i) = (i * spacing + index * (spacing / rendersSize)).asInstanceOf[Int]
            i += 1
          }
        } else {
          val bar = Math.max(3.0, spacing / rendersSize)
          var i = 0
          while (i < size) {
            xArray(i) = (i * spacing + index * bar).asInstanceOf[Int]
            i += 1
          }

        }
      }
    }
  }
  val renders: Array[LayerRender] = plot.layers.zipWithIndex.map {
    case (lay, index) =>
      lay match {
        case cont: LayerLike if cont.ui.numericalType == Continuous =>
          logger.trace("cont for " + cont.name)
          new LayerRender(lay, index) {
            def render(): Unit = {
              var i = 0
              while (i < size) {
                yArray(i) = (off - hScale * layer(i)).asInstanceOf[Int]
                i += 1
              }
              g2d.drawPolyline(xArray, yArray, yArray.length)
            }
          }
        case stack: LayerLike if plot.stackedBars =>
          logger.trace("stacked for " + stack.name)
          new LayerRender(lay, index) {
            val colors: Array[java.awt.Color] = createColorsAwt(stack)
            def render(): Unit = {
              var i = 0
              val sp = (spacing - 1).asInstanceOf[Int]
              val w = if (sp < 1) 1 else sp
              while (i < size) {
                g2d.setColor(colors(i))
                val h = round(hScale * layer(i)).asInstanceOf[Int]
                g2d.fillRect(xArray(i), off - h - stacked(i), w, h)
                stacked(i) += h
                i += 1
              }
            }
          }
        case small: LayerLike if smallLayer(small) =>
          logger.trace("small for " + small.name)
          new LayerRender(lay, index) {
            val colors: Array[java.awt.Color] = createColorsAwt(small)
            def render(): Unit = {
              var i = 0
              val sp = ((spacing - 1) / rendersSize).asInstanceOf[Int]
              val w = if (sp < 1) 1 else sp
              while (i < size) {
                g2d.setColor(colors(i))
                val h = (hScale * layer(i)).asInstanceOf[Int]
                if (h > 0)
                  g2d.fillRect(xArray(i), off - h, w, h)
                else
                  g2d.fillRect(xArray(i), off, w, -h)
                i += 1
              }
            }
          }
        case default =>
          logger.trace("default for " + default.name)
          new LayerRender(lay, index) {
            def render(): Unit = {
              val w = 3
              if (layer.lastTopK.isDefined) {
                val top = layer.lastTopK.get
                val len = top.entries.length
                var i = 0
                while (i < len) {
                  val entry = top.entries(i)
                  val j = entry.index
                  g2d.setColor(yColorMap(entry.y))
                  val h = (hScale * layer(j)).asInstanceOf[Int]
                  if (h > 0)
                    g2d.fillRect(xArray(j), off - h, w, h)
                  else
                    g2d.fillRect(xArray(j), off, w, h)
                  i += 1
                }
              }
            }
          }
      }
  }.toSeq.reverse.toArray // define last
  val rendersSize: Double = renders.length

  def init(): Unit = {
    for (info <- renders)
      info.init()
  }
  def render(layRender: LayerRender): Unit = {
    import layRender._

    g2d.setColor(nameAwt)
    // note : layers in reverse order, so put names in right to left
    g2d.drawString(layer.name, w / 2 + nameStart, h - 1)

    g2d.setColor(awt)
    g2d.setStroke(stroke)
    layRender.render()
  }
  def tick(): Unit = {
    g2d.clearRect(0, 0, w, h)
    if (plot.stackedBars) {
      var i = 0
      while (i < stacked.length) {
        stacked(i) = 0
        i += 1
      }
    }
    var i = 0
    while (i < renders.length) {
      val info = renders(i)
      if (i == renders.length - 1) {
        if (plot.showZeroLine) {
          g2d.setStroke(zeroStroke)
          g2d.setPaint(zeroAwt)
          val x = info.xArray
          g2d.drawLine(x(0), off, x(x.length - 1), off)
        }
        if (plot.showRange) {
          g2d.setPaint(rangeAwt)
          g2d.drawString(maxYStr, 4, 10)
          g2d.drawString(minYStr, 4, h - 1)
        }
      }
      render(info)
      i += 1
    }
  }
  private def smallLayer(layer: LayerLike) = layer.length <= 1024
}
