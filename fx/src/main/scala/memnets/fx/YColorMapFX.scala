package memnets.fx

import memnets.model._
import memnets.ui._
import scalafx.scene.paint._

object YLinearGradientFX {
  import collection.JavaConverters._

  def apply(lg: JLinearGradient) = new YLinearGradientFX(lg.getStops.asScala.map(new Stop(_)).toSeq: _*)
  def apply(start: Color, end: Color) = new YLinearGradientFX(Stop(0.0, start), Stop(1.0, end))
  def apply(stops: Stop*) = new YLinearGradientFX(stops: _*)
}

/** big assumption that always include stop at 0.0 and 1.0 */
class YLinearGradientFX(val stops: Stop*) extends YColorMap[Col] {
  require(stops.length >= 2)
  require(stops.head.getOffset == 0.0)
  require(stops.last.getOffset == 1.0)
  override def signalColor(t: Col) = { t.interpolate(Color.Black, 0.5) }
  def apply(i: Int, length: Int): Col = {
    val perc = i.toDouble / length.toDouble
    val j = stops.lastIndexWhere(_.getOffset <= perc)
    val jcol = if (j >= 0 && j < stops.length - 1) {
      val start = stops(j)
      val end = stops(j + 1)
      val range = end.getOffset - start.getOffset
      start.getColor.interpolate(end.getColor, (perc - start.getOffset) / range)
    } else
      stops.last.getColor
    new Color(jcol)
  }
}

case class YGradientFX(var hints: GradientHints = GradientHints()) extends YGradientMap[Col] {
  override def signalColor(t: Col): Col = { t.interpolate(Color.Black, 0.5) }
  def apply(i: Int, length: Int): Col = {
    val hintz = hints
    import hintz._
    val maxlen = Math.min(length, maxLength)
    Color.hsb((hue + i * (spectrum / Math.max(minDivs, maxlen))) % spectrum, saturation, brightness, opacity)
  }
}

trait CustomColorsFX extends YColorMap[Col] {
  override def apply(y: Y): Col = {
    val cust = custom(y)
    if (cust != null)
      cust
    else
      super.apply(y)
  }
  override def apply(layer: LayerLike, i: Int): Col = {
    if (layer.id != 0)
      custom(layer, i)
    else {
      val cust = custom(layer.system.variables(i))
      if (cust != null)
        cust
      else
        custom(layer, i)
    }
  }
  override def apply(layer: LayerLike): Col = {
    val cust = custom(layer)
    if (cust.isDefined)
      cust.get(layer)
    else
      super.apply(layer)
  }
  @inline final private def custom(layer: LayerLike, i: Int): Col = {
    val cust = custom(layer)
    if (cust.isDefined)
      cust.get(layer, i)
    else
      super.apply(layer, i)
  }

  @inline final private def custom(y: Y): Col = {
    val colfOpt = y.ui.color
    if (colfOpt.isDefined) {
      val colf = colfOpt.get
      if (colf.converted == null)
        colf.converted = Color(colf.r, colf.g, colf.b, colf.a)
      colf.converted.asInstanceOf[Col]
    } else
      null
  }
  @inline final private def custom(layer: LayerLike): Option[YCol] = {
    val custMap = layer.get[YCol](YCOLMAP)
    if (custMap.isDefined)
      custMap
    else {
      val hints = layer.ui.gradient
      if (hints.isDefined) {
        val map = YGradientFX(hints.get)
        layer.update(YCOLMAP, map)
        Some(map)
      } else
        None
    }
  }
}
