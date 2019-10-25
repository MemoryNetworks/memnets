package memnets.awt

import java.awt.Color

import memnets.model.YRange
import memnets.ui.{ColorMap, ColorMapBase}

object HeatMapAwt {
  val midColor = Color.getHSBColor(230.0f / 360.0f, 1.0f, 1.0f)
  val darkColor = Color.MAGENTA.darker().darker().darker()
  def apply(max: Double = YRange.scale, numColors: Int = 256): ColorMap[Col] =
    new ColorMapBase(max, numColors)(color)
  def color(i: Double) = {
    if (i >= 0.5)
      Color.getHSBColor(((2.0 * (1.0 - i) * 230.0).asInstanceOf[Float]) / 360.0f, 1.0f, 1.0f)
    else
      darkColor // todo: hack
  }
}
