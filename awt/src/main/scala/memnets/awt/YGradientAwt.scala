package memnets.awt

import java.awt.Color

import memnets.model.GradientHints
import memnets.ui.YGradientMap

case class YGradientAwt(var hints: GradientHints = GradientHints()) extends YGradientMap[Color] {
  def apply(i: Int, length: Int): Col = {
    val hintz = hints
    import hintz._
    val maxlen = Math.min(length, maxLength)
    Color.getHSBColor(
      ((hue + i * (spectrum / Math.max(minDivs, maxlen))) % spectrum)
        .asInstanceOf[Float] / 360.0f,
      saturation,
      brightness)
  }
}
