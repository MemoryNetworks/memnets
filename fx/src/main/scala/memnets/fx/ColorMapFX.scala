package memnets.fx

import memnets.model._
import memnets.ui.ColorMapBase
import scalafx.scene.paint.Color
// don't import at this scope import scalafx.scene.paint.Color

object HeatMapFX {
  val midColor = Color.hsb(230.0, 1.0, 1.0, 1.0)
  val darkColor = Color.DarkViolet.interpolate(Color.Black, 0.9)
  def apply(max: Double = YRange.scale, numColors: Int = 256): ColMap = new ColorMapFX(max, numColors)(color)
  private def color(i: Double): Col = {
    if (i >= 0.5)
      Color.hsb(2.0 * (1.0 - i) * 230.0, 1.0, 1.0, 1.0)
    else
      midColor.interpolate(darkColor, 1.0 - 2.0 * i)
  }
}
object OpacityMapFX {
  def apply(
      max: Double = YRange.scale,
      numColors: Int = 256,
      negColor: Color = Color.DarkOrchid,
      posColor: Color = Color.DeepPink,
      midColor: Color = Color.Transparent
  ): ColMap = new ColorMapFX(max, numColors)(color(_, negColor, posColor, midColor))

  def color(
      i: Double,
      negColor: Color = Color.DarkOrchid,
      posColor: Color = Color.DeepPink,
      midColor: Color = Color.Transparent
  ): Col = {

    if (i >= 0.5) midColor.interpolate(posColor, 2.0 * (i - 0.5))
    else midColor.interpolate(negColor, 2.0 * (0.5 - i))
  }
}
object GrayMapFX {
  def apply(max: Double = YRange.scale, numColors: Int = 256): ColMap = new ColorMapFX(max, numColors)(color)
  import scalafx.scene.paint.Color
  private def color(i: Double): Col = { Color.gray(i) }
}
object InvertedGrayMapFX {
  def apply(max: Double = YRange.scale, numColors: Int = 256): ColMap = new ColorMapFX(max, numColors)(color)
  import scalafx.scene.paint.Color
  private def color(i: Double): Col = { Color.gray(1.0 - i) }
}

sealed class ColorMapFX(
    override val max: Double = YRange.scale,
    override val numColors: Int = 256,
    override val min: Option[Double] = None
)(colGen: Double => Col)
    extends ColorMapBase[Col](max, numColors, min)(colGen)
    with ColMap
    with Logging
