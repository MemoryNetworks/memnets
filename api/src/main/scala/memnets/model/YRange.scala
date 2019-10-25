package memnets.model

object YRange {
  val scale: Double = 10.0
  val scaleF: Float = scale.toFloat

  def apply(): YRange = YRange(min = -YRange.scale, max = YRange.scale)
}
object NormalizedRange extends YRange(0.0, 1.0)
sealed case class YRange(min: Double, max: Double) {
  def range: Double = max - min
  def isMinZero: Boolean = min == 0.0
}
