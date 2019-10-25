package memnets.ui

import memnets.model._

import scala.reflect.ClassTag

trait ColorMap[T <: AnyRef] {
  def max: Double
  def min: Option[Double]
  def numColors: Int
  def width: Int = Math.sqrt(numColors).toInt
  def height: Int = Math.ceil(numColors / width).toInt

  def colors: Array[T]
  def apply(act: Double): T = apply(act.asInstanceOf[Float])
  def apply(act: Float): T
  def index(act: Double): Int = index(act.asInstanceOf[Float])
  def index(act: Float): Int

  def textureCoords(): Array[Float] = {
    var index = 0
    val texture = Array.ofDim[Float](2 * numColors)
    val w = width
    val h = height
    val wF: Float = w.toFloat
    val hF: Float = h.toFloat
    for {
      y <- 0 until h
      x <- 0 until w
    } {

      texture(index) = x.asInstanceOf[Float] / wF
      texture(index + 1) = y.asInstanceOf[Float] / hF
      index += 2
    }
    texture
  }
}

class ColorMapBase[T <: AnyRef: ClassTag](val max: Double, val numColors: Int = 256, val min: Option[Double] = None)(
    colGen: Double => T)
    extends ColorMap[T]
    with Logging {
  val maxF: Float = max.asInstanceOf[Float]
  val minF: Float = min.getOrElse(-max).asInstanceOf[Float]
  require(maxF > 0.0f)
  require(maxF > minF)
  val rangeF: Float = maxF - minF

  if (width != height)
    logger.warn(s"numColors: $numColors, (w,h) = ($width, $height)")

  private val numColorD = numColors.toDouble
  val colors: Array[T] = Array.tabulate(numColors) { i =>
    val scale = i.toDouble / numColorD
    colGen(scale)
  }
  val lastIndex: Int = numColors - 1

  final def apply(act: Float): T = {
    val j = (((act - minF) / rangeF) * numColors).asInstanceOf[Int]
    if (j > lastIndex)
      colors(lastIndex)
    else if (j < 0)
      colors(0)
    else
      colors(j)
  }
  final def index(act: Float): Int = {
    val j = (((act - minF) / rangeF) * numColors).asInstanceOf[Int]
    if (j > lastIndex)
      lastIndex
    else if (j < 0)
      0
    else
      j
  }
}
