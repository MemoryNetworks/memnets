package memnets.ml

import java.util.Scanner

import memnets.utils._

import scala.collection.mutable.ArrayBuffer

trait Feature {
  type T
  def index: Int
  def name: String
  def raw(data: Row): T

  /**
   *  NOTE: in case of discrete, overrides to be data(index) which should be 0.0-1.0
   */
  def normalized(data: Row): Double

  /**
   *  largely way to use Discrete in same way as Continuous
   *  Discrete overrides to return 1.0 if j matches raw index value
   */
  def apply(data: Row, j: Int = 0): Double = normalized(data)

  /**
   *  0 for Output
   *  1 for Continuous
   *  n for Discrete (n = # of categories)
   */
  def dim: Int
  def distance(test: Row, train: Row): Double
  def isOutput: Boolean

  /**
   *  should produce value returned by normalized
   */
  def parse(s: String): Double
}

object Feature {
  val discrete = (raw"(\w*)[ \t]+(discrete)[ \t]+([ \t\w,]+)" r).pattern
  val cont = (raw"(\w*)[ \t]+(continuous)[ \t]+(-?\d+.?\d*|-?.\d+)[ \t]*,[ \t]*(-?\d+.?\d*|-?.\d+)" r).pattern
  val output = (raw"(\w*)[ \t]+(output)[ \t]+(\w+)[ \t]*,[ \t]*(\w+)".r).pattern
  val disTest = discrete.asPredicate
  val contTest = cont.asPredicate
  val outTest = output.asPredicate
  def read(file: String): Features = {
    def parse(index: Int, line: String): Option[Feature] = {
      line match {
        case c: String if contTest.test(c) =>
          val m = cont.matcher(line)
          m.find()
          Some(ContFeature(index, m.group(1), m.group(3).toDouble, m.group(4).toDouble))
        case c: String if disTest.test(c) =>
          val m = discrete.matcher(line)
          m.find()
          Some(DiscFeature(index, m.group(1), m.group(3).split(",").map(_.trim)))
        case c: String if outTest.test(c) =>
          val m = output.matcher(line)
          m.find()
          Some(Output(index, m.group(1), m.group(3), m.group(4)))
        case default =>
          None
      }
    }
    val features = ArrayBuffer[Feature]()
    val scan = new Scanner(file.asStream)
    while (scan.hasNextLine) {
      for (f <- parse(features.length, scan.nextLine))
        features += f
    }
    scan.close()
    require(features.size > 0, s"no features in : $file")
    val array: Array[Feature] = features.toArray
    array
  }
}
sealed case class DiscFeature(index: Int, name: String, values: Array[String]) extends Feature {
  type T = String
  require(values.length > 1, "must have multiple values")
  private val _sizeD: Double = values.length - 1.0
  private[memnets] var _posIndex = -1

  def isOutput = _posIndex >= 0
  def isPos(data: Row) = valueIndex(data) == _posIndex
  def parse(s: String) = {
    val i = values.indexOf(s)
    require(i >= 0)
    i.toDouble / _sizeD
  }
  override def dim: Int = values.length
  def valueIndex(data: Row): Int = (data(index) * _sizeD).toInt
  def raw(data: Row): String = values(valueIndex(data))
  override def apply(data: Row, j: Int): Double = { if (j == valueIndex(data)) 1.0 else 0.0 }
  def normalized(data: Row): Double = data(index)
  def distance(test: Row, train: Row): Double = if (test(index) == train(index)) 0.0 else 1.0
}
object Output {
  def apply(index: Int, name: String, posCat: String, negCat: String) = {
    val discrete = DiscFeature(index, name, Array(posCat, negCat))
    discrete._posIndex = 0
    discrete
  }
}
sealed case class ContFeature(index: Int, name: String, min: Double, max: Double) extends Feature {
  type T = Double
  val range: Double = max - min

  def isOutput: Boolean = false
  def parse(s: String): Double = {
    val i = s.toDouble
    require(min <= i && i <= max, f"error: out of bounds for $name : $i%.3f")
    i
  }
  def dim: Int = 1
  @inline final def raw(data: Row): Double = data(this.index)
  def normalized(data: Row): Double = (raw(data) - min) / range

  /**
   * NOTE : can be + or -, client must use as needed
   */
  def distance(test: Row, train: Row): Double = normalized(test) - normalized(train)
}

class ExemplarFeature(
    override val index: Int,
    override val name: String,
    val exemplar: Row,
    val features: Features
) extends ContFeature(index, name, min = 0.0, max = 1.0) {

  import Math._
  private var _sigma = 1.0
  private var _sigma_sq = 1.0

  override def parse(s: String): Double = ???
  override def distance(test: Row, train: Row): Double = ???
  final def dist(point: Row): Double = {
    var sum = 0.0
    var i = 0
    val len = features.length
    while (i < len) {
      val dist = features(i).distance(point, exemplar)
      sum += (dist * dist)
      i += 1
    }
    sum
  }
  override def apply(pt: Row, j: Int) = {
    exp(-dist(pt) / _sigma_sq)
  }
  def sigma: Double = _sigma
  def sigma_=(sig: Double): Unit = {
    _sigma = sig
    _sigma_sq = sig * sig
  }
  sigma = 3.0
}

sealed class BinFeature(override val index: Int) extends DiscFeature(index, "b" + index, Array("on", "off"))
