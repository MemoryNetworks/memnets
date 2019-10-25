package memnets.model

import java.util.function.Consumer

import memnets.utils._

object Param {

  val DEFAULT_NAME = "none"
  val PRECISION = Precision(0.0001)
  // NOTE : if only supply max as negative, will map input max -> real min, 0.0 -> real max
  def apply(
      name: String,
      max: Double,
      init: Double = 0.0,
      min: Double = 0.0,
      precision: Double = 0.01,
      system: Boolean = false
  )(implicit mn: DynamicSystem): Param = {

    val t = mn.params.create(name, system)
    var minEff = min
    var maxEff = max
    if (min.~(0.0)(PRECISION) && max < 0.0) {
      minEff = max
      maxEff = 0.0
    }
    require(minEff <= maxEff, "min > max")
    require(init >= minEff, "init < min")
    require(init <= maxEff, "init > max")
    t.max = maxEff
    t.min = minEff
    t.value = init
    t.precision = precision
    t
  }
}

/**
 * User modifiable parameter.  Typically used to on W.tie or LayerLinks
 *
 * NOTE: all listeners receive the calculated w = { func(value) } not value
 */
trait Param extends Descriptable with Comparable[Param] with Dsl {
  def desc: String
  def desc_=(desc: String): Unit
  override def description: String = s"parameter $name: ${desc ? "no description"}"

  /** function applied to value before being applied */
  def func: FuncD
  def func_=(f: FuncD): Unit
  def name: String
  def max: Double
  def max_=(d: Double): Unit
  def min: Double
  def min_=(d: Double): Unit

  /** raw value does NOT have func applied. */
  def value: Double
  def value_=(d: Double): Unit

  /**
   * for precision > 0.0, value is rounded by (1.0/precision * value) / (1.0/precision)
   * any change less than precision will not be broadcast to tied weights
   * too small of a value could cause excessive/costly modifications
   * default value is 0.01
   */
  def precision: Double
  def precision_=(sen: Double): Unit
  def system: Boolean

  /** @return func.eval(value) */
  def w: Double = func.eval(value)
  def ==>(listener: Double => Unit): Subscribed = {
    this.onChange { (_, w) =>
      listener(w)
    }
  }
  def onChange(listener: (Double, Double) => Unit): Subscribed
  def addListener(f: Consumer[Double]): Subscribed = { ==>(f.accept) }
  def addChangeListener(f: Consumer[(Double, Double)]): Subscribed = { onChange(f.accept(_, _)) }
  def getDesc: String = desc
  def setDesc(value: String): Unit = { desc = value }
  def getFunc: FuncD = func
  def setFunc(df: FuncD): Unit = func = df
  def getName: String = name
  def getMin: Double = min
  def setMin(d: Double): Unit = { min = d }
  def getMax: Double = max
  def setMax(d: Double): Unit = { max = d }
  def getPrecision: Double = precision
  def setPrecision(d: Double): Unit = { precision = d }
  def getValue: Double = value
  def setValue(f: Double) = value = f
  def getW: Double = w
  def compareTo(other: Param) = name.compareTo(other.name)
}
