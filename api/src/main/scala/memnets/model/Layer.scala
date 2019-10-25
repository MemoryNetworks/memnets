package memnets.model

import breeze.linalg.DenseVector
import memnets.model.impl._
import memnets.utils._

trait Layer extends AbstractLayer with Config {
  def decay: Double
  def decay_=(v: Double): Unit

  /**
   * NOTE : only used if Decay link ( this --> this ) present.
   * also, Decay.w auto updates Layer.decay but not vice versa
   */
  def bias: Option[DenseVector[Double]]
  def bias_=(b: DenseVector[Double]): Unit
  override def toString = s"Layer[name=$name, n=$length, data=${prettyPrint()}]"
}

/** NOTE : the DSL does not stop links TO an Input, but they will be ignored */
trait Input extends AbstractLayer {
  def vector: DenseVector[Double]
  def markDirty(): Unit
  def isDirty: Boolean
  override def toString = s"Input[name=$name, n=$length, data=${prettyPrint(16)}]"
}
trait Lambda {
  def dxF(out: Array[Float], x: Array[Float], f: Int => Float): Unit
  def dx(out: Array[Double], x: Array[Double], f: Int => Double): Unit
}
trait LambdaLayer extends AbstractLayer {
  def lambda: Lambda
  override def toString = s"LambdaLayer[name=$name, n=$length, data=${prettyPrint()}]"
}
trait SoftMax extends AbstractLayer {
  override def toString = s"SoftMax[name=$name, n=$length, data=${prettyPrint()}]"
}
trait Dense extends Layer {
  override def toString = s"Dense[name=$name, n=$length, data=${prettyPrint()}]"
}
import memnets.model.VariableType._

object Layer {
  val DECAY_PRECISON = Precision(0.00001)
  import Activation._

  /** has internal state (StringBuilder).  do not share */
  final class DefaultIndexFormat extends IndexFormat {
    val spacer = "="
    val sb = new StringBuilder()
    def format(i: Int, act: Double): String = {
      sb.clear()
      sb.append(i)
      sb.append(spacer)
      sb.append(TwoDigits.format(act))
      sb.toString()
    }
  }
  val labelFormat = "%s=%.2f"
  @inline def labelFormatter(s: String, act: Double): String = labelFormat.format(s, act)

  /** @param tau if (tua == DynamicSystem.TAU_DEFAULT), won't get stored, resulting in system.tau */
  def apply(
      n: Int,
      name: String = EMPTY_STRING,
      decay: Double = 0.0,
      tau: Double = DynamicSystem.TAU_DEFAULT,
      act: Activation = Linear,
      threshold: Double = 0.0,
      scale: Double = YRange.scale,
      numericalType: VariableType = VariableType.Continuous
  )(implicit sys: DynamicSystem): Layer = {

    val lay: Layer = new LayerImpl(n.toEven, sys)
    lay.name = name
    // threshold is part of DecayEdge, so need lay --> lay even if 0.0
    if (threshold != 0.0) {
      lay.threshold = threshold
      lay --> lay w = decay
    } else if (decay != 0.0)
      lay --> lay w = decay

    if (tau != DynamicSystem.TAU_DEFAULT) lay.tau = tau
    if (scale != YRange.scale) lay.ui.scale = scale
    if (act != Linear) lay.activation = act
    lay.ui.numericalType = numericalType

    lay
  }
}
object Dense {
  import Activation._
  def apply(
      n: Int,
      bias: LayerInit = ZEROS,
      act: Activation = Relu,
      name: String = EMPTY_STRING,
      scale: Option[Double] = None
  )(implicit sys: DynamicSystem): Layer = {

    val lay = new DenseImpl(n.toEven, sys)
    lay.activation = act
    lay.name = name
    lay.tau = 10.0f // could be global default
    lay.bias = bias(lay.length)

    lay.ui.numericalType = Discrete
    for (sc <- scale) lay.ui.scale = sc
    lay.ui.plot.range = lay.ui.rangeDefault
    lay
  }
}
object SoftMax {
  def apply(input: AbstractLayer, name: String = "")(implicit sys: DynamicSystem): SoftMax = {
    val lay = new SoftMaxImpl(input.length.toEven, sys)
    lay.name = name
    input --> lay

    lay.ui.numericalType = Discrete
    lay.ui.normalized = true
    lay.ui.plot.width = input.ui.plot.width
    lay.ui.plot.height = input.ui.plot.height
    lay
  }
}
object LambdaLayer {
  def apply[T <% Lambda](
      n: Int,
      name: String
  )(f: T)(implicit sys: DynamicSystem): LambdaLayer = {

    val lay = new LambdaLayerImpl(n.toEven, sys, f)
    lay.name = name
    lay
  }
  def apply(
      n: Int,
      f: Double => Double,
      name: String = "lambda"
  )(implicit sys: DynamicSystem): LambdaLayer = {

    apply(n, name)(f)
  }
}
object Input {
  def apply(
      n: Int,
      name: String = "",
  )(implicit sys: DynamicSystem): Input = {

    InputP(n, name, VariableType.Discrete, normalized = true)
  }
}
object InputP {
  def apply(
      n: Int,
      name: String = "",
      numericType: Option[VariableType] = None,
      normalized: Boolean = false,
      init: LayerInit = ZEROS
  )(implicit sys: DynamicSystem): Input = {

    val n2 = n.toEven
    val lay = new InputImpl(n2, sys, init(n2))
    lay.name = name

    if (normalized) lay.ui.normalized = normalized
    lay.ui.numericalType = numericType.getOrElse(Discrete)
    lay
  }
}
