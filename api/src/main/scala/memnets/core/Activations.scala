package memnets.core

import memnets.model._

import scala.collection.mutable.ArrayBuffer

object Activations extends Logging {
  import Config._

  def apply(name: String): ActivationFactory = _activations.find(_.name == name).get
  /** NOTE: using known ref to method for comparisons */
  val LINEAR = linear(_)
  @inline def linear(d: Double): Double = d
  @inline def relu(d: Double): Double = if (d < 0.0) 0.0 else d
  @inline def sigmoid(d: Double): Double = 1.0 / (1.0 + Math.exp(-1.0 * d))
  private val _activations = new ArrayBuffer[ActivationFactory]()
  _activations += ActivationFactory(RELU_MAX) { max =>
    logger.debug("relumax: " + max)
    (d: Double) =>
      if (d > max) max else if (d < 0.0) 0.0 else d
  }
  _activations += ActivationFactory(SIGMOID_KNEE) { knee => (d: Double) =>
    1.0 / (1.0 + Math.exp(-knee * d))
  }
}

case class ActivationFactory(name: String)(val create: Double => OP)
