package memnets.model

import memnets.utils._

import scala.collection.mutable.ArrayBuffer

object Config {
  val ONE = Some(1.0f)
  private val _validKeys = new ArrayBuffer[String]()
  val THRES: String = registerKey("thres")
  val TAU: String = registerKey("tau")
  val RELU_MAX: String = registerKey("relu_max")
  val SIGMOID_KNEE: String = registerKey("sigmoid_knee") // value = knee
  val SPIKE: String = registerKey("spike")
  val NOISE_SCALE: String = registerKey("noise")
  val CUSTOM: String = registerKey("custom") // need to set to any value if custom out in builder
  val SCALE: String = registerKey("scale")
  val MIN_ZERO: String = registerKey("min_zero")
  val NORMALIZED: String = registerKey("normalized")
  val SHOW_TEXT: String = registerKey("show_text")
  val ACTIVATION: String = registerKey("activation")
  val OUT_FUNC: String = registerKey("out_func")
  val GRAD_HINTS = "GRAD_HINTS" // only for Layerlike, no register
  val GRID_HINTS = "GRID_HINTS" // only for Layerlike, no register
  val COLF = "COLF"

  /** todo: capture type info */
  def registerKey(key: String) = { _validKeys += key; key }
}

trait Config extends ConfigMap {
  import Config._

  def activation: Activation
  def activation_=(a: Activation): Unit
  def noiseScale: Double = applyD(NOISE_SCALE, 1.0)
  def noiseScale_=(d: Double): Unit = { update(NOISE_SCALE, d) }
  def relumax: Option[Float] = get[Float](RELU_MAX)
  def relumax_=(v: Double): Unit = { update(RELU_MAX, v) }
  def sigmoidKnee: Option[Float] = get[Float](SIGMOID_KNEE)
  def sigmoidKnee_=(v: Double): Unit = { update(SIGMOID_KNEE, v) }
  def tau: Double
  def tau_=(v: Double): Unit = { update(TAU, v) }
  def threshold = applyD(THRES, 0.0)
  def threshold_=(v: Double): Unit = { update(THRES, v) }
  // Java
  def getActivation: Activation = activation
  def setActivation(a: Activation): Unit = activation = a
  def getNoiseScale: Double = noiseScale
  def setNoiseScale(f: Float): Unit = noiseScale = f
  def getReluMax: Option[Float] = relumax
  def setReluMax(d: Double): Unit = relumax = d
  def getTau: Double = tau
  def setTau(d: Double): Unit = tau = d
  def getThreshold: Double = threshold
  def setThreshold(d: Double): Unit = threshold = d
}
