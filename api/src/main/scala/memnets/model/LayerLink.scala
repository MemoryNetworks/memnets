package memnets.model

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.distributions._
import memnets.model.impl.Conv1DImpl

/**
 * at least one src or tgt should be a layer
 * NOTE: for Y --> Y, uses separate native only W in linalg
 */
trait LayerLink {
  type S
  type T
  def src: S
  def tgt: T

  /** called in sim after all weights are defined. can compact/sort/??? */
  def optimize(): Unit = {}
}

/** can use w w/o tie, but using tie will set w  */
trait TiedLink extends LayerLink {
  def tie: Option[Param]
  def tie_=(t: Option[Param]): Unit
  def w: Double
  def w_=(d: Double): Unit

  def getW: Double = w
  def setW(weight: Double): this.type = {
    w = weight
    this
  }
  def getTie: Option[Param] = tie
  def setTie(param: Option[Param]): this.type = {
    tie = param
    this
  }
}

/**
 * the inverse of Dot,
 * i.e., multiply each entry in w by Y.act then add to corr index in Layer
 */
trait Diverge extends LayerLink {
  type S = Y
  type T = AbstractLayer
  def w: DenseVector[Double]
}

trait Conv1D extends LayersLink {
  type S = AbstractLayer
  def kernel: Array[Double]
  def ktype: KernelType
}
object Conv1D {
  def apply(
      src: Tgt,
      kernel: Array[Double],
      tgt: Tgt,
      ktype: KernelType = KernelType.Wrap
  ): Conv1D = {
    src.system.addLink(new Conv1DImpl(src, tgt, kernel, ktype), srcOut = false)
  }
}

/** useful for models such as SoftWTALayer */
trait DivergeTied extends TiedLink {
  type S = Y
  type T = AbstractLayer

  /** defaults to full layer, but can modify for subrange */
  def tgtRange: LinkRange
}
trait Dot extends LayerLink with DenseVectorLike {
  type S = AbstractLayer
  type T = Y
}

/** useful for models such as SoftWTALayer */
trait DotTied extends TiedLink {
  type S = AbstractLayer
  type T = Y

  /** defaults to full layer, but can modify for subrange */
  def srcRange: LinkRange
}
trait LayersLink extends LayerLink {
  type S <: AbstractLayer
  type T = Tgt
}
trait OnetoOne extends LayersLink with TiedLink
trait Decay extends OnetoOne {
  type S = Layer
}
trait DenseLink extends LayersLink with DenseVectorLike

trait MatMul extends LayersLink {
  type S = AbstractLayer
  def apply(i: Int, j: Int): Double
  def update(i: Int, j: Int, v: Double): Unit
  def edge(src: Int, tgt: Int, w: Double): Unit = update(tgt, src, w)

  def clear(): Unit
  def random(rand: Rand[Float] = Gaussian(0.0, 1.0)): this.type = {
    w = DenseMatrix.rand[Float](rows = tgt.length, cols = src.length, rand = rand)
  }

  /** WARNING: impl may or may not be a COPY!!!.  you must set w after modification  */
  def w: DenseMatrix[Float]

  /** @param other impl may or may not just COPY values and not keep reference */
  def w_=(other: DenseMatrix[Float]): this.type
}
trait SparseLink extends LayersLink {
  type S = AbstractLayer
  def activeSize: Int
  def apply(i: Int, j: Int): Double
  def update(i: Int, j: Int, v: Double): Unit
  def edge(src: Int, tgt: Int, w: Double): Unit = update(tgt, src, w)
}

class LinkRange private[memnets] (layer: AbstractLayer) {
  var _from = 0
  var _until = layer.length
  def contains(i: Int): Boolean = i >= _from && i < _until
  def from = _from
  def until = _until
  def modify(from: Int, until: Int): Unit = {
    require(from < until, "from < until")
    require(until <= layer.length, "until <= length")
    require((until - from) % 2 == 0, "range must be even")
    _from = from
    _until = until
  }
}

trait LambdaLink extends LayersLink {
  type S = AbstractLayer
  def lambda: LambdaCalc
}
trait LambdaCalc {
  def name: String = ""
  def dxF(out: Array[Float], x: Array[Float]): Unit
  def dx(out: Array[Double], x: Array[Double]): Unit
}
trait CustomLink extends LayerLink {
  def dxF(out: Array[DenseVector[Float]], x: Array[DenseVector[Float]]): Unit
  def dx(out: Array[DenseVector[Double]], x: Array[DenseVector[Double]]): Unit
}
