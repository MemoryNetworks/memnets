package memnets.model.impl

import breeze.linalg._
import breeze.stats.distributions.Gaussian
import memnets.linalg.CooMatrix
import memnets.model._
import memnets.utils.Subscribed

private[model] abstract class TiedImpl extends Logging {
  private var _w: Double = 1.0
  private var _tie: Option[TieType] = None
  private var _sub: Option[Subscribed] = None
  def w = _w
  def w_=(f: Double): Unit = { _w = f }
  def tie = _tie
  def tie_=(optT: Option[TieType]): Unit = {
    for (sub <- _sub)
      sub.cancel()
    for (t <- optT) {
      w = t.getValue
      _sub = Some(t ==> { wgt =>
        this.w = wgt
      })
    }
    _tie = optT
  }
}
private[model] sealed class DotTiedImpl(val src: AbstractLayer, val tgt: Y) extends TiedImpl with DotTied {
  val srcRange = new LinkRange(src)
}
private[model] sealed class OnetoOneImpl(val src: AbstractLayer, val tgt: Tgt) extends TiedImpl with OnetoOne {
  type S = AbstractLayer
  require(
    src.length == tgt.length,
    s"Layer[name = ${src.name}, n = ${src.length}] --> Layer[name = ${tgt.name}, n = ${tgt.length}] must have same length. ")
  if (src == tgt)
    logger.warn("use decay on layer instead of edge")

  // NOTE : softmax is strange exception here, so test if LayerImpl (should re-eval)
  for (tgt <- tgt.as[LayerImpl])
    tgt.outLinks += this // technically not out, but ok
}
private[memnets] sealed class DVLikeImpl(val length: Int) extends DenseVectorLike {
  val _w = DenseVector.zeros[Double](length)
  def clear(): Unit = _w.clear()
  def apply(i: Int): Double = _w(i)
  def update(i: Int, v: Double): Unit = _w(i) = v
  def random(): this.type = {
    _w := (DenseVector.rand[Double](size = length, rand = Gaussian(0.0, 1.0)))
    this
  }
  def toDenseVector(other: DenseVector[Double] = null) = {
    if (other == null)
      _w
    else {
      require(other.length == length)
      other := _w
      other
    }
  }
  def :=(other: DenseVector[Double]): this.type = {
    if (!(_w eq other))
      _w := other
    this
  }
}
private[memnets] sealed class DVFLikeImpl(val length: Int) extends DenseVectorLike {
  val _w = DenseVector.zeros[Float](length)
  def clear(): Unit = _w.clear()
  def apply(i: Int): Double = _w(i)
  def update(i: Int, v: Double): Unit = _w(i) = v.asInstanceOf[Float]
  def random(): this.type = {
    _w := (DenseVector.rand[Float](size = length, rand = Gaussian(0.0, 1.0)))
    this
  }
  def toDenseVector(other: DenseVector[Double] = null): DenseVector[Double] = {
    val data = if (other == null) DenseVector.zeros[Double](size = length) else other
    require(other.length == length)
    var i = 0
    val len = length
    while (i < len) {
      data(i) = _w(i)
      i += 1
    }
    data
  }
  def :=(dv: DenseVector[Double]): this.type = {
    var i = 0
    val len = dv.data.length
    while (i < len) {
      this(i) = dv(i)
      i += 1
    }
    this
  }
}
private[memnets] sealed class DenseLinkFImpl(val src: AbstractLayer, val tgt: Tgt)
    extends DVFLikeImpl(src.length)
    with DenseLink {
  type S = AbstractLayer
  require(
    src.length == tgt.length,
    s"Layer[name = ${src.name}, n = ${src.length}] -:> Layer[name = ${tgt.name}, n = ${tgt.length}] must have same length. ")
}
private[memnets] sealed class DenseLinkImpl(val src: AbstractLayer, val tgt: Tgt)
    extends DVLikeImpl(src.length)
    with DenseLink {
  type S = AbstractLayer
  require(
    src.length == tgt.length,
    s"Layer[name = ${src.name}, n = ${src.length}] -:> Layer[name = ${tgt.name}, n = ${tgt.length}] must have same length. ")
}
private[model] sealed class LambdaLinkImpl(val src: AbstractLayer, val tgt: Tgt, val lambda: LambdaCalc)
    extends LambdaLink
    with CustomLink
    with Logging {
  if (src.length != tgt.length)
    logger.warn(
      s"Layer[name = ${src.name}, n = ${src.length}] -:> Layer[name = ${tgt.name}, n = ${tgt.length}] could be an issue if not expert")
  def dxF(out: Array[DenseVector[Float]], x: Array[DenseVector[Float]]): Unit = {
    val odata = out(tgt.id).data
    val data = x(src.id).data
    lambda.dxF(odata, data)
  }
  def dx(out: Array[DenseVector[Double]], x: Array[DenseVector[Double]]): Unit = {
    val odata = out(tgt.id).data
    val data = x(src.id).data
    lambda.dx(odata, data)
  }
}
private[model] sealed class DecayImpl(val src: Layer, val tgt: Layer) extends TiedImpl with Decay {
  require(src == tgt)
  logger.trace(s"Decay: $src")
  override def w_=(f: Double): Unit = {
    src.decay = f
    super.w_=(f)
  }
}
private[memnets] sealed class DotFImpl(val src: AbstractLayer, val tgt: Y) extends DVFLikeImpl(src.length) with Dot
private[memnets] sealed class DotImpl(val src: AbstractLayer, val tgt: Y) extends DVLikeImpl(src.length) with Dot

private[model] sealed class Conv1DImpl(
    val src: AbstractLayer,
    val tgt: AbstractLayer,
    val kernel: Array[Double],
    val ktype: KernelType
) extends Conv1D {
  require(kernel.length % 2 == 1, "supports only odd-sized kernels")
  require(kernel.length < src.length, s"kernel[n=${kernel.length}] is too large for array[n=${src.length}]")
}

private[model] sealed class DivergeImpl(val src: Y, val tgt: AbstractLayer) extends Diverge {
  val w = DenseVector.zeros[Double](size = tgt.length)
  tgt.asInstanceOf[LayerImpl].outLinks += this // technically not out, but ok

}
private[model] sealed class DivergeTiedImpl(val src: Y, val tgt: AbstractLayer) extends TiedImpl with DivergeTied {
  tgt.asInstanceOf[LayerImpl].outLinks += this // technically not out, but ok
  val tgtRange = new LinkRange(tgt)
}
private[memnets] sealed class MatMulFImpl(val src: AbstractLayer, val tgt: Tgt) extends MatMul with Logging {
  import memnets.model.DenseMatrixFExt
  val _w = DenseMatrix.zeros[Float](rows = tgt.length, cols = src.length)
  def w = _w
  def w_=(other: DenseMatrix[Float]): this.type = {
    if (_w != other) {
      assert(other.rows == _w.rows, s"rows mismatch : needed ${_w.rows}, found ${other.rows}")
      assert(other.cols == _w.cols, s"cols mismatch : needed ${_w.cols}, found ${other.cols}")
      _w := other
    }
    this
  }
  def clear() = _w.clear()
  def apply(i: Int, j: Int): Double = _w(i, j)
  def update(i: Int, j: Int, v: Double): Unit = _w(i, j) = v.asInstanceOf[Float]
}
private[memnets] sealed class MatMulDImpl(val src: AbstractLayer, val tgt: Tgt) extends MatMul {
  val _w: DenseMatrix[Double] = DenseMatrix.zeros[Double](rows = tgt.length, cols = src.length)
  def w = {
    val copy = DenseMatrix.zeros[Float](rows = tgt.length, cols = src.length)
    // column major...
    var i, j = 0
    while (j < _w.cols) {
      i = 0
      while (i < _w.rows) {
        copy(i, j) = _w(i, j).asInstanceOf[Float]
        i += 1
      }
      j += 1
    }
    copy
  }
  def w_=(other: DenseMatrix[Float]): this.type = {
    assert(other.rows == _w.rows, s"rows mismatch : needed ${_w.rows}, found ${other.rows}")
    assert(other.cols == _w.cols, s"cols mismatch : needed ${_w.cols}, found ${other.cols}")
    // column major...
    var i, j = 0
    while (j < _w.cols) {
      i = 0
      while (i < _w.rows) {
        _w(i, j) = other(i, j)
        i += 1
      }
      j += 1
    }
    this
  }
  def clear() = _w.clear()
  def apply(i: Int, j: Int): Double = _w(i, j)

  /** NOTE: intuitively, seems like 0 <= i < src.length,
   * but remember each matrix row multiplies against src
   * so 0 <= i < tgt.length and 0 <= j < src.length
   * */
  def update(i: Int, j: Int, v: Double) = _w(i, j) = v
}
private[memnets] sealed class SparseLinkFImpl(val src: AbstractLayer, val tgt: Tgt) extends SparseLink {
  val _w = CooMatrix.zeros[Float](rows = tgt.length, cols = src.length)
  var _scale = 1.0f // used by certain impls.  not exposed to public api
  def activeSize: Int = _w.activeSize
  def apply(i: Int, j: Int): Double = _w(i, j)
  def update(i: Int, j: Int, v: Double) = _w(i, j) = v.asInstanceOf[Float]
  override def optimize(): Unit = { _w.sort() }
}
private[memnets] sealed class SparseLinkDImpl(val src: AbstractLayer, val tgt: Tgt) extends SparseLink {
  val _w = CooMatrix.zeros[Double](rows = tgt.length, cols = src.length)
  def activeSize: Int = _w.activeSize
  def apply(i: Int, j: Int): Double = _w(i, j)
  def update(i: Int, j: Int, v: Double) = _w(i, j) = v
  override def optimize(): Unit = { _w.sort() }
}
