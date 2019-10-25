package memnets.core.impl

import breeze.linalg._
import memnets.core._
import memnets.model._
import memnets.model.impl._

import scala.util.Random

private[core] final class SimImpl(override val system: DynamicSystem, paraThreshold: Int)
    extends SimImplBase(system, paraThreshold)
class SimImplBase(override val system: DynamicSystem, paraThreshold: Int) extends SimBase(system) with Logging {
  type T = Float
  import com.github.fommil.netlib.BLAS.{getInstance => blas}
  import memnets.utils.para._
  implicit val locEC = ec
  // need to be careful here
  @inline implicit final def d2T(d: Double): T = d.asInstanceOf[T]

  val ZERO = 0.0f
  val MIN_VALUE = Float.MinValue
  val NEG_INFINITY = Float.NegativeInfinity
  val ONE = 1.0f
  val NEG_ONE = -1.0f
  val HALF = 0.5f
  val LEAP_DECAY = 0.9995f
  val TWO = 2.0f
  val SIXTH = 1.0f / 6.0f

  object NowTickImpl extends TickImpl {
    val system = _system
    @inline def apply(i: Int) = _sparseData(i)
    @inline def applyF(i: Int) = _sparseData(i)
    @inline def update(i: Int, v: Double) = _sparseData(i) = v

    @inline def apply(id: Int, i: Int): Double = _x(id).data(i)
    @inline def update(id: Int, i: Int, v: Double) = _x(id).data(i) = v

    def calc(i: Int) = _xCalc(0)(i)
    def trialIndex: Int = trial.index
    // DenseVectorLike
    def random(lay: LayerLike): Unit = {
      val dv = _x(lay.id)
      dv := DenseVector.rand[Float](size = lay.length)
    }
    def replace(lay: LayerLike, other: DenseVector[Double]): Unit = {
      val dv = _x(lay.id).data
      require(other.length.toEven == dv.length)
      var i = 0
      val len = other.length
      while (i < len) {
        dv(i) = other(i)
        i += 1
      }
    }
    def toDenseVector(lay: LayerLike, output: DenseVector[Double] = null): DenseVector[Double] = {
      val dv = _x(lay.id).data
      val data = if (null == output) DenseVector.zeros[Double](size = dv.length) else output
      require(data.length == dv.length)
      var i = 0
      val len = data.length
      while (i < len) {
        data(i) = dv(i)
        i += 1
      }
      data
    }
  }
  val nowTick: Tick = NowTickImpl
  val nowTickImpl = NowTickImpl

  // NOTE : don't want to allocate unless needed, so don't use objects...
  protected class EulerImpl extends IterMethod("Euler") {
    val _k1 = zerosStack
    val iterBuffers = Array(_k1)
    val _k1Data = data(_k1)
    def k1F(id: Int, i: Int) = _k1Data(id)(i)
    def iterate(x: STK, in: SIGS, ns: NoiseSampler) = {
      _isLastDx = true
      dx(_k1, x, in, ns)
      step(x, x, k1F, ns)
    }
  }
  protected class Ode23Impl extends IterMethod("Ode23") {
    val _k1 = zerosStack
    val _k2 = zerosStack
    val iterBuffers = Array(_k1, _k2)
    val _k1Data = data(_k1)
    val _k2Data = data(_k2)
    final def k1F(id: Int, i: Int) = _k1Data(id)(i)
    final def kAvg(id: Int, i: Int) = HALF * (_k1Data(id)(i) + _k2Data(id)(i))
    final def iterate(x: STK, in: SIGS, ns: NoiseSampler) = {
      dx(_k1, x, in, ns)
      step(_xTemp, x, k1F, ns)

      _isLastDx = true
      dx(_k2, _xTemp, in, ns)
      step(x, x, kAvg, ns)
    }
  }
  // NOTE : only here for SimDoubleImpl
  protected class Ode45Impl extends IterMethod("Ode45") {
    val _k1 = zerosStack
    val k2 = zerosStack
    val k3 = zerosStack
    val k4 = zerosStack
    val iterBuffers = Array(_k1, k2, k3, k4)
    val _k1Data = data(_k1)
    val _k2Data = data(k2)
    val _k3Data = data(k3)
    val _k4Data = data(k4)

    def k1Half(id: Int, i: Int) = HALF * _k1Data(id)(i)
    def k2Half(id: Int, i: Int) = HALF * _k2Data(id)(i)
    def k3Full(id: Int, i: Int) = _k3Data(id)(i)
    def kAvg(id: Int, i: Int) = SIXTH * (_k1Data(id)(i) + TWO * _k2Data(id)(i) + TWO * _k3Data(id)(i) + _k4Data(id)(i))

    def iterate(x: STK, in: SIGS, ns: NoiseSampler) = {
      dx(_k1, x, in, ns)
      step(_xTemp, x, k1Half, ns)

      dx(k2, _xTemp, in, ns)
      step(_xTemp, x, k2Half, ns)

      dx(k3, _xTemp, in, ns)
      step(_xTemp, x, k3Full, ns)

      _isLastDx = true
      dx(k4, _xTemp, in, ns)
      step(x, x, kAvg, ns)
    }
  }
  protected final class LeapFrogImpl extends IterMethod("LeapFrog") {
    // only impl for sparse (0)
    val iterBuffers = Array[STK]()
    val _k1 = zerosDen()
    val _prev = zerosDen()
    val _x0 = _x(0)
    val _xTemp0 = _xTemp(0)
    val _k1Data = _k1.data
    val _tData = _xTemp0.data
    val size = _sparseData.length
    val tau = ONE / TWO
    assert(tau > 0.0f, "invalid leap tau = " + tau)
    logger.debug("leap frog tau = " + tau)
    override def reset = {
      for (d <- List(_k1, _prev))
        clear(d)
    }
    import memnets.utils._
    // only using in param
    def iterate(x: STK, in: SIGS, ns: NoiseSampler) = {
      // save off past timesteps
      _prev.data.arrayCopy(_tData)
      _sparseData.arrayCopy(_prev.data)

      clear(_k1)
      matrixCalc(_k1, _x0)

      val len = in.length
      var i = 0
      while (i < len) {
        val input = in(i)
        if (input.tgt.layerId == 0)
          _k1Data(input.tgt.id) += input.act.asInstanceOf[T]
        i += 1
      }
      i = 0
      while (i < size) {
        _sparseData(i) = LEAP_DECAY * (TWO * _sparseData(i) + tau * _k1Data(i) - _tData(i))
        i += 1
      }
    }
  }
  protected def custom: IterMethod = ???
  protected def euler: IterMethod = new EulerImpl
  protected def ode23: IterMethod = new Ode23Impl
  protected def ode45: IterMethod = new Ode45Impl
  protected def leap: IterMethod = new LeapFrogImpl

  protected def clear(d: DV): Unit = {
    val values = d.data
    val len = values.length
    val elen = _empty.length
    System.arraycopy(_empty.data, 0, values, 0, if (len <= elen) len else elen)
    if (len > elen) {
      var i = elen
      while (i < len) {
        System.arraycopy(values, 0, values, i, if ((len - i) < i) len - i else i)
        i += i
      }
    }
  }
  protected def data(vecs: STK): Array[ARR] = vecs.map(_.data)
  protected def matrixCalc(out: DV, in: DV) = _matrix * (in.data, out.data)
  protected def zerosDen(size: Int = _n): DV = DenseVector.zeros(size = size)
  protected def zerosSp(size: Int = _n): SV = SparseVector.zeros(size = size)
  protected def weights(link: DenseVectorLike): DV = link.asInstanceOf[DVFLikeImpl]._w

  protected def conv1DCalc(conv: Conv1D) = new LayerLinkCalc {
    import conv._
    val klen = kernel.length
    val khalf: Int = (klen - 1) / 2
    val len = src.length
    val clen = len + klen - 1
    val calc = new Array[Float](clen)
    val zeros = new Array[Float](khalf)
    def calc(outStk: STK, xStk: STK): Unit = {
      val out = outStk(tgt.id).data
      val x = xStk(src.id).data
      require(len == x.length, s"expected $len != input ${x.length}")
      if (ktype == KernelType.Wrap) {
        Array.copy(x, len - 1 - khalf, calc, 0, khalf)
        Array.copy(x, 0, calc, khalf, len)
        Array.copy(x, 0, calc, clen - 1 - khalf, khalf)
      } else {
        Array.copy(zeros, 0, calc, 0, khalf)
        Array.copy(x, 0, calc, khalf, len)
        Array.copy(zeros, 0, calc, clen - 1 - khalf, khalf)
      }
      val klen2 = klen - 1
      var i = 0
      while (i < len) {
        var j = 0
        var acc = 0.0
        while (j < klen) {
          acc += kernel(klen2 - j) * calc(i + j)
          j += 1
        }
        out(i) += acc
        i += 1
      }
    }
  }
  protected def decayCalc(dec: Decay, out: STK, x: STK): Unit = {
    val odata = out(dec.tgt.id).data
    val data = x(dec.src.id).data
    val w: T = dec.w
    if (dec.src.bias.isDefined) {
      val bias = dec.src.bias.get.data
      val len = bias.length
      var i = 0
      while (i < len) {
        odata(i) += w * data(i) - bias(i)
        i += 1
      }
    } else {
      val thres: T = dec.src.threshold
      if (w != ZERO || thres != ZERO) {
        val len = data.length
        var i = 0
        while (i < len) {
          odata(i) += w * data(i) - thres
          i += 1
        }
      }
    }
  }
  protected def denseCalc(den: DenseLink, out: STK, x: STK): Unit = {
    val odata = out(den.tgt.id).data
    val data = x(den.src.id).data
    val w = weights(den).data
    val len = data.length
    var i = 0
    while (i < len) {
      odata(i) += w(i) * data(i)
      i += 1
    }
  }

  protected def divCalc(div: DivergeTied, out: STK, x: STK): Unit = {
    val off: T = div.w * x(0)(div.src.id)
    if (off != ZERO) {
      val odata = out(div.tgt.id).data
      val len = div.tgtRange.until
      var i = div.tgtRange.from
      while (i < len) {
        odata(i) += off
        i += 1
      }
    }
  }
  protected def divergeCalc(div: Diverge, out: STK, x: STK): Unit = {
    val act = x(0)(div.src.id)
    if (act != ZERO) {
      val odata = out(div.tgt.id).data
      val w = div.w.data
      val len = odata.length
      var i = 0
      while (i < len) {
        odata(i) += act * w(i)
        i += 1
      }
    }
  }
  protected def dotCalc(dot: Dot, out: STK, x: STK): Unit = {
    val data = x(dot.src.id).data
    val w = weights(dot).data
    val len = data.length
    var sum, sum2 = 0.0
    var i = 0
    while (i < len) {
      sum += data(i) * w(i)
      sum2 += data(i + 1) * w(i + 1)
      i += 2
    }
    out(0)(dot.tgt.id) += (sum + sum2)
  }
  protected def dotTiedCalc(dott: DotTied, out: STK, x: STK): Unit = {
    val xdata = x(dott.src.id).data
    var s, s2 = ZERO
    val len = dott.srcRange.until
    var i = dott.srcRange.from
    while (i < len) {
      s += xdata(i)
      s2 += xdata(i + 1)
      i += 2
    }
    out(0)(dott.tgt.id) += dott.w * (s + s2)
  }
  protected def oneToOneCalc(rep: OnetoOne, out: STK, x: STK): Unit = {
    val odata = out(rep.tgt.id).data
    val data = x(rep.src.id).data
    val w: T = rep.w
    val len = data.length
    var i = 0
    while (i < len) {
      odata(i) += w * data(i)
      i += 1
    }
  }
  protected def customCalc(cus: CustomLink, out: STK, x: STK) = cus.dxF(out, x)
  protected def matMulCalc(mat: MatMul, out: STK, x: STK): Unit = {
    import memnets.model.impl.MatMulFImpl
    val a = mat.asInstanceOf[MatMulFImpl]._w
    val b = x(mat.src.id)
    val rv = out(mat.tgt.id)
    require(a.cols == b.length, "Dimension mismatch!")

    blas.sgemv(
      a.transposeString,
      if (a.isTranspose) a.cols else a.rows,
      if (a.isTranspose) a.rows else a.cols,
      ONE,
      a.data,
      a.offset,
      a.majorStride,
      b.data,
      b.offset,
      b.stride,
      ONE,
      rv.data,
      rv.offset,
      rv.stride
    ) // rv += a * b
  }
  protected def sparseCalc(spa: SparseLink, out: STK, x: STK): Unit = {
    import memnets.model.impl.SparseLinkFImpl
    val a = spa.asInstanceOf[SparseLinkFImpl]._w
    val b = x(spa.src.id).data
    val res = out(spa.tgt.id).data
    a * (b, res)
  }

  /** DO NOT MODIFY x or off INPUTS!!! */
  protected def dx(out: STK, x: STK, in: SIGS, ns: NoiseSampler): Unit = {
    _xCalc = x
    val out0 = out(0)
    val odata = out0.data
    clear(out0)

    var i = 0
    val len = in.length
    while (i < len) {
      val input = in(i)
      if (input.on != 0 && input.tgt.layerId == 0 && input.isActive)
        odata(input.tgt.id) += input.act.asInstanceOf[T]
      i += 1
    }

    if (_T.activeSize > 0)
      out0 += _T

    matrixCalc(out0, x(0))

    // sparse noise
    // TODO : scale with sqrt (time step * variance)
    val q = ns.Q
    if (q > ZERO) {
      var qIndex = Random.nextInt(_randoms.length)
      val qlen = _count
      i = 0
      while (i < qlen) {
        odata(i) += q * _randoms(qIndex) * _noiseScale(i)
        i += 1
        qIndex += 1
        if (qIndex == _randoms.length) qIndex = 0
      }
    }
    val funcs = system.sparse.funcs
    val flen = funcs.length
    i = 0
    while (i < flen) {
      val uf = funcs(i)
      odata(uf.owner.id) += uf.eval(nowTick)
      i += 1
    }
    dxLayers(out, x, in)
  }
  protected def updateLayer(out: STK, layerId: Int, id: Int, act: Double): Unit = {
    out(layerId)(id) += act.asInstanceOf[T]
  }
  protected def stepLayer(lay: Layer, out: STK, x: STK, f: STPF, ns: NoiseSampler): Unit = {
    // cache all these calls locally
    val tauF: T = 1.0 / lay.tau
    val id = lay.id
    val oData = out(id).data
    val xData = x(id).data
    val len = xData.length
    val op = _layerActs(lay.id)
    val noiseScale = lay.noiseScale
    val q = ns.Q

    val g: OP =
      if (q > ZERO && noiseScale > 0.0) {
        var qIndex = Random.nextInt(_randoms.length)
        val z: OP = (res: Double) => {
          qIndex += 1
          if (qIndex == _randoms.length) qIndex = 0
          res + tauF * q * _randoms(qIndex) * noiseScale
        }
        if (op != Activations.LINEAR)
          op.get.compose(z)
        else
          z
      } else
        op

    if (len < paraThreshold) {
      var i = 0
      while (i < len) {
        oData(i) = g(xData(i) + tauF * f(id, i)).asInstanceOf[T]
        i += 1
      }
    } else {
      para(0, len, paraThreshold) { (begin: Int, end: Int) =>
        var i = begin
        while (i < end) {
          oData(i) = g(xData(i) + tauF * f(id, i)).asInstanceOf[T]
          i += 1
        }
      }
    }
  }
  protected def stepLambda(lay: LambdaLayer, out: STK, x: STK, f: STPF, ns: NoiseSampler): Unit = {
    val id = lay.id
    val oData = out(id).data
    val xData = x(id).data
    lay.lambda.dxF(oData, xData, f(id, _))
  }

  /** NOTE : need to subtract off max to keep Math.exp stable  */
  protected def stepSoftMax(lay: SoftMax, out: STK, f: STPF): Unit = {
    val id = lay.id
    val oData = out(id).data
    val len = lay.length
    var max: T = MIN_VALUE
    var i = 0
    var x = ZERO
    while (i < len) {
      x = f(id, i)
      oData(i) = x
      if (x > max)
        max = x
      i += 1
    }
    i = 0
    var sum = ZERO
    while (i < len) {
      x = (Math.exp(oData(i) - max)).asInstanceOf[T]
      oData(i) = x
      sum += x
      i += 1
    }
    i = 0
    while (i < len) {
      oData(i) = oData(i) / sum
      i += 1
    }
  }
  protected def stepInput(lay: Input, out: STK, x: STK, f: STPF, ns: NoiseSampler): Unit = {
    val id = lay.id
    val oData = out(id).data
    val inData = lay.vector
    val len = lay.length
    var i = 0
    while (i < len) {
      oData(i) = (inData(i) + f(id, i)).asInstanceOf[T]
      i += 1
    }
  }
  protected def stepHelper(out: ARR, x: ARR, f: STPF): Unit = {
    val size = _count
    val taudata = _tau.data
    // NOTE: JVM slower on (Int, Double) function, so define 2 blocks
    if (_sparseAct.isDefined) {
      val op = _sparseAct.get
      var i = 0
      while (i < size) {
        out(i) = op(x(i) + taudata(i) * f(0, i))
        i += 1
      }
    } else {
      val outs = _sparseActs
      var i = 0
      while (i < size) {
        out(i) = outs(i)(x(i) + taudata(i) * f(0, i))
        i += 1
      }
    }
  }
}
