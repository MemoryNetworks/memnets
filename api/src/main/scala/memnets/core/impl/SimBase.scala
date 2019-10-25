package memnets.core.impl

import java.util
import java.util.Collections

import breeze.linalg._
import breeze.stats.distributions.Gaussian
import memnets.core._
import memnets.linalg._
import memnets.model.Config._
import memnets.model.Tick._
import memnets.model._

/**
 * attempts to share as much code as possible between float + double impls
 * not going with breeze implicit pattern on purpose
 * also, using specialized here leads to lots of compiler issues
 * javap shows native float[], double[] are generated in final classes
 *
 * NOTE: the LeapFrog impl currently only supports the sparse API
 */
abstract class SimBase(override val system: DynamicSystem) extends AbstractSim(system) with Logging {
  logger.debug("SimBase ctor")
  type T <: AnyVal
  type ARR = Array[T]
  type DV = DenseVector[T]
  type STK = Array[DV]
  type SV = SparseVector[T]
  type STPF = (Int, Int) => T
  type SIGS = scala.collection.IndexedSeq[SimSignal]

  implicit def d2T(d: Double): T
  // using class instead of func in case need to save off info upon creation
  trait LayerLinkCalc {
    def calc(out: STK, x: STK): Unit
  }
  trait LayerCalc {
    def calc(out: STK, x: STK, f: STPF, q: NoiseSampler): Unit
  }
  protected var _linearOpCount = 0
  protected var _t = 0
  protected var _isLastDx = false // set at end of iter schema
  protected val _MAX_N_2LOG = 20
  protected val _EMPTY_SPIKES = List[Yb]()
  protected val _count = system.variables.length
  protected val _n = if (_count < 8) 8 else _count.toEven
  protected val _x = zerosStack
  protected val _sparseData = _x(0).data
  protected val _system = system // extra ptr helps subclasses
  protected var _xCalc = _x // xCalc should always point to someone
  protected val _xTemp = zerosStack
  protected val _empty = zerosDen(size = emptySize)
  protected val _in = WMatrix[Double](initCapacity = 256)
  protected val _T = zerosSp()
  protected val _tau = zerosDen()
  protected val _noiseScale = zerosDen()
  protected var _iter: IterMethod = _ // do NOT set b/c leap depends ONE,TWO (not valid here)
  protected val _spikeReset: SpikeListener = system.sparse.onSpike.getOrElse { t =>
    for (n <- system.variables)
      if (n.lastSpike == t) n.update(0.0)
  }
  protected var _sparseAct: Option[OP] = None

  /** huge performance diff (2-3+ x) for saving off funcs in array vs access on Y impl */
  protected var _sparseActs: Array[OP] = Array.fill(if (system.sparse.activation.isEmpty) _n else 0) { Activations.LINEAR }
  protected val _layerActs: Array[OP] = Array.fill(system.layers.size + 1) { Activations.LINEAR }
  protected val _layerCalcs = Array.ofDim[LayerCalc](system.layers.size)
  protected val _linkCalcs = Array.ofDim[LayerLinkCalc](system.links.length)
  protected val _pdf = new Gaussian(0.0, 1.0)
  /** NOTE : very expensive to always sample, so cache one big draw, shuffle periodically */
  protected val _randoms = _pdf.sample(Math.pow(2, 14).toInt).toArray // ~16K
  protected val _randomsJ = util.Arrays.asList(_randoms: _*)
  protected val _matrix: CooMatrixLike[T] = system.sparse.matrix.asInstanceOf[CooMatrixLike[T]]

  protected abstract class IterMethod(val name: String) {
    def iterBuffers: Array[STK]
    def iterate(x: STK, in: SIGS, q: NoiseSampler): Unit
    def reset(): Unit = {
      for (b <- iterBuffers)
        SimBase.this.clear(b)
    }
    override def toString = name
  }
  protected def dx(out: STK, x: STK, in: SIGS, ns: NoiseSampler): Unit
  protected object NoiseSamplerImpl extends NoiseSampler {
    private var _Q = 0.0
    def Q = _Q
    def sample(): Unit = { _Q = sampleNoise() }
  }
  protected abstract class TickImpl extends Tick {
    var lastSpikeCount = 0
    var spikeCount = 0
    override def topK(
        layer: LayerLike,
        k: Int = 3,
        min: Double = 0.0,
        infl: Influence = NopInfluence
    ): TopK = {
      topKHelper(layer, k, min, infl)
    }
    def forceGpuSync(lay: Tgt): Unit = forceGpuSyncHelper(lay)
    def clearLayer(lay: LayerLike): Unit = { clear(_x(lay.id)) }
    @inline final def length = _count
    @inline final def t = _t
    @inline final def end = _t == trial.time
    override def toString(): String = s"Tick[t=$t, spikes=${spikeCount}, sparse=${prettyPrint()}]"
  }

  def iterMethod = _iter
  def print = _sparseData.map("%.2f".format(_)).mkString(", ")
  override def reset(): Unit = {
    system.now = NullTick
    _t = 0
    logger.debug("reset: t -> 0")
    clear(_x)
    clear(_xTemp)
    _xCalc = _x // need to set this b4 ics update!!
    _iter.reset()
    nowTickImpl.spikeCount = 0
    nowTickImpl.lastSpikeCount = 0
    for (n <- system.variables) n.lastSpike = -1
    for (tr <- system.triggers) tr.reset()
    for (lay <- system.layers) lay.lastTopK = null
    if (null != trial) {
      logger.debug("init ICs")
      system.now = nowTick // must set b4 ics below (y.update needs)
      super.reset()
      if (_n < _MAX_N_2LOG) log(_sparseData, "_x")
    }
  }
  def step(inputs: scala.collection.IndexedSeq[SimSignal]): Unit = {
    nowTickImpl.lastSpikeCount = nowTick.spikeCount
    if (nowTick.spikeCount > 0) {
      _spikeReset(_t)
      nowTickImpl.spikeCount = 0
    }
    _t += 1

    // update signal samples b4 iterMethod
    for (input <- inputs)
      input.tick(nowTick)

    _isLastDx = false
    // note : technically need inputs for t and t+1 for RK23 but just using t
    NoiseSamplerImpl.sample()
    _iter.iterate(_x, inputs, NoiseSamplerImpl)

    val triggers = system.triggers
    val tlen = triggers.length
    var i = 0
    while (i < tlen) {
      triggers(i).tick(nowTick)
      i += 1
    }

    system.tick(nowTick)

    _xCalc = _x
  }
  def sync(): Unit = {
    logger.debug("sync")
    import OdeMethod._
    _iter = method match {
      case Euler    => euler
      case Ode23    => ode23
      case LeapFrog => leap
      case Custom   => custom
      case _        => ode45
    }
    syncSparse(variables = system.variables)
    logger.debug(s"SimBase: ode= ${_iter}")
    for (base <- system.layers) base match {
      case layer: Layer =>
        _layerActs(layer.id) = syncLayerActivation(layer, log = true) // layers do not support spikes...
      case default =>
    }

    for ((lay, i) <- system.layers.zipWithIndex)
      _layerCalcs(i) = createLayerCalc(lay)

    for ((link, i) <- system.links.zipWithIndex)
      _linkCalcs(i) = createLinkCalc(link)

    logger.trace(s"_count ${_count}")
    log(_T, "_T")
    log(_tau.data, "_tau")
    log(_sparseData, "_x0Data")

    logger.trace(s"_sparseActivation : ${system.sparse.activation}")
    logger.trace(s"_layers : ${system.layers.size}")
    logger.trace(s"_funcs: ${system.sparse.funcs}")
    system.compact()
    _T.compact()
  }
  @inline final def t = _t
  protected def createLayerCalc(lay: AbstractLayer): LayerCalc = lay match {
    case lambda: LambdaLayer => stepLambda(lambda, _, _, _, _)
    case layer: Layer        => stepLayer(layer, _, _, _, _)
    case soft: SoftMax =>
      (out, x, f, n) =>
        stepSoftMax(soft, out, f)
    case input: Input => stepInput(input, _, _, _, _)
  }
  protected def step(out: STK, x: STK, f: STPF, q: NoiseSampler): Unit = {
    stepLayers(out, x, f, q)
    stepSparse(out, x, f)
  }
  protected def stepSparse(out: STK, x: STK, f: STPF): Unit = stepHelper(out(0).data, x(0).data, f)
  protected def stepHelper(out: ARR, x: ARR, f: STPF): Unit

  protected def custom: IterMethod
  protected def euler: IterMethod
  protected def ode23: IterMethod
  protected def ode45: IterMethod
  protected def leap: IterMethod

  // linkCalcs section
  protected def createLinkCalc(link: LayerLink): LayerLinkCalc = link match {
    case dec: Decay        => decayCalc(dec, _, _) // Decay MUST be above OneToOne
    case one: OnetoOne     => oneToOneCalc(one, _, _)
    case dot: Dot          => dotCalc(dot, _, _)
    case mat: MatMul       => matMulCalc(mat, _, _)
    case spa: SparseLink   => sparseCalc(spa, _, _)
    case den: DenseLink    => denseCalc(den, _, _)
    case divT: DivergeTied => divCalc(divT, _, _)
    case dotT: DotTied     => dotTiedCalc(dotT, _, _)
    case div: Diverge      => divergeCalc(div, _, _)
    case conv: Conv1D      => conv1DCalc(conv)
    case ext: CustomLink   => customCalc(ext, _, _) // Lambda impl is a custom
    case lay: LayerLink    => subClassCalc(lay, _, _)
  }
  protected def dxLayers(out: STK, x: STK, in: SIGS) = {
    clearLayers(out)

    val len = in.length
    var i = 0
    while (i < len) {
      val input = in(i)
      val tgt = input.tgt
      // want to filter out as much as possible, as updateLayer could be gpu write
      if (input.on != 0 && tgt.layerId != 0 && input.isActive && input.act != 0.0)
        updateLayer(out, tgt.layerId, tgt.id, input.act)
      i += 1
    }

    val clen = _linkCalcs.length
    i = 0
    while (i < clen) {
      _linkCalcs(i).calc(out, x)
      i += 1
    }
  }
  protected def updateLayer(out: STK, layerId: Int, id: Int, act: Double): Unit
  protected def customCalc(e: CustomLink, out: STK, x: STK): Unit
  protected def conv1DCalc(e: Conv1D): LayerLinkCalc
  protected def decayCalc(e: Decay, out: STK, x: STK): Unit
  protected def denseCalc(e: DenseLink, out: STK, x: STK): Unit
  protected def divCalc(e: DivergeTied, out: STK, x: STK): Unit
  protected def divergeCalc(e: Diverge, out: STK, x: STK): Unit
  protected def dotCalc(e: Dot, out: STK, x: STK): Unit
  protected def dotTiedCalc(e: DotTied, out: STK, x: STK): Unit
  protected def matMulCalc(e: MatMul, out: STK, x: STK): Unit
  protected def oneToOneCalc(e: OnetoOne, out: STK, x: STK): Unit
  protected def sparseCalc(e: SparseLink, out: STK, x: STK): Unit

  /** derived subclasses can extend extra to handle more types */
  protected def subClassCalc(e: LayerLink, out: STK, x: STK): Unit = logger.warn(s"unknown link type $e")

  // Layers section
  protected def stepLayers(out: STK, x: STK, f: STPF, q: NoiseSampler): Unit = {
    val len = _layerCalcs.length
    var i = 0
    while (i < len) {
      _layerCalcs(i).calc(out, x, f, q)
      i += 1
    }
  }
  protected def stepLambda(layer: LambdaLayer, out: STK, x: STK, f: STPF, q: NoiseSampler): Unit
  protected def stepLayer(layer: Layer, out: STK, x: STK, f: STPF, q: NoiseSampler): Unit
  protected def stepSoftMax(soft: SoftMax, out: STK, f: STPF): Unit
  protected def stepInput(input: Input, out: STK, x: STK, f: STPF, q: NoiseSampler): Unit

  protected def clear(d: DV): Unit
  protected def clear(d: STK): Unit = {
    clear(d(0))
    clearLayers(d)
  }
  protected def clearLayers(d: STK): Unit = {
    val len = d.length
    var i = 1
    while (i < len) {
      clear(d(i))
      i += 1
    }
  }
  protected def data(vecs: STK): Array[ARR]
  protected def sampleNoise(): Double = {
    var q = system.noise.getValue
    if (q >= 0.01) {
      // doing costly shuffle here so only once (iter method called more)
      Collections.shuffle(_randomsJ)
      if (q > system.noise.max)
        q = system.noise.max

      q
    } else
      0.0
  }
  protected def nowTickImpl: TickImpl
  protected def syncActivation(y: Y, log: Boolean = false): OP = {
    import Activation._
    y.activation match {
      case Linear =>
        _linearOpCount += 1
        Activations.LINEAR
      case Relu =>
        findOP(y, RELU_MAX, Activations.relu)
      case Sigmoid =>
        findOP(y, SIGMOID_KNEE, Activations.sigmoid)
      case Custom =>
        y.out.get
      case Tanh =>
        Math.tanh
      case Spike =>
        val spike: Double = y.get[Float](SPIKE).getOrElse(30.0f).asInstanceOf[Double]
        (d: Double) =>
          {
            if (_isLastDx && d >= spike) {
              y.lastSpike = _t
              nowTickImpl.spikeCount += 1
              DynamicSystem.SPIKE_AP
            } else d
          }
    }
  }
  protected def syncLayerActivation(lay: Layer, log: Boolean = false): OP = {
    import Activation._
    lay.activation match {
      case Linear =>
        Activations.LINEAR
      case Relu =>
        findOP(lay, RELU_MAX, Activations.relu)
      case Sigmoid =>
        findOP(lay, SIGMOID_KNEE, Activations.sigmoid)
      case Tanh =>
        Math.tanh
      case default =>
        logger.error(s"layer ${lay.name} has invalid activation : ${lay.activation}")
        throw new IllegalArgumentException("activation invalid")
    }
  }
  protected def syncSparse(variables: Iterable[Y]) = {

    var i = 0
    _linearOpCount = 0 // don't like field here, but using anyway
    val spAct = system.sparse.activation
    for (n <- variables) {
      _tau(n.id) = 1.0 / n.tau
      _noiseScale(n.id) = n.noiseScale
      // NOTE : should not remove Y(THRES) once set, but update to 0.0
      for (thr <- n.get[Float](THRES)) {
        val thrD: Double = thr
        _T(n.id) = -thrD
      }
      if (spAct.isEmpty)
        _sparseActs(n.id) = syncActivation(n, log = i < 20)

      i += 1
    }
    val noCustomActs = _linearOpCount == i
    logger.debug(s"noOuts: ${noCustomActs}")
    if (noCustomActs || spAct.isDefined) {
      import Activation._
      val op: OP = spAct.getOrElse(Linear) match {
        case Linear =>
          Activations.linear
        case Relu =>
          Activations.relu
        case Sigmoid =>
          Activations.sigmoid
        case Tanh =>
          Math.tanh
        case default =>
          null
      }
      _sparseAct = Option(op)
      logger.debug(s"using sparseActivation : ${spAct}, _sparseAct = ${_sparseAct}")
    }
  }
  protected def zerosStack: STK = {
    val data = Array.ofDim[DV](system.layers.size + 1)
    data(0) = zerosDen() // sparse always at 0
    for ((layer, i) <- system.layers.zipWithIndex)
      data(i + 1) = zerosDen(size = layer.length)
    data
  }
  protected def zerosDen(size: Int = _n): DV
  protected def zerosSp(size: Int = _n): SV
  protected def weights(link: DenseVectorLike): DV

  /** default impl is just to use Tick's impl */
  protected def topKHelper(
      layer: LayerLike,
      k: Int,
      min: Double = Double.NegativeInfinity,
      infl: Influence = NopInfluence
  ): TopK = {
    Tick.defaultTopK(nowTickImpl, layer, k, min, infl)
  }
  protected def forceGpuSyncHelper(lay: AbstractLayer): Unit = {}

  @inline private def findOP(p: Config, name: String, default: OP): OP = {
    val param = p.get[Float](name)
    if (param.isDefined)
      Activations(name).create(param.get)
    else
      default
  }
  protected def emptySize: Int = {
    val maxSize = Math.pow(2, 18).toInt // ~ 262K
    val xMaxSize = _x.map(_.length).max
    if (xMaxSize <= maxSize) xMaxSize else maxSize
  }
  protected def log(sp: SV, name: String) = {
    if (sp.activeSize < _MAX_N_2LOG) logger.trace(s"$name : $sp")
  }
  protected def log(sp: ARR, name: String) = {
    if (sp.length < _MAX_N_2LOG) logger.trace(s"$name : ${sp.mkString(",")}")
  }
}
