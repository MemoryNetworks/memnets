package memnets.model.impl

import memnets.linalg._
import memnets.model.Tick._
import memnets.model.VariableType.Continuous
import memnets.model._
import memnets.utils._

import scala.collection.mutable.ArrayBuffer

private[model] final class DynamicSystemImpl(private val _matrix: WMatrix, val params: Params = new ParamsImpl)
    extends DynamicSystem
    with ConfigMap
    with Logging {
  import DynamicSystem._

  import collection.mutable.AnyRefMap
  val elements = ArrayBuffer[Element]()
  val triggers = ArrayBuffer[Trigger]()
  val props = new AnyRefMap[String, Any]()
  val noise: Param = params.create("noise", system = false)(this)
  noise.desc = "Change the global noise for all variables.  Use Y.noiseScale for individual variable"
  var now: Tick = NullTick

  protected val _layers = ArrayBuffer[AbstractLayer]()
  protected val _links = ArrayBuffer[LayerLink]()
  protected val _variables = ArrayBuffer[Y]()
  protected var _onReset: Procedure = NULL_PROCEDURE
  protected var _onTrial: Procedure = NULL_PROCEDURE
  protected var _onTick: TickListener = NULL_TICK_LISTENER
  protected var _onLayout: Option[ModelLayout] = None
  protected var _oscCount = 0

  def layers: scala.collection.IndexedSeq[AbstractLayer] = _layers
  def links: scala.collection.IndexedSeq[LayerLink] = _links
  def variables: scala.collection.IndexedSeq[Y] = _variables
  lazy val variablesShown: Int = variables.iterator.count(_.ui.isShown)

  def onLayout: Option[ModelLayout] = _onLayout
  def onLayout_=(f: => Any): Unit = { _onLayout = Option(new ModelLayoutImpl(f)) }
  def setOnLayout(f: ModelLayout): Unit = { _onLayout = Option(f) }
  def onReset: Procedure = _onReset
  def onReset_=(f: => Any): Unit = { _onReset = new ProcedureImpl(f) }
  def setOnReset(f: Procedure): Unit = { _onReset = f }

  def onTick: TickListener = _onTick
  def onTick_=(tl: TickListener): Unit = { _onTick = tl ? NULL_TICK_LISTENER }
  def setOnTick(tl: TickListener): Unit = { onTick = tl }
  def onTrial: Procedure = _onTrial
  def onTrial_=(f: => Any): Unit = { _onTrial = new ProcedureImpl(f) }
  def setOnTrial(f: Procedure): Unit = { _onTrial = f }
  override def destroy(): Unit = {
    super.destroy()
    _layers.clear()
    _links.clear()
    _variables.clear()
    elements.clear()
    triggers.clear()
    props.clear()
    _onTick = NULL_TICK_LISTENER
    _onReset = NULL_PROCEDURE
    _onLayout = None
    _onTrial = NULL_PROCEDURE

    sparse.destroy()
  }

  /** not a bad idea to call gc shortly after this... */
  override def compact(): Unit = {
    logger.debug("optimize")
    sparse.matrix.sortWeights(variables.length)
    for (link <- _links)
      link.optimize()
  }
  def addLayer[T <: AbstractLayer](layer: T): T = {
    val n = layer.length
    require(n > 0, "layer size must be > 0")
    require(n % 2 == 0, "layer size must be divisible by 2")
    _layers += layer
    layer
  }
  def addLink[T <: LayerLink](link: T, srcOut: Boolean = true): T = {
    _links += link
    if (srcOut) link.src.asInstanceOf[LayerBase].outLinks += link
    link
  }
  def addOsc(osc: Osc): Unit = {
    _oscCount += 1
    osc._oscId = _oscCount
    elements += osc
  }
  def oscCount = _oscCount

  override def equals(obj: Any): Boolean = obj match {
    case ref: AnyRef =>
      this.eq(ref)
    case default =>
      false
  }
  override def toString = s"DynamicSystem[name= $name, props= ${props.take(16).mkString("[", ",", "]")}]"

  object sparse extends Sparse with LayerLikeUI {
    protected var _activation: Option[Activation] = None
    protected var _onSpike: Option[SpikeListener] = None
    protected val _funcs = ArrayBuffer[F]()
    protected var _lastTopK: Option[TopK] = None

    var name = "sparse"
    val id = 0
    val system: DynamicSystem = DynamicSystemImpl.this
    var loc = Loc().down(150)
    var viz = Viz.Default
    var numericalType: VariableType = Continuous
    var width = 1000.0
    var height = 300.0
    var showText = false
    var format: IndexFormat = new Layer.DefaultIndexFormat
    var plot = Plot(this)
    plot.height = 250.0
    plot.loc = Loc().down(125.0)
    def rangeDefault: YRange = {
      if (activation.find(_ == Activation.Relu).isDefined)
        YRange(min = 0.0, max = YRange.scale)
      else if (length > 0 && length < 200) {
        val ranges = variables.map(_.ui.rangeDefault)
        val rMin = ranges.map(_.min).min
        val rMax = ranges.map(_.max).max
        YRange(min = rMin, max = rMax)
      } else
        YRange(-YRange.scale, YRange.scale)
    }
    def owner = this
    def gradient: Option[GradientHints] = get[GradientHints](Config.GRAD_HINTS)
    def gradient_=(hints: GradientHints): Unit = update(Config.GRAD_HINTS, hints)
    def gridHints: Option[GridHints] = get[GridHints](Config.GRID_HINTS)
    def gridHints_=(hints: GridHints): Unit = update(Config.GRID_HINTS, hints)
    def funcs: scala.collection.IndexedSeq[F] = _funcs
    def y(i: Int) = variables(i)
    def lastTopK = _lastTopK
    def lastTopK_=(topK: TopK): Unit = { _lastTopK = Option(topK) }
    def addFunc(f: F): Unit = _funcs += f
    def matrix: WMatrix = _matrix
    def props = DynamicSystemImpl.this.props
    def length = _variables.length
    def ui: LayerLikeUI = this
    def outLinks: Iterable[LayerLink] = EMPTY_LAYERLINKS
    def nextId: Int = variables.length
    def activation: Option[Activation] = _activation
    def activation_=(act: Activation): Unit = _activation = Option(act)
    def onSpike = _onSpike
    def onSpike_=(sl: SpikeListener) = _onSpike = Option(sl)
    def setOnSpike(f: SpikeListener): Unit = { onSpike = f }
    def destroy(): Unit = {
      _onSpike = None
      _lastTopK = None
      _funcs.clear()
    }

    // Graph
    def nodes: Iterable[Y] = _variables
    def add(n: Y) = _variables += n
    def remove(ys: Y): Unit = {
      logger.debug(s"$ys")
      val id = ys.id
      val delEdges = matrix.weights.filter(e => e.src == id || e.tgt == id).toList
      for (e <- delEdges)
        matrix.remove(e)
      ys.ui.viz = Viz.Dead
      _variables -= ys // but remove from sparse
    }
    @inline def find(src: Y, tgt: Y) = matrix.outEdges(src.id).find(_.tgt == tgt.id)
    @inline def inEdges(n: Y) = matrix.inEdges(n.id)
    @inline def outEdges(n: Y) = matrix.outEdges(n.id)

    def addEdgeUnique(src: Y, tgt: Y): E = find(src, tgt).getOrElse(addEdge(src, tgt))
    override def addEdge(src: Y, tgt: Y): E = {
      val e = matrix.create(src.id, tgt.id)
      if (e.isLoop)
        e.w = -1.0f
      e
    }
    def removeEdge(e: E) = matrix.remove(e)
    // NOTE : could be sending events out for this.
    def modifyWs(edges: Array[E], w: Array[Double]): Unit = {
      require(edges.length == w.length)
      var i = 0
      while (i < edges.length) {
        edges(i).w = w(i)
        i += 1
      }
    }
    def modifyWs(edges: Iterable[E], w: Double) = for (e <- edges) e.w = w

  }
}

private final class ModelLayoutImpl(f: => Unit) extends ModelLayout { @inline def layout() = { f } }
