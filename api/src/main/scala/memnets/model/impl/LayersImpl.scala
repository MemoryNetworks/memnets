package memnets.model.impl

import breeze.linalg.DenseVector
import memnets.model.{GridHints, _}
import memnets.utils._

import scala.collection.mutable.ArrayBuffer

/**  NOTE : delegates most method calls to layer for smaller memory footprint */
private final class YbImpl(val id: Int, val layer: AbstractLayer) extends Yb with Loc with YbUI {
  def scale = layer.ui.scale
  def range = layer.ui.rangeDefault

  def ui = this
  def viz: Viz = layer.ui.viz
  def viz_=(v: Viz) = {}
  def name: String = s"${layer.name}[$id]"
  def name_=(name: String): Unit = {}

  override def loc_=(l: Loc): Unit = {}
  def act = layer.system.now(layer.id, id)
  def update(v: Double) = layer.system.now.update(layer.id, id, v)
  def system = layer.system
  def x = layer.ui.getX(id)
  def y = layer.ui.loc.y
  def z = layer.ui.loc.z
  def loc = this
  def isSpiking = layer.isSpiking(this)

  override def equals(other: Any) = {
    if (other.isInstanceOf[Yb]) {
      val that = other.asInstanceOf[Yb]
      id == that.id && layerId == that.layer.id
    } else false
  }
  override def hashCode = 37 * id * layer.id
  override def toString = description
}

private[memnets] abstract class LayerBase(val length: Int, val system: DynamicSystem)
    extends AbstractLayer
    with ConfigMap
    with LayerUI {

  import VariableType._
  system.addLayer(this)
  protected var _plot: Plot = Plot(this)
  protected var _lastTopK: Option[TopK] = None
  protected var _name = EMPTY_STRING
  protected var _tau: Option[Double] = None

  val id = system.layers.length // do after add, id=0 reserved for sparse...
  val props = collection.mutable.Map[String, Any]()
  val outLinks = ArrayBuffer[LayerLink]()

  var activation = Activation.Linear
  var viz = Viz.Default
  var numericalType: VariableType = Continuous
  var format: IndexFormat = new Layer.DefaultIndexFormat
  var lastGpuSync = -1

  override def name = _name
  def name_=(name: String): Unit = { _name = name }
  @inline def nowTick = system.now
  def loc = _plot.loc
  def loc_=(l: Loc): Unit = { _plot.loc = l }
  def plot = _plot
  def plot_=(pl: Plot): Unit = { _plot = pl }
  def owner: AbstractLayer = this
  def gradient: Option[GradientHints] = get[GradientHints](Config.GRAD_HINTS)
  def gradient_=(hints: GradientHints): Unit = update(Config.GRAD_HINTS, hints)
  def gridHints: Option[GridHints] = get[GridHints](Config.GRID_HINTS)
  def gridHints_=(hints: GridHints): Unit = update(Config.GRID_HINTS, hints)
  def ui: LayerUI = this
  def getX(i: Int): Double = {
    val w: Double = plot.width
    loc.x - w / 2.0 + (i + 0.5) * spacing(w)
  }

  /** NOTE : not an inexpensive call if used a lot */
  def isSpiking(y: Yb) = y.act >= this.applyD(Config.SPIKE, DynamicSystem.SPIKE_AP)
  def lastTopK = _lastTopK
  def lastTopK_=(topK: TopK): Unit = { _lastTopK = Option(topK) }
  def tau: Double = if (_tau.isDefined) _tau.get else system.tau
  override def tau_=(v: Double): Unit = _tau = Some(v)
  def y(i: Int): Yb = new YbImpl(i, this)
  // need these for Element/Realtime signal to work....
  override def hashCode(): Int = super.hashCode()
  override def equals(obj: scala.Any): Boolean = obj match {
    case that: AnyRef =>
      this eq that
    case default =>
      false
  }
  override def toString = s"Layer[name= ${_name}, n= $length, id= $id]"
}
private[model] class LambdaLayerImpl(
    override val length: Int,
    override val system: DynamicSystem,
    val lambda: Lambda
) extends LayerBase(length, system)
    with LambdaLayer

private[model] class SoftMaxImpl(
    override val length: Int,
    override val system: DynamicSystem,
) extends LayerBase(length, system)
    with SoftMax

private[model] class DenseImpl(
    override val length: Int,
    override val system: DynamicSystem,
) extends LayerImpl(length, system)
    with Dense {

  this --> this w = -1.0
}

private[model] class LayerImpl(
    override val length: Int,
    override val system: DynamicSystem
) extends LayerBase(length, system)
    with Layer {

  protected var _bias: Option[DenseVector[Double]] = None
  // faster than map access...
  protected var _decay: Double = 0.0
  override var threshold: Double = 0.0
  def decay: Double = _decay
  def decay_=(v: Double): Unit = { _decay = v }
  def bias = _bias
  def bias_=(b: DenseVector[Double]): Unit = { _bias = Option(b) }

}
private[memnets] class InputImpl(
    override val length: Int,
    override val system: DynamicSystem,
    val vector: DenseVector[Double])
    extends LayerBase(length, system)
    with Input {
  require(vector.length == length, "initial data wrong length")
  protected[memnets] var _isDirty = false
  def isDirty = _isDirty
  def markDirty(): Unit = { _isDirty = true }
  // NOTE : not overriding apply(i) on purpose b/c a realtime input could alter
  override def update(i: Int, d: Double): Unit = {
    vector(i) = d
    markDirty()
  }
  override def clear(): Unit = { :=(DenseVector.zeros[Double](size = length)) }
  override def random(): this.type = {
    :=(DenseVector.rand[Double](size = length))
    this
  }
  override def :=(input: DenseVector[Double]): this.type = {
    if (!(vector eq input))
      System.arraycopy(input.data, 0, vector.data, 0, input.data.length) // vector might be +1 longer
    markDirty()
    this
  }
}
