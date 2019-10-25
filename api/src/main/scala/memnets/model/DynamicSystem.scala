package memnets.model

import breeze.linalg.DenseMatrix
import memnets.linalg._
import memnets.utils._

import scala.collection.mutable.Buffer

object DynamicSystem {
  val GAME = "GAME"
  val SPIKE_AP = 30.0
  val TAU_DEFAULT = 30.0
  // default impl
  def apply(matrix: WMatrix = WMatrix(numberType = NumberType.Doubles)): DynamicSystem = {
    import memnets.model.impl.DynamicSystemImpl
    new DynamicSystemImpl(matrix)
  }
  val EMPTY_LAYERLINKS: Iterable[LayerLink] = List()
}

/**
 * NOTE: only supports one listener of each type (layout, reset, tick, trial, ...)
 */
trait DynamicSystem extends ConfigMap with Dsl {
  import DynamicSystem._
  var name = ""
  var tau: Double = DynamicSystem.TAU_DEFAULT // don't use map b/c lots of access...

  def addLink[T <: LayerLink](link: T, srcOut: Boolean = true): T
  def addLayer[T <: AbstractLayer](layer: T): T
  def addOsc(osc: Osc): Unit
  def destroy(): Unit = {}

  /** elements here rather than model b/c used often in components */
  def elements: Buffer[Element]
  def game: Boolean = hasFlag(GAME)
  def game_=(b: Boolean): Unit = setFlag(GAME, b)
  def layers: scala.collection.IndexedSeq[AbstractLayer]
  def links: scala.collection.IndexedSeq[LayerLink]
  def noise: Param
  def now: Tick

  /** should ONLY be called by implementations of Sim!!! */
  def now_=(t: Tick): Unit

  /** if you don't put all loc modifys here, take risk that custom skin layout will alter */
  def onLayout: Option[ModelLayout]
  def onLayout_=(f: => Any): Unit

  /** called at the beginning of each trial */
  def onReset: Procedure
  def onReset_=(f: => Any): Unit
  def onTick: TickListener
  def onTick_=(tl: TickListener): Unit

  /**
   * called each time a trial is changed before the trial's own onInit
   * useful if each trial modify params but want default init
   * a bus.post(TrialEvent) comes AFTER trials are set on engine (init) and UI
   */
  def onTrial: Procedure
  def onTrial_=(f: => Any): Unit
  def compact(): Unit = {} // sim calls when done w/ sync.  can optionally free up some memory
  def oscCount: Int
  def params: Params
  def reset(): Unit = { onReset.body() }
  def setOnLayout(f: ModelLayout): Unit
  def setOnReset(f: Procedure): Unit
  def setOnTick(tl: TickListener): Unit
  def setOnTrial(f: Procedure): Unit
  def sparse: Sparse
  def tick(te: Tick): Unit = { onTick.tick(now) }
  def triggers: Buffer[Trigger]
  def numberType: NumberType = sparse.matrix.numberType
  def variables: scala.collection.IndexedSeq[Y]
  def variablesShown: Int
}

trait Sparse extends LayerLike with MutableGraph[Y, W] {
  def nextId: Int
  def activation: Option[Activation]
  def activation_=(act: Activation): Unit
  def addFunc(f: F): Unit
  def funcs: scala.collection.IndexedSeq[F]
  def matrix: WMatrix
  def onSpike: Option[SpikeListener]
  def onSpike_=(sl: SpikeListener): Unit
  def setOnSpike(f: SpikeListener): Unit
  def toMatrix(): DenseMatrix[Double] = {
    val size = nodes.size
    val matirx = DenseMatrix.zeros[Double](size, size)
    for {
      n <- nodes
      e <- n.outs
    } matirx(e.tgt, e.src) = e.w
    matirx
  }

  /** recommended to only call once at start of builder  */
  def fromMatrix(matrix: DenseMatrix[Double]): Array[Y] = {
    require(matrix.rows == matrix.cols)
    val nodes = Array.fill(matrix.rows) { Y()(system) }
    // NOTE: DenseMatrix.activeIterator has nonzero entries
    for (((r, c), w) <- matrix.activeIterator if w != 0.0)
      addEdge(nodes(c), nodes(r)) w = w
    nodes
  }
  def modifyWs(weights: Iterable[E], w: Double): Unit
  def modifyWs(weights: Array[E], w: Array[Double]): Unit
  // extra
  def addWs(src: Linkable, tgts: Iterable[Linkable]): Iterable[W] = for (t <- tgts) yield addEdge(src, t)
  def addWs(srcs: Iterable[Linkable], tgt: Linkable): Iterable[W] = for (s <- srcs) yield addEdge(s, tgt)

}
