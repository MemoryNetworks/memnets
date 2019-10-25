package memnets.model

import java.util.Comparator

import breeze.linalg.DenseVector

trait Tick extends IndexedSeq[Double] {
  def t: Int
  def even: Boolean = t % 2 == 0
  def quarter: Boolean = t % 4 == 0
  def modSec(secs: Int): Boolean = t % (secs * 60) == 0
  def secs: Int = t / 60
  def end: Boolean
  def system: DynamicSystem
  def trialIndex: Int

  // sparse layer section
  def length: Int
  def apply(i: Int): Double
  def update(i: Int, v: Double): Unit

  /** not great name, but want to be clear just apply returning float */
  def applyF(i: Int): Float
  def isActive(i: Int): Boolean = {
    val a = apply(i)
    a > 0.01 || a < -0.01
  }
  def prettyPrint(n: Int = 8): String = {
    val dataStr = iterator.take(n).map(d => f"$d%.3f").mkString("[", ",", "]")
    dataStr
  }

  /** if use variable in Y.F function, this provides access to current data */
  def calc(i: Int): Double
  def spikeCount: Int

  /** spikeCount resets at start of sim step, so use this if need in Y.out function */
  def lastSpikeCount: Int
  // end sparse layer section

  def apply(id: Int, i: Int): Double
  def update(id: Int, i: Int, v: Double): Unit
  def topK(layer: LayerLike, k: Int = 3, min: Double = 0.0, infl: Influence = NopInfluence): TopK
  def forceGpuSyncAll(): Unit = {
    for (lay <- system.layers)
      forceGpuSync(lay)
  }
  def forceGpuSync(lay: AbstractLayer): Unit
  def clearLayer(lay: LayerLike): Unit
  def random(lay: LayerLike): Unit
  def replace(lay: LayerLike, other: DenseVector[Double]): Unit
  def toDenseVector(lay: LayerLike, output: DenseVector[Double] = null): DenseVector[Double]

  // Java
  final def getLength: Int = length
  final def getT: Int = t
  final def isEnd: Boolean = end
}
object Tick {

  /** base here mainly here for testing purposes */
  class MockTick(val system: DynamicSystem) extends Tick {
    var t = 0
    var end = false
    var trialIndex = 0
    var spikeCount = 0
    var lastSpikeCount = 0
    val stack = defaultStack()

    def defaultStack() = Array.tabulate(system.layers.length + 1) { i =>
      if (i == 0)
        Array.ofDim[Double](system.variables.length)
      else
        Array.ofDim[Double](system.layers(i - 1).length)
    }
    def length = system.variables.length
    def apply(i: Int): Double = apply(0, i)
    def applyF(i: Int) = apply(i).asInstanceOf[Float]
    def update(i: Int, v: Double) = update(0, i, v)
    def calc(i: Int) = apply(i)

    def clearLayer(lay: LayerLike): Unit = {}
    def random(lay: LayerLike): Unit = {}
    def replace(lay: LayerLike, other: DenseVector[Double]): Unit = {}
    def toDenseVector(lay: LayerLike, output: DenseVector[Double] = null): DenseVector[Double] = { output }
    def forceGpuSync(lay: Tgt): Unit = {}

    def apply(id: Int, i: Int): Double = stack(id)(i)
    def update(id: Int, i: Int, v: Double) = stack(id).update(i, v.asInstanceOf[Float])
    def topK(
        layer: LayerLike,
        k: Int = 3,
        min: Double = Double.NegativeInfinity,
        infl: Influence = NopInfluence): TopK = Tick.defaultTopK(this, layer, k, min, infl)

    def clear(): Unit = {
      for {
        array <- stack
        i <- array.indices
      } array(i) = 0.0f
    }
    override def toString = s"MockTick[t=$t]"
  }
  class EmptyTick(system: DynamicSystem) extends MockTick(system) {
    override def defaultStack() = Array(Array(0.0f))
    override def apply(id: Int, i: Int): Double = 0.0
    override def update(id: Int, i: Int, v: Double) = {}
    override def topK(
        layer: LayerLike,
        k: Int = 3,
        min: Double = Double.NegativeInfinity,
        infl: Influence = NopInfluence): TopK = TopK(layer, IndexedSeq(), t)

    override def toString = s"EmptyTick[t=$t]"
  }
  object NullTick extends EmptyTick(DynamicSystem())

  object TopEntryComparator extends Comparator[TopKEntry] {
    def compare(o1: TopKEntry, o2: TopKEntry): Int = o2.compare(o1)
  }

  /** a bit strange but don't want impl on trait b/c Sim delegates to sim.topKHelper */
  def defaultTopK(
      tick: Tick,
      layer: LayerLike,
      k: Int = 3,
      min: Double = 0.0,
      infl: Influence = NopInfluence
  ): TopK = {

    val jQueue = new java.util.PriorityQueue[TopKEntry](k)
    var qmin: Double = min
    var i = 0
    val end = layer.length
    var act = 0.0
    while (i < end) {
      act = infl.influence(i, layer(i))
      if (act > qmin) {
        jQueue.add(TopKEntry(index = i)(act = act))
        if (jQueue.size > k) {
          jQueue.poll()
          qmin = jQueue.peek.act
        }
      }
      i += 1
    }
    // todo: don't use array here b/c extra copy below when converted to IndexedSeq...
    val entries = jQueue.toArray(Array.ofDim[TopKEntry](jQueue.size()))
    // compiler won't use natural ordering...
    java.util.Arrays.sort(entries, TopEntryComparator)
    layer match {
      case abslayer: AbstractLayer =>
        for (e <- entries) e.y = abslayer.y(e.index)
      case sparse: Sparse =>
        for (e <- entries) e.y = tick.system.variables(e.index)
    }
    TopK(layer = layer, entries = entries, t = tick.t)
  }
}
