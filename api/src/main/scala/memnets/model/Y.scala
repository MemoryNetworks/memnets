package memnets.model

import memnets.model.impl.YImpl

import scala.collection.Iterable

object Y {
  val DECAY_PRECISION = Precision(0.00001)

  /** @param tau if (tua == DynamicSystem.TAU_DEFAULT), won't get stored, resulting in system.tau */
  def apply(
      name: String = EMPTY_STRING,
      decay: Double = 0.0,
      tau: Double = DynamicSystem.TAU_DEFAULT,
      threshold: Double = 0.0,
      act: Activation = Activation.Linear,
      scale: Double = YRange.scale,
      showText: Boolean = false)(implicit sys: DynamicSystem): Y = {

    val y = new YImpl(sys.sparse.nextId, sys)
    sys.sparse.add(y)
    y.name = name
    if (act != Activation.Linear) y.activation = act
    if (tau !~ DynamicSystem.TAU_DEFAULT) y.tau = tau
    // NOTE: using extra precision on decay
    if (decay.!~(0.0)(DECAY_PRECISION)) sys.sparse.addEdge(y, y).w = decay
    if (threshold !~ 0.0) y.threshold = threshold
    if (scale !~ YRange.scale) y.ui.scale = scale
    if (showText) y.ui.showText = true
    y
  }
}

/** NOTE: naive impl of xCount methods.  leaving open for impl w/ info to do more...  */
trait Y extends Yb with Element with Config with Ordered[Y] with Linkable {
  import Config._

  def lastSpike: Int

  /** should not be set by clients... */
  protected[memnets] def lastSpike_=(se: Int): Unit
  // NOTE: make sure to enclose default in parens!!!
  override def description: String = name ? ("y" + (id + 1).toString)
  def src: Y = this
  def decay: Option[E]
  def ins: Iterator[E]
  def insCount: Int = ins.size
  def outs: Iterator[E]
  def outsCount: Int = outs.size
  def outsNoLoops: Iterator[E]
  def outsNoLoopsCount: Int = outsNoLoops.size

  def f(desc: String, inputs: Y*)(body: TickFunction): F = func(desc, inputs)(body)
  def func(
      desc: String = EMPTY_STRING,
      inputs: Iterable[Y] = Iterable.empty,
      scale: Option[Double] = None,
      viz: Viz = Viz.Default)(body: TickFunction): F
  def functions: Iterable[F]
  def out: Option[OP]
  def out_=(op: OP): Unit
  def spike: Option[Float] = get[Float](SPIKE)
  def spike_=(v: Double): Unit = { update(SPIKE, v) }
  def ui: YUI

  final def compare(that: Y): Int = {
    if (act < that.act)
      -1
    else if (act > that.act)
      1
    else
      0
  }
  // Java API
  def createF(name: String, body: TickFunction): F = func(name)(body)
  def getOut: FuncD = {
    val f = out
    if (f.isDefined)
      f.get.apply(_)
    else
      ???
  }
  def setOut(op: FuncD): Unit = out = op.eval
  def getLastSpike: Int = lastSpike
  def setSpike(v: Double): Unit = spike = v
}
