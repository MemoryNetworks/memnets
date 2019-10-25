package memnets.model.impl

import memnets.model.Config._
import memnets.model._
import memnets.utils._

private[model] final class YImpl(val id: Int, val system: DynamicSystem) extends Y with ConfigMap with YUI {
  override var name = EMPTY_STRING
  var loc = Loc.SINGLETON_LOC // unlike other elements, don't want to allocate each time
  var viz = Viz.Default
  var lastSpike = -1

  def layer: LayerLike = system.sparse
  @inline def act: Double = system.now.apply(id)
  def update(v: Double): Unit = system.now.update(id, v)

  def activation: Activation = get[Activation](ACTIVATION).getOrElseP(Activation.Linear)
  def activation_=(a: Activation): Unit = update(ACTIVATION, a)
  def decay: Option[E] = system.sparse.loop(this)
  def ins: Iterator[E] = system.sparse.inEdges(this)
  def outs: Iterator[E] = system.sparse.outEdges(this)

  /** outEdges is optimized, so better to filter results... */
  def outsNoLoops: Iterator[E] = system.sparse.outEdges(this).filter(!_.isLoop)

  /** NOTE: default Sim caches all custom outs in array.  access in map or even local var in YImpl is slow */
  def out: Option[OP] = get[OP](OUT_FUNC)
  def out_=(op: OP): Unit = {
    update(OUT_FUNC, op)
    activation = if (op != null) Activation.Custom else Activation.Linear
  }
  def func(name: String, inputs: Iterable[Y], scale: Option[Double], viz: Viz)(body: TickFunction): F = {
    val uf = new FImpl(this, name, inputs)(body)
    uf.viz = viz
    if (scale.isDefined)
      uf.scale = scale.get.asInstanceOf[Float]
    else
      for (sc <- this.scale)
        uf.scale = sc
    system.sparse.addFunc(uf)
    uf
  }
  def functions: Iterable[F] = system.sparse.funcs.filter(_.owner == this)
  def isSpiking: Boolean = system.now.t == lastSpike
  def tau: Double = applyD(TAU, system.tau)

  // ConfigMap section
  override protected[memnets] def props = system.props
  override def mapKey(key: String) = s"${id}_$key"
  def keys = system.keys(s"${id}_")

  // equality section
  override def equals(other: Any) = {
    if (other.isInstanceOf[Y]) {
      val that = other.asInstanceOf[Y]
      id == that.id
    } else false
  }
  override def hashCode = 17 * id
  override def toString = description
  def ui: YUI = this
  def owner: Y = this
}
