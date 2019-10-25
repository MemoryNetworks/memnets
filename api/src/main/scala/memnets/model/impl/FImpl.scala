package memnets.model.impl

import memnets.model._

private final class FImpl(
    val owner: Y,
    override val description: String,
    var inputs: Iterable[Y]
)(f: TickFunction)
    extends ElementBase
    with F {
  var _lastEval = 0.0
  var _scale: Option[Float] = None

  def act = _lastEval
  def eval(te: Tick) = {
    _lastEval = f.eval(te)
    _lastEval
  }
  def scale = if (_scale.isEmpty) owner.ui.scale else _scale
  def scale_=(v: Double): Unit = { _scale = Some(v.asInstanceOf[Float]) }
  override def toString: String = s"F[owner= $owner,desc= $description]"
}
