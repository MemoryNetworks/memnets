package memnets.model

object Trigger {
  def apply(test: => Boolean, name: String = EMPTY_STRING)(body: => Unit)(implicit sys: DynamicSystem): Trigger = {
    val trig = new Trigger(test)(body)
    trig.name = name
    sys.triggers += trig
    trig
  }
}

/**
 * only evaluate ONCE for each Sim.step() at end of IterMethod
 * does not make any guarantees on execution order or attempt conflict resolution
 * NOTE : evaluated in Sim, so be careful about altering UI elements if multithread
 */
class Trigger private (test: => Boolean)(body: => Unit) extends ElementBase with Tickable with Logging {
  var sampling = 1
  private var _triggered = false
  def triggered = _triggered

  /** NOTE: init will never be called on Trigger */
  final override def init(): Unit = {}
  override def reset(): Unit = { _triggered = false }
  def tick(te: Tick): Unit = {
    if (te.t % sampling == 0) {
      val t = test
      if (_triggered != t) {
        if (t) {
          logger.debug(s"trigger: $name")
          body
        }
        _triggered = t
      }
    }
  }
}
