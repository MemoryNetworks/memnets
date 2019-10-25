package memnets.core.impl

import memnets.core._
import memnets.model.Tick.NullTick
import memnets.model._

abstract class AbstractSim(val system: DynamicSystem) extends Sim with Logging {
  var method: OdeMethod = OdeMethod.Ode45
  protected var _trial: SimTrial = _

  def destroy(): Unit = { system.destroy() }
  def reset(): Unit = {
    if (trial != null) {
      require(nowTick.t == 0)
      for (ic <- trial.ics) {
        ic.tick(nowTick)
        ic.tgt.update(ic.act)
      }
      system.reset()
      trial.reset()
    }
  }
  def trial: SimTrial = _trial
  def trial_=(t: SimTrial): Unit = {
    require(t != null, "null trial")
    if (_trial != null)
      _trial.done()

    if (_trial != t) {
      logger.trace("new trial detected")
      system.now = NullTick
      _trial = t
      system.onTrial.body()
      _trial.init()
    }
  }
}
