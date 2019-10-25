package memnets.core

import memnets.linalg.NumberType
import memnets.model._

object Sim extends Logging {
  def apply(system: DynamicSystem, config: ModelConfig): Sim = {

    logger.debug(s"numberType: ${system.numberType}")
    import memnets.core.impl._

    val sim = system.numberType match {
      case NumberType.Doubles =>
        new SimDImpl(system, config.paraThreshold)
      case default =>
        new SimImpl(system, config.paraThreshold)
    }
    // must be set b4 sync
    sim.method = config.methodWithHints()
    sim.sync()
    sim
  }
}

trait Sim {
  def destroy(): Unit
  def method: OdeMethod
  def method_=(m: OdeMethod): Unit
  def nowTick: Tick
  def reset(): Unit
  def step(): Unit = step(trial.inputs)
  def step(inputs: scala.collection.IndexedSeq[SimSignal]): Unit
  def sync(): Unit
  def system: DynamicSystem
  def t: Int
  def trial: SimTrial
  def trial_=(t: SimTrial): Unit

  // Java
  def getSystem = system
  def getMethod = method
  def setMethod(m: OdeMethod): Unit = { method = m }
  def getNowTick = nowTick
  def getT = t
  def getTrial = trial
  def setTrial(t: SimTrial): Unit = { trial = t }
}
trait NoiseSampler {

  /** value of last call to sample */
  def Q: Double

  /** samples global Q value.  if > 0.0, also shuffles pool of random samples*/
  def sample(): Unit
}
