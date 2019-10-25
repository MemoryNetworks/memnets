package memnets

import com.typesafe.scalalogging.StrictLogging
import memnets.model._

package object core extends StrictLogging {
  type BldType = ModelBuilder

  implicit def ts2Opt(ts: TrialState): Option[TrialState] = Some(ts)

  object NULL_ENGINE_LISTENER extends EngineListener {
    def process(ee: EngineEvent): Unit = {}
  }
  def emptyModel(name: String = "New model"): BldType = ModelBuilder(name, SciBranch.Other) { b =>
    }

  def runSteps(config: ModelConfig, steps: Int = 1000, tPrintDiv: Int = 10)(builder: ModelBuilder): Unit = {
    val model = builder.build(config)
    val sim = model.buildSim()
    val start = System.nanoTime
    val tick = sim.nowTick
    val tPrintMod = steps / tPrintDiv
    for (t <- 0 until steps) {
      sim.step()
      if (tick.t < 5 || tick.t % tPrintMod == 0) logger.debug(tick.toString)
    }
    logger.debug("final")
    logger.debug(tick.toString())
    val elapsed = (System.nanoTime - start) / Math.pow(10.0, 6)
    logger.debug(s"complete : $elapsed ms")
    sim.nowTick.forceGpuSyncAll()
    model.validator.tick(tick)
    model.destroy()
    sim.destroy()
  }
}
