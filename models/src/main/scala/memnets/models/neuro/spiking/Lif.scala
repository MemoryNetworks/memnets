package memnets.models.neuro.spiking

import memnets.model._

object Lif {
  def apply(name: String, spike: Double = 4.0, tau: Double = 30.0)(implicit mn: DynamicSystem): Y = {
    val lif = Y(name = name, decay = -1.0, act = Activation.Spike)
    lif.spike = spike
    lif
  }
}
