package memnets.core

import memnets.model.DynamicSystem

object SimProvider {
  implicit def sp2opt(sp: SimProvider): Option[SimProvider] = Option(sp)
}
trait SimProvider {
  def create(sys: DynamicSystem, cfg: ModelConfig): Sim
}
