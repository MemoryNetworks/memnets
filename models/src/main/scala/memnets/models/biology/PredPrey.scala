package memnets.models.biology

import memnets.model.Activation._
import memnets.model._

class PredPrey(implicit mn: DynamicSystem) {

  val prey = Y(name = "Prey", decay = 2.0, act = Relu) // prey (2x growth)
  val pred = Y(name = "Pred", decay = -1.0, act = Relu) // pred (decay)

  prey.f("-py * pd", prey, pred) { t =>
    -prey * pred
  }
  pred.f("py * pd", prey, pred) { t =>
    prey * pred
  }
}
