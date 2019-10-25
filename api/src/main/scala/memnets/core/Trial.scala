package memnets.core

import memnets.model._

object Trial {
  def apply(time: Int = -1, name: String = "")(implicit model: Model): Trial = {
    val t = new Trial(model, model.trials.length)
    model.trials += t
    t.name = name
    t.time = time
    t
  }
}
