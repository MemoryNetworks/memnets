package memnets.models.neuro.swta.fsm

import memnets.model.Activation._
import memnets.model._

class Transition private[fsm] (val src: State, val tgt: State, val input: Y)(implicit sys: DynamicSystem) {
  val gate = Y(
    name = s"${src.y.id} > ${tgt.y.id}",
    //    name = s"${src.name}>${tgt.name}",
    decay = 0.2,
    tau = 20.0,
    act = Relu,
    scale = 4.0,
  )
  src.y --> gate
  input --> gate
  gate --> tgt.y w = 2.0
}
