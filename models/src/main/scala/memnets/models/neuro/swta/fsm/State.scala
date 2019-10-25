package memnets.models.neuro.swta.fsm

import memnets.model._

class State private[fsm] (val y: Y, fsm: FSM) {
  def name_=(name: String): Unit = y.name = name
  def name = y.name
  def -->(tgt: State, name: String): Unit = fsm.add(this, tgt, name)
  def outs = fsm.transitions.filter(_.src == this)
  def ins = fsm.transitions.filter(_.tgt == this)
}
