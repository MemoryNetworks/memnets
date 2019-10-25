package memnets.models.neuro.swta.fsm

import memnets.model.Activation._
import memnets.model._
import memnets.models.neuro.swta._

import scala.collection.mutable.ArrayBuffer

object FSM {
  def apply(names: String*)(implicit mn: DynamicSystem): FSM = {
    val fsm = new FSM(names.length)
    for ((n, i) <- names.zipWithIndex)
      fsm(i).name = n
    fsm
  }
}

/**
 * largely based on "State dependent computation using coupled recurrent networks"
 * at https://arxiv.org/abs/0809.4296
 * NOTE : original paper used two swtas for proper attractors, but not completely necessary
 */
class FSM(n: Int)(implicit sys: DynamicSystem) extends ElementBase with IndexedSeq[State] with Layout with Logging {

  name = "FSM"
  override def length = n
  val wta = new SoftWTA(n = size)
  wta.inhib.tau = 2.0
  wta.e2eT.value = 0.3
  for (e <- wta) e.tau = 10.0
  private val _states = Array.tabulate(size) { i =>
    val ex = wta(i)
    ex.name = s"${i + 1}"
    new State(ex, this)
  }
  private val _inputs = ArrayBuffer[Y]()
  private val _transitions = ArrayBuffer[Transition]()
  def apply(name: String): State = _states.find(_.name == name).get
  def apply(i: Int) = _states(i)
  def layout(): Unit = {
    logger.debug("fsm layout")
    wta.layoutHelper(loc.up(130), spacing = 140.0, vspacing = 120.0)
    gInhib.ui.loc = loc
    _transitions.map(_.gate).center(loc.down(110))
    _inputs.center(loc.down(250))
  }
  def transitions = _transitions
  def states = _states
  def inputs = _inputs
  def input(name: String) = inputs.find(_.name == name).get
  def add(src: State, tgt: State, input: String): Unit = {
    val in = _inputs.find(_.name == input).getOrElse {
      val y = Y(name = input, decay = -1.0, tau = 5.0, act = Relu)
      y.relumax = 8.0
      _inputs += y
      y
    }
    _transitions += new Transition(src, tgt, in)
  }
  def reset(): Unit = { _states.head.y.update(2.0) }
  // 2.0 (thres) * -5 (-FB) = -10.0
  val gInhib = Y("gInh", decay = -1.0, act = Activation.Relu, threshold = -2.2, scale = 2.0, tau = 4.0)
  gInhib.ui.hide()
  def build(): Unit = {
    val gates = _transitions.map(_.gate)
    gInhib --> gates w = -5.0
    gates --> gInhib w = 0.2
  }

  sys.elements += this // add last so SWTA in elements b4 -> layoutHelper here runs after
  sys.onReset = { reset() }
}
