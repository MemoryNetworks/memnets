package memnets.model

object Osc {
  def apply(
      freq: Double = Osc.toFreq(0.5),
      damping: Double = 0.0,
      tau: Double = DynamicSystem.TAU_DEFAULT
  )(implicit mn: DynamicSystem): Osc = {

    val osc = new Osc()
    if (tau != DynamicSystem.TAU_DEFAULT) {
      osc.y.tau = tau
      osc.x.tau = tau
    }
    osc.frequency = freq
    osc.decay.w = damping
    osc
  }
  def apply(
      freq: Param,
      damping: Param
  )(implicit mn: DynamicSystem): Osc = {

    val osc = new Osc()
    osc.y2x tie = freq
    osc.decay tie = damping
    osc
  }

  def toPeriod(i: Double): Double = if (i != 0.0) 60.0 / i else 30.0
  def toFreq(i: Double, tau: Double = DynamicSystem.TAU_DEFAULT): Double = {
    2.0 * Math.PI * i * tau / 60.0 // magic #: 60.0 = fps
  }

  def idText(id: Int): String = if (id == 1) EMPTY_STRING else id.toString
}

/**
 * NOTE : both x and model.elements are created.
 * if a skin implements both createOsc and createY, two visualizations will be created.
 * Pendulum3DFX skips all createY and only implements createOsc
 */
class Osc(implicit mn: DynamicSystem) extends Oscillator with Linkable with Element with ElementUI {
  import Osc.idText
  private[memnets] var _oscId = -1

  mn.addOsc(this) // sets oscId
  var viz = Viz.Default

  val y = Y("y" + idText(_oscId))
  val x = Y("x" + idText(_oscId))
  x.ui.hide()

  val decay: E = x --> x
  val y2x: E = y --> x
  x --> y

  decay.w = 0.0
  frequency = 1.0.toFreq()

  def init(phase: Double, scale: Double = 1.0): Unit = {
    y.update(scale * initY(phase))
    x.update(scale * initX(phase))
  }
  def ics(phase: Double, scale: Double = 1.0)(implicit tri: Trial): Signal = {
    val s = Signal(y = y, on = 0, period = 1, scale = scale * initY(phase), gen = StepGen)(tri)
    tri._inputs += s
    tri._inputs += Signal(y = x, on = 0, period = 1, scale = scale * initX(phase), gen = StepGen)(tri)
    s
  }
  def loc: Loc = y.ui.loc
  def loc_=(l: Loc): Unit = {
    y.ui.loc = l
    x.ui.loc = l.up(60.0)
  }
  def name: String = s"osc$oscId"
  def name_=(name: String): Unit = {}
  def oscId: Int = _oscId
  def phase: Double = phaseHelper(y.act, x.act)
  def rawFrequency: Double = y2x.w
  def rawFrequency_=(rf: Double): Unit = { y2x.w = rf }
  def src: Y = y
  def ui: Osc = this

  override def toString: String = s"Osc[id= ${_oscId}]"
}
