package memnets.model

object OscPop {

  /**
   * can use Osc.toFreq if want to convert raw freq into sim's cycles/sec
   *
   * damping/dampingTie is applied directly to decay, so use negative values.
   *
   * caller must factor tau in
   *
   * NOTE : should not supply both damping and dampingTie
   *
   * @param freq no units assumed.  must calculate desired cycles/sec
   */
  def apply(
      size: Int,
      freq: Double = Osc.toFreq(0.5),
      damping: Double = 0.0,
      dampingTie: Option[Param] = None
  )(implicit sys: DynamicSystem): OscPop = {

    val oscs = new OscPop(size)
    oscs.frequency = freq
    oscs.decay.w = damping
    for (dt <- dampingTie) oscs.decay tie = dt
    oscs
  }

}
class OscPop(val size: Int)(implicit sys: DynamicSystem) extends LinkableLayer with Oscillator {

  val y = Layer(n = size, name = "y")
  val x = Layer(n = size, name = "x")
  x.ui.hide()

  x --> y
  val decay: OnetoOne = x --> x
  val y2x: OnetoOne = y --> x
  decay.w = 0.0

  def init(i: Int, phase: Double, scale: Double = 1.0): Unit = {
    y(i) = scale * initY(phase)
    x(i) = scale * initX(phase)
  }
  def phase(i: Int): Double = phaseHelper(y(i), x(i))
  def rawFrequency: Double = y2x.w
  def rawFrequency_=(rf: Double): Unit = y2x.w = rf
  def src: Layer = y
}

object HeteroOscPop {

  /**
   * can use Osc.toFreq if want to convert raw freq into sim's cycles/sec
   *
   * damping is applied directly to decay, so use negative values.
   *
   * caller must factor tau in
   *
   * @param freq no units assumed.  must calculate desired cycles/sec
   */
  def apply(
      size: Int,
      freq: Int => Double,
      damping: Int => Double
  )(implicit sys: DynamicSystem): HeteroOscPop = {

    val oscPop = new HeteroOscPop(size)
    for (i <- 0 until size) {
      oscPop.frequency(i, freq(i))
      oscPop.decay(i) = damping(i)
    }
    oscPop
  }
}

class HeteroOscPop(val size: Int)(implicit sys: DynamicSystem) extends LinkableLayer {

  val y = Layer(n = size, name = "y")
  val x = Layer(n = size, name = "x")
  x.ui.hide()

  x --> y
  val decay: DenseLink = x ==> x
  val y2x: DenseLink = y ==> x

  def src: Layer = y

  def init(i: Int, phase: Double, scale: Double = 1.0): Unit = {
    y(i) = scale * Math.sin(phase)
    x(i) = scale * Math.cos(phase) * frequency(i)
  }
  def frequency(i: Int): Double = {
    if (y2x(i) < 0.0)
      Math.sqrt(-y2x(i))
    else
      0.0
  }
  def frequency(i: Int, f: Double): Unit = { y2x(i) = -f * f }
}
