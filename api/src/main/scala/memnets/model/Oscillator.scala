package memnets.model

trait Oscillator {
  def rawFrequency: Double
  def rawFrequency_=(rf: Double): Unit
  def frequency: Double = {
    val rawF = rawFrequency
    if (rawF < 0.0)
      Math.sqrt(-rawF)
    else
      0.0
  }
  def frequency_=(f: Double): Unit = rawFrequency = -f * f

  /** sin(wt + ph) */
  def initY(phase: Double): Double = Math.sin(phase)

  /** w * cos(wt + ph)    from x = y' */
  def initX(phase: Double): Double = Math.cos(phase) * frequency

  def phaseHelper(y: Double, x: Double): Double = {
    val rad = Math.atan2(y, x / frequency)
    if (rad < 0.0) 2.0 * Math.PI + rad else rad
  }
}
