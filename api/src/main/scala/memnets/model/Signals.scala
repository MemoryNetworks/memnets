package memnets.model
import scala.collection.immutable

object LambdaSignal {
  def apply(
      y: Yb,
      on: Int,
      period: Double = 10 s,
      scale: Double = 1.0
  )(f: (Int, Double) => Double)(implicit tri: Trial) : Signal = {
    val s = Signal(y, on, period, scale, new LambdaGen(f))
    s.loopCount = -1 // endless loop (period not important)
    s
  }
}
object Step {
  def apply(
      y: Yb,
      on: Int = 2 s,
      dur: Int = 3 s,
      scale: Double = 1.0,
      desc: String = ""
  )(implicit tri: Trial) : Signal = {
    Signal(y, on, dur, scale, StepGen, desc)
  }
}
object Sin {

  /** default cycles = -1 => loop */
  def apply(
      y: Yb,
      on: Int,
      period: Double = 0.5 toPeriod,
      phase: Double = 0.0,
      cycles: Int = -1,
      scale: Double = 1.0
  )(implicit tri: Trial): Signal = {

    val s = Signal(y, on, period, scale, SinGen(phase))
    s.loopCount = cycles
    s
  }
}
object Cos {

  /** default cycles = -1 => loop */
  def apply(
      y: Yb,
      on: Int,
      phase: Double = 0.0,
      period: Double = 0.5 toPeriod,
      cycles: Int = -1,
      scale: Double = 1.0
  )(implicit tri: Trial): Signal = {
    val s = Signal(y, on, period, scale, CosGen(phase))
    s.loopCount = cycles
    s
  }
}
object Square {

  /** default cycles = -1 => loop */
  def apply(
      y: Yb,
      on: Int,
      phase: Double = 0.0,
      period: Double = 0.5 toPeriod,
      cycles: Int = -1,
      scale: Double = 1.0
  )(implicit tri: Trial): Signal = {
    val s = Signal(y, on, period, scale, SquareGen)
    s.loopCount = cycles
    s
  }
}
object Triangle {

  /** default cycles = -1 => loop */
  def apply(
      y: Yb,
      on: Int,
      phase: Double = 0.0,
      period: Double = 0.5 toPeriod,
      cycles: Int = -1,
      scale: Double = 1.0
  )(implicit tri: Trial): Signal = {
    val s = Signal(y, on, period, scale, TriangleGen)
    s.loopCount = cycles
    s
  }
}
object Sequence {
  def apply(ids: Iterable[Yb])(
      on: Int,
      dur: Int = 2 s,
      scale: Double = 1.0,
      delay: Option[Int] = None
  )(implicit tri: Trial): immutable.IndexedSeq[Signal] = {
    val del = delay.getOrElse(dur)
    val steps = for ((id, i) <- ids.zipWithIndex) yield Step(id, on + i * del, dur, scale)
    steps.toIndexedSeq
  }
}
