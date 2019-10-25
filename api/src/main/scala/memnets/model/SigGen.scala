package memnets.model

import java.lang.Math._

import scala.beans.BooleanBeanProperty
import scala.collection.mutable.ArrayBuffer

// make in type enum... of constant, cos, sin
// or maybe dependent obj
abstract class SigGen(val name: String) {
  def input(t: Int, period: Double): Double
  override def equals(other: Any): Boolean = {
    if (other.isInstanceOf[SigGen]) {
      val that = other.asInstanceOf[SigGen]
      name eq that.name
    }
    else false
  }
  override def hashCode = name.hashCode
  override def toString = name
}
// here x = t, y = ampl, assumes pts is sorted by t.  not efficient for lots of pts...
object RealtimeGen {
  sealed case class Sample(var t: Int, var x: Double) extends Comparable[Sample] {
    def compareTo(other: Sample) = t - other.t
  }
  object SampleSorter extends Ordering[Sample] {
    // if overflow issue, if (a.t > b.t) 1 else if (a.t < b.t) -1 else 0
    def compare(a: Sample, b: Sample) = a.t - b.t
  }
}
final class RealtimeGen private[memnets] extends SigGen("realtime") {
  import RealtimeGen._

  import collection.Searching._
  private val test = Sample(0, -1.0)
  private val samples = new ArrayBuffer[Sample]()
  @BooleanBeanProperty var notrec = true
  def add(t: Int, amp: Double): Unit = {
    if (notrec && samples.size > 120) {
      val s = samples.remove(0)
      s.t = t
      s.x = amp
      samples += s
    } else
      samples += Sample(t, amp)
  }

  /**
   * @param t is always RELATIVE to start of signal.  parent object takes care of that....
   */
  def input(t: Int, period: Double): Double = {
    test.t = t
    // NOTE: scala doesn't support simple in place quicksort
    parse(samples.search(test)(SampleSorter))
  }
  // perf : by calling method, no anon method creation each time
  private def parse(res: SearchResult) = res match {
    case InsertionPoint(pos) =>
      val i = pos - 1
      if (i >= 0) samples(i).x else 0.0 // element before
    case Found(pos) =>
      samples(pos).x
  }
}

trait ShiftableGen {
  def phase: Double
  def phase_=(ph: Double): Unit
}
final class LambdaGen(f: (Int, Double) => Double) extends SigGen("lambda") {
  def input(t: Int, period: Double) = f(t, period)
}

object PulseGen extends SigGen("pulse") {
  def input(t: Int, period: Double) = if (0 == t % period) 1.0 else 0.0
}
object SquareGen extends SigGen("square") {
  def input(t: Int, period: Double) = if (t % period < period / 2.0) 1.0 else -1.0
}
object TriangleGen extends SigGen("triangle") {
  def input(t: Int, period: Double) = {
    val half = period / 2.0
    val t2 = t % period
    if (t2 < half) 2.0 * t2 / half - 1.0 else 1.0 - 2.0 * (t2 - half) / half
  }
}
object StepGen extends SigGen("step") { def input(t: Int, period: Double) = 1.0 }
sealed case class SinGen private[memnets] (var phase: Double) extends SigGen("sin") with ShiftableGen {
  def input(t: Int, period: Double) = sin(2.0 * PI * t / period + phase)
}
sealed case class CosGen private[memnets] (var phase: Double) extends SigGen("cos") with ShiftableGen {
  def input(t: Int, period: Double) = cos(2.0 * PI * t / period + phase)
}
