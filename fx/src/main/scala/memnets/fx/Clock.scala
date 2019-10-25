package memnets.fx

import javafx.beans.binding.BooleanBinding
import memnets.model._
import scalafx.beans.property._

case class Clock(var name: String, var loc: Loc = Loc()) extends Element with ElementUI {
  val maxTime = IntegerProperty(-1)
  val time = IntegerProperty(0)
  val degree = DoubleProperty(0.0)
  val warn = BooleanProperty(false)
  val arcEnabled: BooleanBinding = maxTime > 0
  val text = StringProperty("0")
  var viz: Viz = Viz.Default

  time ==> { t =>
    if (t % 60 == 0)
      text.delegate.set((t / 60).toString)
    if (arcEnabled.get) {
      val max = maxTime.value
      val angle = 360.0 * t / max
      degree.delegate.set(Math.floor(angle))
      if (max - t == 600) warn.value = true // 10 secs
    }
  }
  def reset(): Unit = {
    warn.value = false
    time.value = 0
  }
  def ui: Clock = this
}

class SystemClock private[memnets] extends Clock("SystemClock")
