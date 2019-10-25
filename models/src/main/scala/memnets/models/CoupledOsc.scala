package memnets.models

import memnets.model._

import scala.math.random

class CoupledOsc(
    val frequency: Double = Osc.toFreq(0.5),
    val w: Double = 0.05
)(implicit mn: DynamicSystem)
    extends ElementBase
    with Layout {

  mn.elements += this

  val oscA = Osc(freq = frequency)
  val oscB = Osc(freq = frequency)

  val coupling = Param("coupling", max = 2.0, init = w)
  val freq = Param("freg", max = 4.0, init = frequency, min = 1.0)
  freq ==> { w =>
    oscA.frequency = w
    oscB.frequency = w
  }
  val diff = Y(s"${oscA.src.name}-${oscB.src.name}", decay = -1.0, tau = 1.0)
  // coupling * error/diff = K(x - y)
  oscA --> diff
  oscB --> diff w = -1.0
  diff --> oscB tie = coupling

  val diff2 = Y(s"${oscB.src.name}-${oscA.src.name}", decay = -1.0, tau = 1.0)
  // coupling * error/diff = K(y - x)
  oscB --> diff2
  oscA --> diff2 w = -1.0
  diff2 --> oscA tie = coupling

  def randomPhase(mag: Double = 10.0): Unit = {
    oscA.init(phase = 2.0 * Math.PI * random(), scale = mag)
    oscB.init(phase = 2.0 * Math.PI * random(), scale = mag)
  }
  def layout(): Unit = {
    val row0 = loc.down(80)
    oscA.y.ui.loc = row0.left(50)
    oscA.x.ui.loc = oscA.y.ui.loc.left()
    oscB.y.ui.loc = row0.right(50)
    oscB.x.ui.loc = oscB.y.ui.loc.right()
    diff.ui.loc = loc.up(150).right(75)
    diff2.ui.loc = loc.up(150).left(75)
  }
  override def equals(obj: scala.Any): Boolean = obj match {
    case that: AnyRef =>
      this eq that
    case default =>
      false
  }
}
