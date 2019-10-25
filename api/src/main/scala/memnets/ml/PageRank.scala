package memnets.ml

import memnets.model.Activation._
import memnets.model._

import scala.collection.mutable.ListBuffer

class PageRank(showJump: Boolean = false, tau: Double = 10.0, showText: Boolean = false)(implicit mn: DynamicSystem) {
  val pages = ListBuffer[Y]()
  def page(name: String): Y = {
    val p = Y(name, decay = -1.0, act = Relu, scale = 1.0, showText = showText, tau = tau)
    pages += p
    p
  }
  val j = Y("Jump", decay = -1.0, threshold = -1.0, act = Relu, tau = tau, scale = 1.0)
  if (!showJump) j.ui.hide()
  val jump = Param("jump", max = 1.0, min = 0.0, init = 0.85)
  jump ==> { w =>
    updateWeights(init = false)
  }
  def updateWeights(init: Boolean = true): Unit = {
    val d = jump.getValue
    for (p <- pages) {
      val outsNoLoops = p.outsNoLoops.toList
      val numOuts = outsNoLoops.size
      for (out <- outsNoLoops)
        out.w = d / numOuts

      if (init) j --> p w = 1.0 - d
    }
    if (!init)
      for (out <- j.outsNoLoops)
        out.w = 1.0 - d
  }
}
