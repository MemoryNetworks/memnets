package memnets.models.neuro.swta

import memnets.model.Activation._
import memnets.model._

import scala.collection.mutable.ArrayBuffer

class SoftWTADist(val count: Int, val n: Int = 3)(implicit sys: DynamicSystem) extends ElementBase with Layout {
  type T = SoftWTASum
  val loc2GlbT = Param("L2G", 1.0, 0.5)
  val glb2LocT = Param("G2L", 1.0, 1.0)
  // inhT affects simult peaks
  val global = Y("Global", decay = -1.0, tau = 1.0, act = Relu, scale = 1.0)
  private val _swtas = ArrayBuffer[T]()
  def swtas: scala.collection.IndexedSeq[T] = _swtas
  def +=(swta: T, add: Boolean = true): Unit = {
    if (add) _swtas += swta
    //    swta.inhib.threshold = 2.0
    //    swta.e2eT.w = 0.3
    swta.sum2inh.w = 0.5
    swta.sum --> global tie = loc2GlbT
    global --> swta.inhib tie = glb2LocT
  }
  for (i <- 0 until count) this += new SoftWTASum(n = n)
  def apply(i: Int) = swtas(i)
  def layoutHelper(ctr: Loc = loc, w: Double = Display.width, locW: Double = 200, inhUp: Double = 180): Unit = {
    global.ui.loc = ctr.up(inhUp)
    val n = swtas.length
    val width = w / n
    val start = ctr.left((n - 1) * width / 2.0)
    for ((swta, i) <- swtas.zipWithIndex if i < n)
      swta.layoutHelper(start.right(i * width), spacing = Math.min(locW, 0.9 * width) / swta.size)
  }
  def layout(): Unit = layoutHelper()
  sys.elements += this
}
