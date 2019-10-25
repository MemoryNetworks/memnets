package memnets.models.neuro.swta

import memnets.model.Activation._
import memnets.model._

object SoftWTALayer {
  def apply(n: Int, tau: Double = 10.0)(implicit sys: DynamicSystem) = new SoftWTALayer(Layer(n, tau = tau, act = Relu))
  def apply(lyr: Layer) = new SoftWTALayer(lyr)
}
class SoftWTALayer(val excites: Layer)
    extends AbstractSoftWTA[Yb]
    with LinkableLayer
    with Element
    with Layout
    with Logging {
  require(excites.activation == Relu, "layer must be relu")
  protected implicit def sys = excites.system
  protected def createInhib(): Y = Y("inh", decay = -1.0, act = Activation.Relu, threshold = 1.0, scale = 1.0)

  val e2eT: Param = Param("e2e", 0.8, 0.2)
  val e2iT: Param = Param("e2i", 1.0, 0.2)
  val i2eT: Param = Param("i2e", -10.0, -5.0)
  val inhib: Y = createInhib()
  excites --> inhib tie = e2iT
  inhib --> excites tie = i2eT

  excites --> excites tie = e2eT
  excites.name = "excites"
  excites.tau = 30
  sys.elements += this

  def apply(i: Int): Yb = excites.y(i) // careful using...
  def length = excites.length
  def src: Layer = excites

  // Element
  def ui: LayerUI = excites.ui
  def name: String = excites.name
  def name_=(name: String): Unit = { excites.name = name }
  def layout(): Unit = {
    logger.debug(s"swtalayer[name ${name}]  layout")
    inhib.ui.loc = excites.ui.loc.up(20)
  }
}
