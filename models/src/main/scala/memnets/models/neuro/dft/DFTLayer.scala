package memnets.models.neuro.dft

import memnets.core.Activations
import memnets.model.Config.SIGMOID_KNEE
import memnets.model._

/**
 * @param hat NOTE: b/c MexicanHat ctor needs implicit sys, can't create default value in ctor
 */
class DFTLayer(val n: Int = 128, val h_u: Double = 1.0, hat: MexicanHat = null)(implicit sys: DynamicSystem) {
  val mexicanHat: MexicanHat = if (hat != null) hat else new MexicanHat()
  val field_u = Layer(n = n, name = "field_u", decay = -1.0, tau = 20.0, threshold = h_u, scale = 4.0)
  val out_u = LambdaLayer(n = n, name = "out_u", f = Activations(SIGMOID_KNEE).create(5.0)) // sigmoid knee (beta) = 5.0
  field_u --> out_u
  Conv1D(out_u, mexicanHat.kernel, field_u)

  // global inhib
  val g_inh = Param("g_inh", max = -2.0, init = -1.0)
  val inhib = Y("inh", decay = -1.0, tau = 5.0, scale = 2.0)
  out_u --> inhib
  inhib --> field_u tie = g_inh
}
