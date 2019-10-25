package memnets.models

import memnets.model._

case class Coupling(
    oscA: Osc,
    oscB: Osc,
    coupling: Param
)(implicit mn: DynamicSystem) {

  val XminusY = Y("x-y", decay = -1.0, tau = 1.0)
  // coupling * diff = K(x - y)
  oscA --> XminusY
  oscB --> XminusY w = -1.0
  XminusY --> oscB tie = coupling

  val YminusX = Y("y-x", decay = -1.0, tau = 1.0)
  // coupling * diff = K(y - x)
  oscB --> YminusX
  oscA --> YminusX w = -1.0
  YminusX --> oscA tie = coupling

  XminusY.ui.skip()
  YminusX.ui.skip()
}
