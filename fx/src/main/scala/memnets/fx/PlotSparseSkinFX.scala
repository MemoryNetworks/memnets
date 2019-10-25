package memnets.fx

import memnets.model._

/** signals rely on y.loc info, which will not be correct here, so no signals shown */
class PlotSparseSkinFX extends BatterySkinFX with Logging {
  name = "PlotSparse"
  backImageOn = false
  backColor = Colorf.gray(0.8)
  sparseLayer = 0
  override def isSuitable(system: DynamicSystem): Boolean = system.variables.length > 1
  override def createSystem(system: DynamicSystem): Iterable[TickableFX] = {
    val sparse = system.sparse
    val spPlot = sparse.ui.plot
    val numType = sparse.ui.numericalType
    val useEffect = spPlot.useEffect
    val hideGlass = spPlot.hideGlass

    if (sparse.length < 256) {
      sparse.ui.numericalType = VariableType.Discrete
      spPlot.useEffect = true
    }
    spPlot.hideGlass = true
    val list = createLayer(sparse) match {
      case Some(fx) =>
        List(fx)
      case None =>
        List()
    }
    spPlot.hideGlass = hideGlass
    spPlot.useEffect = useEffect
    sparse.ui.numericalType = numType
    list
  }
  override def create(elem: Element): Option[TickableFX] = None
  override def createY(y: Y) = None
  override def zoomDefault(system: DynamicSystem): Double = 1.0
}
