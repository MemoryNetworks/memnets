package memnets.ui

import java.net.URL

import memnets.model._
import memnets.utils._

import scala.beans._

object Skin extends Logging {
  def apply(init: SkinType => Unit = sk => {})(implicit sf: SF): SkinType = {
    val skin = sf.create()
    init(skin)
    skin
  }
}

abstract class Skin[T, COL <: AnyRef] extends Comparable[Skin[T, _]] with Dsl with Logging {
  type UI <: TickableUI[T]

  implicit def convertToCol(colorf: Colorf): COL = {
    if (colorf.converted == null)
      colorf.converted = convertToColHelper(colorf)
    colorf.converted.asInstanceOf[COL]
  }
  protected var _yGradientMap: YGradientMap[COL] = _
  protected var _yColorMap: Option[YColorMap[COL]] = None
  protected var _colorMap: Option[ColorMap[COL]] = None

  @BeanProperty var name = ""
  @BeanProperty var backColor: Colorf = defaultBackCol
  @BeanProperty var backImage = SkinImage.TWO
  @BooleanBeanProperty var backImageOn = false
  @BooleanBeanProperty var gradientCustom = true
  @BeanProperty var vizLevel = Viz.Fade
  @BeanProperty var colorMapType = ColorMapType.HeatMap
  @BeanProperty var layerVizType = LayerVizType.Layer
  @BeanProperty var gridVizType = GridVizType.Image
  @BeanProperty var meshType = MeshType.Fill
  @BeanProperty var meshDivs = 64
  @BeanProperty var gradientHints = GradientHints()
  @BooleanBeanProperty var bestQuality: Boolean = true

  @BooleanBeanProperty var sparseLayer: Int = 1024
  @BooleanBeanProperty var sparseType: VariableType = VariableType.Continuous
  @BooleanBeanProperty var sparseHints: Boolean = false
  @BeanProperty var sparseWidth: Int = 1000
  @BeanProperty var sparseHeight: Int = 300
  @BooleanBeanProperty var sparseGlass: Boolean = true
  @BooleanBeanProperty var sparseEffect: Boolean = false

  @BeanProperty var chartSampling = 2
  @BooleanBeanProperty var chartOn = true
  @BooleanBeanProperty var chartDynamic = true
  @BooleanBeanProperty var chartGlassOn = true
  @BooleanBeanProperty var signalsOn = true
  @BooleanBeanProperty var titleOn = true
  @BooleanBeanProperty var fpsOn = true
  @BooleanBeanProperty var topPaneOn = true
  @BooleanBeanProperty var useElementDpad = false
  @BeanProperty var zoom = 1.0
  @BeanProperty var zoomAuto = true
  @BeanProperty var layoutAuto = true
  @BeanProperty var edgeScale = 1.0

  def backColorConverted: COL = backColor
  def colorMap: ColorMap[COL] = {
    import ColorMapType._
    colorMapType match {
      case HeatMap =>
        heatMap
      case OpacityMap =>
        opacityMap
      case GrayMap =>
        grayMap
      case InvertedGrayMap =>
        invertedGrayMap
      case CustomMap =>
        _colorMap.getOrElseP(defaultColorMap)
    }
  }
  def colorMap_=(map: Option[ColorMap[COL]]): Unit = {
    _colorMap = map
    if (map.isDefined)
      colorMapType = ColorMapType.CustomMap
  }
  def colorMapName: String = colorMapType.name()
  def yColorMap: YColorMap[COL] = _yColorMap.getOrElseP(_yGradientMap)

  def compareTo(other: Skin[T, _]): Int = other.rank - rank // ascending
  def create(elem: Element): Option[UI] = {
    if (elementFilter(elem)) {
      elem match {
        case y: Y                            => createY(y)
        case s: Signal                       => createSignal(s)
        case f: F if elementFilter(f.owner)  => createF(f)
        case yg: YGoal                       => createYGoal(yg)
        case g: Goal                         => createGoal(g)
        case lg: Plot                        => createPlot(lg)
        case p: PhasePlot                    => createPhasePlot(p)
        case gr: Grd                         => createGrid(gr)
        case l: LayerLike if !l.ui.plot.user => createLayer(l)
        case sc: Scene3D                     => create3d(sc)
        case o: Osc                          => createOsc(o)
        case tc: Tracer                      => createTracer(tc)
        case tr: Trigger                     => createTrigger(tr)
        case default                         => createHelper(elem)
      }
    } else
      None
  }

  /** true if should skin should create */
  def elementFilter(elem: Element): Boolean = elem.ui.viz >= vizLevel

  /** option to create whole system at once.  helpful if create edge objects for small graph */
  def createSystem(system: DynamicSystem): Iterable[UI] = Iterable.empty
  def createHelper(elem: Element): Option[UI] = None
  def createY(y: Y): Option[UI]
  def createOsc(o: Osc): Option[UI] = None
  def createTrigger(tr: Trigger): Option[UI] = None
  def createTracer(tc: Tracer): Option[UI] = None
  def createF(f: F): Option[UI] = None
  def createYGoal(yg: YGoal): Option[UI] = None
  def createGoal(g: Goal): Option[UI] = None
  def create3d(sc: Scene3D): Option[UI] = None
  def createSignal(s: Signal): Option[UI] = None
  def createPlot(l: Plot): Option[UI] = None
  def createPhasePlot(pl: PhasePlot): Option[UI] = None
  def createImage(im: GridData): Option[UI] = None
  def createMesh(gd: GridData, meshType: MeshType = MeshType.Fill): Option[UI] = None

  def createGrid(gr: Grd): Option[UI] = {
    import GridVizType._
    gridVizType match {
      case Image =>
        createImage(gr)
      case Mesh =>
        createMesh(gr, meshType)
    }
  }
  def createLayer(lay: LayerLike): Option[UI] = {
    layerVizType match {
      case LayerVizType.Layer =>
        createPlot(lay.ui.plot)
      case LayerVizType.Grid =>
        createGrid(Grid(lay))
      case LayerVizType.Sliding =>
        createGrid(new Sliding(lay))
    }
  }

  /**
   * is model.sparse already being visualized?
   * default checks if Plot.user or elements has YGrid.
   * if not shown, UI may show sparse as layer
   */
  def hasSparseViz(system: DynamicSystem): Boolean = {
    system.sparse.ui.plot.user || system.elements.exists(x => x.ui.isShown && x.isInstanceOf[YGrid])
  }
  def imageLookup(i: SkinImage): Option[String] = {
    logger.debug(s"skinImage= $i has no resource")
    None
  }
  def imageLookupURL(i: SkinImage): Option[URL] = imageLookup(i).map(_.asURL)
  def init(system: DynamicSystem): Unit = {
    if (1.0 ~ zoom && zoomAuto)
      zoom = zoomDefault(system)

    _yGradientMap = createYGradientMap(system)
    for (hints <- system.sparse.ui.gradient) {
      logger.trace("applying custom sparse hints")
      gradientHints = hints
    }
    _yGradientMap.hints = gradientHints
    backColor.converted = convertToCol(backColor)
  }

  /** adjust viz based on size or other props... */
  def isSmall(system: DynamicSystem): Boolean = {
    system.layers.isEmpty && system.variables.count(x => x.ui.isShown) <= 10
  }
  def isSuitable(system: DynamicSystem): Boolean = true
  def layout(system: DynamicSystem): Unit = {
    // todo : issue here w/ hidden layers...
    if (system.onLayout.isDefined)
      system.onLayout.get.layout()
    else if (layoutAuto) {
      val layers = system.layers.iterator.filter(x => x.ui.isShown && !x.ui.plot.user && x.ui.plot.autoLayout)
      val layouts = system.elements.iterator.filter(_.isInstanceOf[Layout]).map(_.asInstanceOf[Layout]).toList
      val hasPhase = system.elements.exists(_.isInstanceOf[PhasePlot])

      if (layouts.nonEmpty) {
        logger.debug("found layouts to run")
        for (lay <- layouts)
          lay.layout()
      } else if (system.layers.isEmpty && system.variables.length == 2 && hasPhase) {
        logger.debug("default 2nd order layout")
        val x = system.variables(0)
        val y = system.variables(1)
        x.ui.loc = Loc().left(140).down(40)
        y.ui.loc = x.ui.loc.left(160)
      } else if (layers.isEmpty && isSmall(system) && !hasPhase) {
        logger.debug("default small net layout")
        system.variables.toList.center(Loc())
      } else if (layers.nonEmpty && system.variables.isEmpty) {
        logger.debug("default layer layout")
        val h = Display.height
        val ySpacing = h / (layers.size + 1)
        for ((lay, i) <- layers.zipWithIndex) {
          lay.ui.loc = Loc(x = Loc().x, y = h - ySpacing * (i + 1) + lay.ui.plot.height / 2.0)
        }
      } else if (layers.isEmpty) {
        // try to use structure to help layout
        // todo....
      }
    }
  }
  def rank = 0

  /** special case when Skin has state... */
  def remove(elem: Element): Unit = {}
  def safeMeshDivs: Int = {
    val divs = meshDivs
    if (divs < 4) 4 else divs
  }
  // skins should be largely stateless, but here just in case
  def systemBuilt(): Unit = {}
  def zoomDefault(system: DynamicSystem): Double = if (!system.game && isSmall(system)) 1.3 else 1.0
  override def toString = s"Skin[name= $name, rank= $rank]"

  // Java
  def getColorMapName: String = colorMapName

  protected def convertToColHelper(colorf: Colorf): COL
  protected def defaultBackCol: Colorf = {
    Colorf.gray(0.05 + 0.1 * Math.random())
    /* if want a bit more...
    val col = Colorf.hsb(Random.nextInt(360))
    val scale = 0.05 + 0.1*Math.random()
    Colorf(scale * col.r, scale * col.g, scale * col.b, 1.0)
   */
  }
  protected def createYGradientMap(model: DynamicSystem): YGradientMap[COL]
  protected def defaultColorMap: ColorMap[COL] = heatMap
  protected def heatMap: ColorMap[COL]
  protected def opacityMap: ColorMap[COL]
  protected def grayMap: ColorMap[COL]
  protected def invertedGrayMap: ColorMap[COL]
}
