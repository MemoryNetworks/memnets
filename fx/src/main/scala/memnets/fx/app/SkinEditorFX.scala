package memnets.fx.app

import java.beans.PropertyDescriptor

import javafx.fxml.FXMLLoader
import memnets.fx._
import memnets.fx.fx3d.Skin3DFXAdapter
import memnets.fx.utils.PropertyEditorFX
import memnets.model._
import memnets.ui._
import memnets.utils._
import org.controlsfx.property.BeanPropertyUtils
import scalafx.scene.control.TableView
import scalafx.scene.layout.AnchorPane

import scala.beans.BeanProperty
import scala.collection.immutable.HashMap

class SkinEditorFX {
  private val toolTips = HashMap[String, String](
    "backColor" -> "Background color for scene. Must have backImgOn = false",
    "backImage" -> "Background image for scene. Must have backImgOn = true",
    "backBlend" -> "Background blending mode for scene",
    "backImgOn" -> "Show background image",
    "gradDivsMin" -> "Minimum divisions of gradient. Smaller value means colors change more",
    "gradHue" -> "Starting color of gradient",
    "gradSpectrum" -> "A larger gradient spectrum will give more colors",
    "gradMaxLength" -> "If value < variables.length, gradient will loop over colors",
    "gradSaturation" -> "Color saturation. Smaller numbers are more white",
    "gradBrightness" -> "Color brightness",
    "gradOpacity" -> "Color opacity",
    "gradCustom" -> "if FALSE, ignores any customized colors set",
    "colorMap" -> "Color map for grid visualizations",
    "canvasBlend" -> "Blending mode for canvas",
    "edgeScale" -> "Scaling of edges.  0.0 to turn off. BUG: if 0.0 ignored, use 0.1 then 0.0",
    "canvasEffect" -> "Optional effect for canvas",
    "signals" -> "Show signals",
    "topPane" -> "Show/hide entire top panel. Top left menu button always remains visible",
    "titleOn" -> "Show/hide model/trial titles in top panel",
    "timeChartOn" -> "Show/hide time chart in top panel",
    "timeSampling" -> "Sampling freq of time chart. Larger value gives longer time. BUG: fast forward assumes even values >= 4",
    "timeTracking" -> "Dynamic range based on signals in time chart.",
    "layerViz" -> "Layer visualization",
    "gridViz" -> "Grid visualization",
    "meshType" -> "Type of mesh rendered",
    "zoomAuto" -> "Auto zoom based on model size/type",
    "bestQuality" -> "Some skins can turn off expensive graphics for sparse variables",
    "sparseLayer" -> "Threshold for rendering all variables in one sparse layer",
    "sparseType" -> "Show sparse layer as continuous/line or discrete/bars",
    "sparseHints" -> "Force use of width, height, glass, effect on sparse layer"
  )
  private val loader = new FXMLLoader("skin-editor.fxml".asURL)

  val fx = new AnchorPane(loader.load())
  val skinTable = new TableView[SkinType](fx.findById[javafx.scene.control.TableView[SkinType]]("skinTable"))
  val propertyEditorFX = new PropertyEditorFX(
    categoryOn = true,
    new AnchorPane(fx.findById("skinPropPane"))
  )
  var viewModel: Option[SkinViewModel] = None

  def setSkin(skinType: SkinType): Unit = {
    propertyEditorFX.dirtyProperty.value = false
    val viewModelOpt = skinType match {
      case adapt: Skin3DFXAdapter =>
        Some(new SkinViewModel(adapt.skin3d, Some(adapt)))
      case full: FullSceneSkinFX =>
        None // no edits in fullscreen
      case skinFX: SkinFXBase =>
        Some(new SkinViewModel(skinFX))
    }
    viewModelOpt match {
      case Some(vm) =>
        propertyEditorFX.setItems(BeanPropertyUtils.getProperties(vm, process))
        viewModel = Some(vm)
      case None =>
        propertyEditorFX.clearItems()
        viewModel = None
    }
  }

  protected def process(bp: PropertyDescriptor): Boolean = {
    val cat =
      if (bp.getName.startsWith("back"))
        //            "Background"
        "General"
      else if (bp.getName.startsWith("grad"))
        "Y Coloring"
      else if (bp.getName.startsWith("sparse"))
        "Sparse Plot"
      else
        "General"
    import org.controlsfx.property.{BeanProperty => BP}
    bp.setValue(BP.CATEGORY_LABEL_KEY, cat)
    for (tip <- toolTips.get(bp.getName))
      bp.setShortDescription(tip)

    true
  }
}

class SkinViewModel(val skin: SkinFXBase, val rootSkin: Option[SkinFXBase] = None) extends Logging {
  // NOTE: FX doesn't work w/ BooleanBeanProperty...
  @BeanProperty var backColor: JColor = skin.backColorConverted
  @BeanProperty var backImage = skin.backImage
  @BeanProperty var backBlend = skin.backBlend
  @BeanProperty var backImgOn = skin.backImageOn
  @BeanProperty var gradDivsMin = skin.gradientHints.minDivs
  @BeanProperty var gradHue = skin.gradientHints.hue
  @BeanProperty var gradSpectrum = skin.gradientHints.spectrum
  @BeanProperty var gradMaxLength = skin.gradientHints.maxLength
  @BeanProperty var gradSaturation = skin.gradientHints.saturation
  @BeanProperty var gradBrightness = skin.gradientHints.brightness
  @BeanProperty var gradOpacity = skin.gradientHints.opacity
  @BeanProperty var gradCustom = skin.gradientCustom
  @BeanProperty var colorMap = skin.colorMapType
  @BeanProperty var canvasBlend = skin.canvasBlend
  @BeanProperty var canvasEffect = skin.canvasEffect
  @BeanProperty var edgeScale = skin.edgeScale
  @BeanProperty var fpsOn = skin.fpsOn
  @BeanProperty var layoutAuto = skin.layoutAuto
  @BeanProperty var signals = skin.signalsOn
  @BeanProperty var topPaneOn = skin.topPaneOn
  @BeanProperty var titleOn = skin.titleOn
  @BeanProperty var timeChartOn = skin.chartOn
  @BeanProperty var timeSampling = skin.chartSampling
  @BeanProperty var timeTracking = skin.chartDynamic
  @BeanProperty var timeGlassOn = skin.chartGlassOn
  @BeanProperty var layerViz = skin.layerVizType
  @BeanProperty var gridViz = skin.gridVizType
  @BeanProperty var meshType = skin.meshType
  @BeanProperty var zoom = skin.zoom
  @BeanProperty var zoomAuto = skin.zoomAuto
  @BeanProperty var bestQuality = skin.bestQuality

  @BeanProperty var sparseLayer = skin.sparseLayer
  @BeanProperty var sparseType = skin.sparseType
  @BeanProperty var sparseHints = skin.sparseHints
  @BeanProperty var sparseWidth = skin.sparseWidth
  @BeanProperty var sparseHeight = skin.sparseHeight
  @BeanProperty var sparseGlass = skin.sparseGlass
  @BeanProperty var sparseEffect = skin.sparseEffect

  def sync(): Unit = {
    syncHelper(skin)
    for (root <- rootSkin)
      syncHelper(root, full = false)
  }

  protected def syncHelper(skin: SkinFXBase, full: Boolean = true): Unit = {
    skin.bestQuality = bestQuality
    skin.chartOn = timeChartOn
    skin.chartSampling = timeSampling
    skin.chartDynamic = timeTracking
    skin.chartGlassOn = timeGlassOn
    skin.edgeScale = edgeScale
    skin.layoutAuto = layoutAuto
    skin.signalsOn = signals
    skin.topPaneOn = topPaneOn
    skin.titleOn = titleOn
    skin.zoom = zoom
    skin.zoomAuto = zoomAuto
    skin.colorMapType = colorMap
    skin.layerVizType = layerViz
    skin.gridVizType = gridViz
    skin.meshType = meshType

    skin.sparseLayer = sparseLayer
    skin.sparseType = sparseType
    skin.sparseHints = sparseHints
    skin.sparseWidth = sparseWidth
    skin.sparseHeight = sparseHeight
    skin.sparseGlass = sparseGlass
    skin.sparseEffect = sparseEffect

    if (full) {
      skin.titleOn = titleOn

      skin.backBlend = backBlend
      skin.backImage = backImage
      skin.backImageOn = backImgOn
      skin.backColor = Colorf(backColor.getRed, backColor.getGreen, backColor.getBlue, backColor.getOpacity)

      skin.canvasBlend = canvasBlend
      skin.canvasEffect = canvasEffect

      skin.gradientCustom = gradCustom
      skin.gradientHints.minDivs = gradDivsMin
      skin.gradientHints.hue = gradHue
      skin.gradientHints.spectrum = gradSpectrum
      skin.gradientHints.maxLength = gradMaxLength
      skin.gradientHints.saturation = gradSaturation
      skin.gradientHints.brightness = gradBrightness
      skin.gradientHints.opacity = gradOpacity
    }
  }
}
