package memnets.fx.app

import java.text.DecimalFormat

import javafx.fxml.FXMLLoader
import memnets.fx._
import memnets.fx.fx3d.Scene3DFX
import memnets.fx.utils.ActionP
import memnets.fx.utils.TableColumnUtils.ColumnBuilder
import memnets.ml.DataSource
import memnets.model._
import memnets.ui._
import memnets.utils._
import scalafx.Includes._
import scalafx.scene.control._
import scalafx.scene.control.cell.TextFieldTableCell
import scalafx.scene.layout._
import scalafx.scene.text.Text
import scalafx.util.converter.NumberStringConverter

class SideBarFX(modelFX: ModelFX, engineFX: EngineFXAdapter) extends Logging {
  import javafx.scene.{control => cfx}
  val engine = engineFX.engine
  val formatter: FastFormatter = TwoDigits
  val loader = new FXMLLoader("sidebar.fxml".asURL)
  val fx = new BorderPane(loader.load())
  val showPane = new StackPane(fx.findById("showPane"))
  val showBtn = new ToggleButton(fx.findById[JToggleButton]("showBtn"))
  val focusText = new Text(fx.findTextById("focusText"))

  val toolBar = new ToolBar(fx.findById("toolBar"))
  val libBar = new ToolBar(fx.findById("libBar"))
  val libSepName = "libSep"
  val speed = new Label(fx.findById[JLabel]("speedLbl"))

  val tabPane = new TabPane(fx.findById("tabPane"))
  tabPane.setStyle("-fx-box-border: transparent;")
  // retrieve by id not working
  val trialTab = tabPane.tabs.get(0)
  val paramTab = tabPane.tabs.get(1)
  val skinTab = tabPane.tabs.get(2)
  val configTab = tabPane.tabs.get(3)
  val env3dTab = tabPane.tabs.get(4)
  val dataTab = tabPane.tabs.get(5)

  trialTab.tooltip = "trials/levels for model/game"
  paramTab.tooltip = "adjustable parameters for model"
  skinTab.tooltip = "visualizations for model.  need to press 'Refresh skin' after editing"
  configTab.tooltip = "configuration options.  need to press 'Rebuild' after editing"
  dataTab.tooltip = "available data sets.  only used by certain models"
  env3dTab.tooltip = "lighting and materials editor for 3D scenes"

  speed.text.value = ""
  // unlike selection/disable properties, modifying text always has to be on FX thread.
  engineFX.speedText.onChange { (_, _, text) =>
    runLaterP {
      speed.text.value = text
    }
  }

  val paramEditor = new ParamEditorFX(engineFX)
  paramTab.setContent(paramEditor.rootPane)

  val trialTable = new TableView(fx.findById[cfx.TableView[Trial]]("trialTable"))
  val trialCb = new ColumnBuilder[Trial](trialTable)
  trialCb.pS("Description", editable = false)
  trialCb.pS("Time", editable = false, prop = "timeText")
  trialCb.pI("#Ins", editable = false, prop = "inputCount")
  engineFX.trialsModel.bind(trialTable)
  for ((col, i) <- trialTable.columns.zipWithIndex) {
    col.prefWidthProperty.unbind()
    // 300 total   320-180=120/2=70
    col.prefWidthProperty.value = if (i == 0) 175 else 60
  }
  // trialTable.setColumnResizePolicy(TableView.ConstrainedResizePolicy)
//  trialTab.setContent(trialTable)

  val skinEditor = new SkinEditorFX
  val skinCb = new ColumnBuilder[SkinType](skinEditor.skinTable)
  skinCb.pS("Name", editable = false)
  skinCb.pS("ColorMap", editable = false, prop = "ColorMapName")
  engineFX.skinsModel.bind(skinEditor.skinTable)
  skinTab.setContent(skinEditor.fx)

  val configEditor = new ConfigEditorFX
  configTab.setContent(configEditor.fx)

  /* can't use.  listen to SkinInitializedEvent in AppBaseFX instead
  engineFX.skinsModel.selectedItemProperty ==> { newSkin =>
    runLaterP {
      skinEditor.setSkin(newSkin)
    }
  }
   */

  val sceneEditor = new Scene3DEditorFX
  env3dTab.setContent(sceneEditor.fx)

  val inputTable = new TableView(fx.findById[cfx.TableView[Signal]]("inputTable"))
  inputTable.setEditable(true)
  val inputCb = new ColumnBuilder[Signal](inputTable)
  inputCb.pB("Use", prop = "active")
  inputCb.pS("Target", editable = false)
  inputCb.pI("On")
  inputCb.pD("Scale")
  inputCb.pD("Period")

  engineFX.inputModel.bind(inputTable)
  for ((col, i) <- inputTable.columns.zipWithIndex) {
    col.prefWidthProperty.unbind()
    col.prefWidthProperty.value = i match {
      case 0 => 35
      case 1 => 95
      case 2 => 50
      case 3 => 50
      case 4 => 75
    }
  }

  val dataTable = new TableView[DataSource]()
  dataTable.setEditable(true)
  dataTab.setContent(dataTable)
  val dataCb = new ColumnBuilder[DataSource](dataTable)
  dataCb.pS("Info", editable = false, prop = "desc")
  dataCb.pI("Feat", prop = "features")
//  dataCb pI("Size", prop = "size", converter = new NumberStringConverter(new DecimalFormat("0.##E0")))
  val sizeCol = dataCb.pI("Size", prop = "size")
  sizeCol.cellFactory = (c: TableColumn[DataSource, Number]) =>
    new TextFieldTableCell[DataSource, Number](new NumberStringConverter(new DecimalFormat("0.##E0")))

  dataCb.pD("Prob", prop = "probability")

  import memnets.ml.DataSources.dataSourcesModel
  logger.trace("datasources size = " + dataSourcesModel.getItemCount)
  engineFX.dataSourcesModel.bind(dataTable)
  for (col <- dataTable.columns) col.prefWidthProperty.unbind()
  dataTable.columns(0).setPrefWidth(155)
  dataTable.columns(1).setPrefWidth(36)
  dataTable.columns(2).setPrefWidth(60)
  dataTable.columns(3).setPrefWidth(40)

  def bindActions(): Unit = {
    ActionP.lookupActions(loader)
  }
  def hide(): Unit = {
    showBtn.selected.value = false
  }
  def resize(w: Double, h: Double): Unit = {
    val scale = if (h >= 720) h / 720.0 else 1.0
    showPane.prefHeight.value = ModelFX.HEADER_H * scale
  }
  def setSkin(skin: SkinType) = {
    skinEditor.setSkin(skin)
    syncSceneEditor()
  }
  def syncSceneEditor() = {
    logger.debug("syncSceneEditor")

    var opt: Option[Scene3DFX] = None
    for (sc3d <- modelFX.tickables.find(x => x.element.isInstanceOf[Scene3D] || x.isInstanceOf[ContainerFX])) {
      logger.debug("found scene3D")
      val fx: Scene3DFX = sc3d match {
        case cfx: ContainerFX if cfx.tfx.isInstanceOf[Scene3DFX] =>
          cfx.tfx.asInstanceOf[Scene3DFX]
        case sfx: Scene3DFX =>
          sfx
        case default =>
          null
      }
      opt = Option(fx)
    }
    env3dTab.disable = opt.isEmpty
    sceneEditor.setScene(opt)

  }
  def tick(te: Tick): Unit = {
    if (showBtn.delegate.isSelected) {
      val y = modelFX.selectedY.value
      if (y != null) {
        if (te.t % 3 == 0)
          focusText.text = y.name + ": " + formatter.format(y.act)
      } else
        focusText.text = "" // will not retrigger if the same
    }
  }
}
