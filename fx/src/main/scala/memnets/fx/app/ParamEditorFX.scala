package memnets.fx.app

import javafx.fxml.FXMLLoader
import memnets.fx._
import memnets.fx.utils.TableColumnUtils._
import memnets.model._
import memnets.utils._

class ParamEditorFX(engineFX: EngineFXAdapter) extends Logging {
  import scalafx.Includes._
  import scalafx.scene.control._

  val paramLoader = new FXMLLoader("param-editor.fxml".asURL)
  val paramCtrl = new ParamController
  paramLoader.setController(paramCtrl)
  val rootPane: JAnchorPane = paramLoader.load() // need parens!
  import paramCtrl._
  val tieSliderP = new Slider(tieSlider)
  tieSliderP.disable = true
  tieSliderP.valueChanging ==> { changing =>
    if (!changing) {
      if (paramFX.isDefined) {
        val prm = paramFX.get.param
        if (prm.precision > 0.0) {
          val factor = 1.0 / prm.precision
          tieSliderP.value.value = Math.round(factor * tieSliderP.value.value) / factor
        }
      }
    }
  }
  var paramFX: Option[ParamFX] = None

  engineFX.paramsModel.selectedItemProperty.onChange { (_, oldPrm, newPrm) =>
    for (oldPrmFX <- paramFX) {
      logger.debug(s"unbinding old tag = $oldPrm, w = ${oldPrm.value}")
      tieSliderP.value.unbindBidirectional(oldPrmFX.value)
      // b/c using JavaFX textProperty, make sure to use delegate here!!!
      tiePrecTxt.textProperty.unbindBidirectional(oldPrmFX.precision.delegate)
      tieSliderP.min.unbind()
      tieSliderP.max.unbind()
      tieSliderP.majorTickUnit.unbind()
      tieSliderP.minorTickCount.unbind()
      tieSliderP.blockIncrement.unbind()
    }
    paramFX = None
    tieSliderP.disable = true
    tiePrecTxt.disable = true
    if (null != newPrm) {
      logger.debug("binding new tag = " + newPrm + " w = " + newPrm.getValue)
      val prmFX = new ParamFX(newPrm)
      tieSliderP.min <== prmFX.min
      tieSliderP.max <== prmFX.max
      tieSliderP.majorTickUnit <== prmFX.range / 2.0
      tieSliderP.minorTickCount = 4
      tieSliderP.showTickMarks.value = true
      tieSliderP.blockIncrement <== prmFX.range / 10.0
      tiePrecTxt.disable = false
      tieSliderP.disable = false
      tieSliderP.value <==> prmFX.value // using bi b/c cols table can edit.
      new NumberStringConverterP(tiePrecTxt.textProperty, prmFX.precision)
      paramFX = Some(prmFX)
    }
  }
  val tieCB = new ColumnBuilder[TieType](tieTable)
  tieCB.pS("Name", editable = false)
  tieCB.pD("Value")
  tieCB.pD("Min")
  tieCB.pD("Max")
  engineFX.paramsModel.bind(tieTable)

  val headCol = tieTable.getColumns.get(0)
  headCol.setSortType(TableColumn.SortType.Descending)
  tieTable.getSortOrder.add(headCol)

  for ((col, i) <- tieTable.columns.zipWithIndex) {
    col.prefWidthProperty.unbind()
    // 19 for scroll
    col.prefWidthProperty.value = if (i == 0) 123 else 60
  }
}

class ParamFX(val param: Param) {
  import memnets.fx.utils.BeanUtils._
  val min = dbl(param, "min")
  val max = dbl(param, "max")
  val precision = dbl(param, "precision")
  val value = dbl(param, "value")
  val range = max - min
}
