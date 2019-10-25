package memnets.fx.app

import java.beans.PropertyDescriptor

import memnets.core._
import memnets.fx.utils._
import org.controlsfx.property.BeanPropertyUtils

class ConfigEditorFX extends PropertyEditorFX(categoryOn = false) {
  def setConfig(cfg: ModelConfig): Unit = {
    setItems(BeanPropertyUtils.getProperties(cfg, process(cfg, _)))
  }
  protected def process(cfg: ModelConfig, bp: PropertyDescriptor): Boolean = {
    val name = bp.getName
    name match {
      case x if x.startsWith("gpuSync") =>
        cfg.gpuSupport
      case "sizeScale" =>
        cfg.usesSizeScale
      case "fileHint" =>
        cfg.usesFile
      case "data" | "skinFactory" =>
        false
      case "customSim" | "customProvider" =>
        cfg.customProvider.isDefined
      case default =>
        true
    }
  }
  //    for(tip <- toolTips.get(bp.getName))
  //      bp.setShortDescription(tip)
}
