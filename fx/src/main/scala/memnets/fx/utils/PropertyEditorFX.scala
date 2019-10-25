package memnets.fx.utils

import com.typesafe.scalalogging.StrictLogging
import memnets.utils._
import org.controlsfx.control.PropertySheet
import scalafx.Includes._
import scalafx.scene.layout.AnchorPane

class PropertyEditorFX(
    categoryOn: Boolean,
    val fx: AnchorPane = new AnchorPane(),
    val propFactory: DirtyPropertyEditorFactory = new DefaultDirtyPropertyEditorFactory
) extends StrictLogging {

  fx.styleClass.add("property-editor")
  fx.getStylesheets.add("property-editor.css".asURL.toExternalForm)

  val propertySheet = new PropertySheet
  fx.children += propertySheet
  AnchorPane.setAnchors(propertySheet, 0, 0, 0, 0)
  propertySheet.setPropertyEditorFactory(propFactory)

  if (categoryOn) {
    propertySheet.setModeSwitcherVisible(true)
    propertySheet.setSearchBoxVisible(true)
    propertySheet.setMode(PropertySheet.Mode.CATEGORY)
  } else {
    propertySheet.setModeSwitcherVisible(false)
    propertySheet.setSearchBoxVisible(false)
    propertySheet.setMode(PropertySheet.Mode.NAME)
  }

  import memnets.fx.utils.BeanUtils._
  val dirtyProperty = bool(propFactory, "dirty")

  def clearItems(): Unit = {
    propertySheet.getItems.clear()
  }
  def setItems(items: javafx.collections.ObservableList[PropertySheet.Item]): Unit = {
    propFactory.dirty = false
    propertySheet.getItems.setAll(items)
  }
}
