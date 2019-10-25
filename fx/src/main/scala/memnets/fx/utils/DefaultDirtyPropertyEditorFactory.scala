package memnets.fx.utils

import javafx.util.Callback
import memnets.utils.{BeanSupport, DirtySupport}
import org.controlsfx.control.PropertySheet
import org.controlsfx.property.editor._

/** not perfect for dirty but works well enough */
trait DirtyPropertyEditorFactory
    extends Callback[PropertySheet.Item, PropertyEditor[_]]
    with BeanSupport
    with DirtySupport {

  abstract override def call(item: PropertySheet.Item): PropertyEditor[_] = {
    val editor = super.call(item)
    if (editor != null) {
      editor.getEditor.focusedProperty.addListener { (_, _, value) =>
        if (value)
          dirty = true
      }
    }
    editor
  }
}

class DefaultDirtyPropertyEditorFactory extends DefaultPropertyEditorFactory with DirtyPropertyEditorFactory
