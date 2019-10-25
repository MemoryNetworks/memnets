package memnets.fx.utils

import com.typesafe.scalalogging.StrictLogging
import javafx.scene.paint.Color
import scalafx.scene.control._

class ColorCell[T](c: TableColumn[T, Color]) extends TableCell[T, Color] with StrictLogging {
  val picker = new ColorPicker()
  text = null
  contentDisplay = ContentDisplay.GraphicOnly
  item.onChange { (_, _, col) =>
    logger.trace("item changed")
    picker.value.value = col
    text = null
    graphic = if (col == null) null else picker
  }
  picker.onShowing = { e =>
    val tv = delegate.getTableView
    val index = delegate.getTableRow.getIndex
    tv.getSelectionModel.select(index)
    tv.edit(index, c)
  }
  picker.onHiding = { e =>
    this.commitEdit(picker.value.value)
  }
  picker.editable <== c.editable
  picker.prefWidth <== c.prefWidth
}
