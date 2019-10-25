package memnets.fx.utils

import javafx.beans.binding.Bindings
import javafx.collections._
import scalafx.collections.ObservableBuffer
import scalafx.scene.control.TableView

import scala.collection.JavaConverters._

class SingleSelectionModelP[T <: AnyRef](private val data: ObservableBuffer[T] = new ObservableBuffer[T]())
    extends javafx.scene.control.SingleSelectionModel[T] {
  import scalafx.Includes._
  val isFirstSelected = selectedIndexProperty === 0
  val isLastSelected = selectedIndexProperty === Bindings.size(getItems) - 1
  val isEmptyProp = javafx.beans.binding.Bindings.isEmpty(getItems)

  def this(list: javafx.collections.ObservableList[T]) {
    this(new ObservableBuffer(list))
  }
  def setItems(items: Iterable[T]): Unit = setAllItems(items.asJavaCollection)
  def setItems(list: ObservableList[T]): Unit = setAllItems(list)
  def +=(item: T) = data += item
  def -=(item: T) = data -= item
  def getItemCount: Int = data.size
  def getModelItem(i: Int) = if (i < 0) null.asInstanceOf[T] else data(i)
  def getItems = data.delegate
  def bind(table: TableView[T]): Unit = {
    table.setItems(getItems) // NOTE: make sure to use delegate!
    table.getSelectionModel.selectedItemProperty.onChange { (_, _, newVal) =>
      // table updates selection when setAll.  not desirable when set selection right after
      if (null != newVal)
        select(newVal)
    }
    selectedItemProperty.onChange { (_, _, newVal) =>
      table.getSelectionModel.select(newVal)
    }
  }

  private def setAllItems(col: java.util.Collection[T]): Unit = { data.setAll(col) }
}
