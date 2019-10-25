package memnets.fx.utils

import memnets.utils._
import scalafx.Includes._

object SelectionModelAdapter {
  // unfortunately, can't do this b/c listeners will get thrown away w/o ref to adapter....
  //  implicit class SelectionModelExt[T <: AnyRef](val bs : SelectionModel[T]) extends AnyVal {
//    def toFX() : SingleSelectionModelP[T] = new SelectionModelAdapter(bs).model
//  }

}
class SelectionModelAdapter[T <: AnyRef](val bs: SelectionModel[T]) {
  val model = new SingleSelectionModelP[T]()
  bs.onDirty = { model.setItems(bs.getItems) }
  bs.onSelection(sel => model.select(sel))
  model.selectedItemProperty.onChange { (_, _, sel) =>
    bs.selected = sel
  }
}
