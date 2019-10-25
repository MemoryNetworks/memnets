package memnets.awt

import com.typesafe.scalalogging.StrictLogging
import javax.swing._
import memnets.utils.SelectionModel

// here for Java
class SelectionModelAwt[T <: AnyRef](im: SelectionModel[T]) {
  private val adapter = new ListModelAdapter(im)
  val toListModel: ListModel[T] = adapter
  val toListSelectionModel: ListSelectionModel = adapter.listSelectionModel
}

class ListModelAdapter[T <: AnyRef](protected val _selectModel: SelectionModel[T])
    extends AbstractListModel[T]
    with StrictLogging {
  var _lastItemCount = _selectModel.getItemCount
  _selectModel.onDirty = {
    def lastIncluded(end: Int) = if (end - 1 < 0) 0 else end - 1
    fireIntervalRemoved(this, 0, lastIncluded(_lastItemCount))
    _lastItemCount = _selectModel.getItemCount
    fireIntervalAdded(this, 0, lastIncluded(_lastItemCount))
    listSelectionModel.syncSelected()
  }
  def getSize: Int = _selectModel.getItemCount
  def getElementAt(index: Int): T = _selectModel.getItems(index)

  object listSelectionModel extends DefaultListSelectionModel {
    setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
    syncSelected()
    // make sure to use Item not Index as actual value could change at same index...
    _selectModel.onSelection(item => syncSelected())
    addListSelectionListener { e =>
      val anchor = getAnchorSelectionIndex
      val lead = getLeadSelectionIndex
      if (!e.getValueIsAdjusting && lead < _selectModel.getItemCount && lead >= 0) {
        logger.debug(s"list anchor: $anchor lead: $lead")
        val selected = _selectModel.getItems(lead)
        logger.debug("swing selected: " + selected)
        if (_selectModel.getSelectedItem != selected) {
          logger.debug("syncing: swing --> fx")
          _selectModel.setSelected(selected)
        }
      }
    }
    def syncSelected(): Unit = {
      val item = _selectModel.getSelected
      val i = _selectModel.getSelectedIndex
      val anchor = getAnchorSelectionIndex
      val lead = getLeadSelectionIndex
      logger.debug(s"selectedItemProperty: $item at $i, anchor: $anchor lead: $lead")
      if (lead != i) {
        logger.debug("syncing: fx --> swing")
        setSelectionInterval(i, i)
      }
    }
  }
}
