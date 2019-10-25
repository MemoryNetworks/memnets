package memnets.fx

import memnets.model._
import scalafx.beans.property._
import scalafx.collections.ObservableSet

class TimeChart private[memnets] extends ElementBase with Logging {
  private val _selected = ObjectProperty[Trackable](this, "selected")
  val dynamicRange = BooleanProperty(true)
  val tracked: ObservableSet[Trackable] = ObservableSet[Trackable]()
  var sampling = 4
  def reset(): Unit = { _selected.value = null }
  def select(y: Trackable): Unit = {
    val valid = tracked.contains(y)
    logger.trace(s"selection: $y, valid: $valid")
    _selected.value = if (valid) y else null
  }
  def selected: ReadOnlyObjectProperty[Trackable] = _selected
  def init(trial: Trial): Unit = {
    val model = trial.model
    val system = model.system
    tracked.clear()
    viz = Viz.Default
    if (model.tracked.nonEmpty)
      tracked ++= model.tracked
    else {
      logger.debug(s"user tracking not specified")
      // NOTE : tracked is a set, so duplicates filtered out
      if (trial.goals.flatMap(_.flatMap(_.tgt.as[Y])).nonEmpty) {
        // only grab 1st
        for (user <- system.variables.find(_.ui.viz == Viz.User))
          tracked += user

        tracked ++= trial.goals.flatMap(_.filter(_.isGood).flatMap(_.tgt.as[Y]))
        if (tracked.size < 5 && system.variables.length <= 3) {
          for (y <- system.variables)
            tracked += y
        }
      } else if (trial.inputs.iterator.exists(_.on != 0)) {
        logger.debug("tracking first 5 signal targets ")
        for (in <- trial.inputs.iterator.filter(_.on != 0).take(5)) {
          tracked += in
          tracked += in.tgt
        }
        if (tracked.size < 5 && system.variables.length <= 3) {
          for (y <- system.variables)
            tracked += y
        }
      } else {
        logger.debug("tracking first active X")
        tracked ++= system.variables.filter(_.ui.viz.ordinal >= Viz.Default.ordinal).take(5)
      }
    }
  }
}
