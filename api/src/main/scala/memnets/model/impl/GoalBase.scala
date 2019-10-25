package memnets.model.impl

import memnets.model._

import scala.beans.BeanProperty

abstract class GoalBase private[memnets] extends ElementBase with Goal {
  @BeanProperty var bonusMax = 0
  @BeanProperty var desc: String = EMPTY_STRING
  @BeanProperty var completedTick = 0
  @BeanProperty var startTick = 0
  @BeanProperty var progress = 0.0
  @BeanProperty var reward = 0
  override def description = desc ? s"Goal for $tgt"
}

class GoalImpl(val tgt: AnyRef, val evalF: TickFunction)(implicit val owner: Goals) extends GoalBase {
  type T = AnyRef
  owner += this
  def eval(te: Tick) = evalF.eval(te)
}
class YGoalImpl private[memnets] (val tgt: Y)(implicit val owner: Goals) extends GoalBase with YGoal {
  owner += this
  override def loc = tgt.ui.loc
  override def loc_=(l: Loc): Unit = {}
}
