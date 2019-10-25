package memnets.model

import scala.collection.mutable.ArrayBuffer

trait GoalHandler {
  def goalStart(g: Goal): Unit
  def goalOver(g: Goal): Unit
  def goalGroupOver(grp: Goals): Unit
}

trait Goals extends IndexedSeq[Goal] {
  def +=(g: Goal): Unit

  /**
   * subclass to change bonus points default.
   * bonus points only applied if win based on reward
   */
  def bonusCalc(t: Trial): Int = 0
  def bonusMessage: String = "?"
  def next(gh: GoalHandler): Unit
  def goalsLeft: Int = count(g => !g.isCompleted && g.isGood)
  def reset(): Unit = {
    for (g <- this) g.reset()
  }
  def reward: Int = filter(_.isGood.get).map(_.reward).sum
  def startGoals: Iterable[Goal]
  def tick(te: Tick, gh: GoalHandler): Unit
  import collection.JavaConverters._
  def getGoals(): java.util.List[Goal] = this.asJava
  override def toString: String = s"GoalGroup[goals: ${this.mkString(",")}]"
}

abstract class GoalsBase extends Goals {
  protected val _goals = ArrayBuffer[Goal]()
  def +=(g: Goal): Unit = { _goals += g }
  def length: Int = _goals.length
  def apply(i: Int): Goal = _goals(i)
}

/** all goals start at the same time.  can complete in any order  */
class GoalGroup extends GoalsBase {
  def next(gh: GoalHandler): Unit = {}
  def startGoals: Iterable[Goal] = this
  def tick(te: Tick, gh: GoalHandler): Unit = {
    val len = _goals.length
    var i = 0
    while (i < len) {
      val g = _goals(i)
      if (!g.isCompleted) g.tick(te, gh)
      i += 1
    }
  }
}

/**
 * goals that must be completed in order for next to become active
 * @param time limit in ticks to complete this goal
 */
class GoalSeq(val time: Int = 120 s) extends GoalsBase {
  protected var _activeIndex = 0
  protected var _activeTime = 0

  /** ticks for current active goal */
  def ticks = _activeTime
  def next(gh: GoalHandler): Unit = {
    _activeIndex += 1
    _activeTime = 0
    if (_activeIndex == _goals.length)
      gh.goalGroupOver(this)
    else
      gh.goalStart(apply(_activeIndex))
  }
  override def reset(): Unit = {
    super.reset()
    _activeIndex = 0
    _activeTime = 0
  }
  def startGoals = this.take(1)
  def tick(te: Tick, gh: GoalHandler): Unit = {
    if (_activeIndex < _goals.length) {
      val g = _goals(_activeIndex)
      g.tick(te, gh)
      _activeTime += 1
      if (_activeTime >= time)
        gh.goalOver(g)
    }
  }
}
