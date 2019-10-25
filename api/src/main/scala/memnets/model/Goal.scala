package memnets.model

import java.lang.Math._

import memnets.model.impl._

import scala.beans.BeanProperty

trait Goal extends Element with TickFunction {
  type T <: AnyRef
  def startTick: Int
  def startTick_=(tick: Int): Unit
  def completedTick: Int
  def completedTick_=(tick: Int): Unit
  def owner: Goals
  def progress: Double
  def progress_=(d: Double): Unit
  def reward: Int
  def reset(): Unit = {
    progress = 0.0
    startTick = 0
    completedTick = 0
  }
  def tgt: T
  def tick(te: Tick, gh: GoalHandler): Unit = {
    if (progress < 1.0) {
      progress = min(1.0, eval(te))
      if (progress > 0.99) {
        progress = 1.0
        gh.goalOver(this)
      }
    }
  }
  def isGood: Boolean = reward > 0
  def isCompleted: Boolean = progress >= 1.0
  override def toString = s"Goal[tgt: $tgt]"
}

trait YGoal extends Goal {
  type T = Y
  var linkable: Option[Linkable] = None
  @BeanProperty var expected: Double = 1.0
  def eval(te: Tick): Double = max(0.0, tgt.act / expected)
}

object Goal {
  def apply(tgt: AnyRef, reward: Int = 1, desc: String = EMPTY_STRING)(eval: TickFunction)(
      implicit tri: Trial): Goal = {
    implicit val gg = tri.lastGoals
    val g = new GoalImpl(tgt, eval)
    g.desc = desc
    g.reward = reward
    g
  }
}

object YGoal {
  def apply(tgt: Y, expected: Double, reward: Int = 1, desc: String = EMPTY_STRING, linkable: Linkable = null)(
      implicit tri: Trial): YGoal = {
    implicit val gg = tri.lastGoals
    val g = new YGoalImpl(tgt)
    g.desc = desc
    g.expected = expected
    g.reward = reward
    g
  }
}

object OscGoal {
  def apply(tgt: Osc, expected: Double, reward: Int = 1, desc: String = EMPTY_STRING)(implicit tri: Trial): YGoal = {
    val yg = YGoal(tgt.y, expected, reward, desc)
    yg.linkable = Option(tgt)
    yg
  }
}
