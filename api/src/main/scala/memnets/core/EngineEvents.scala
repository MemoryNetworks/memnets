package memnets.core

import memnets.model._
import memnets.ui.SkinType

import scala.beans._

class EngineEvent
trait EngineListener { def process(ee: EngineEvent): Unit }

case class ErrorEvent(@BeanProperty msg: String = "error", @BeanProperty ex: Throwable) extends EngineEvent
case class FlashTitleEvent(@BeanProperty repeat: Int = 1) extends EngineEvent
case class GameEndEvent(@BeanProperty t: Int, @BooleanBeanProperty win: Boolean) extends EngineEvent
object GameIntroOverEvent { def apply() = new GameIntroOverEvent() }
class GameIntroOverEvent extends EngineEvent
case class GoalsEvent(@BeanProperty goals: Goals, @BooleanBeanProperty start: Boolean = true) extends EngineEvent
case class GoalEvent(@BeanProperty goal: Goal, @BooleanBeanProperty start: Boolean = true) extends EngineEvent

/** used during task when UI has progress dialog  */
case class ProgressEvent(
    @BeanProperty msg: String,
    @BeanProperty workDone: Double = -1.0,
    @BeanProperty max: Double = 1.0)
    extends EngineEvent
object MessageEvent {
  def apply(desc: Descriptable) = new MessageEvent(desc.description)
}
case class MessageEvent(@BeanProperty msg: String) extends EngineEvent
case class RealSampleEvent(@BeanProperty rt: RealTime, @BeanProperty act: Double) extends EngineEvent
case class RealStartEvent(
    @BeanProperty y: Yb,
    @BeanProperty elem: Element,
    @BeanProperty src: UserSource,
    @BeanProperty init: Option[Float] = None,
    @BooleanBeanProperty touch: Boolean = false)
    extends EngineEvent
case class RealEndEvent(@BeanProperty rt: RealTime, @BooleanBeanProperty touch: Boolean = false) extends EngineEvent
case class ResetEvent(@BeanProperty trial: Trial) extends EngineEvent
class EditorEvent extends EngineEvent
class LibraryEvent extends EngineEvent

/** NOTE : y can be null */
case class SelectEvent(@BeanProperty y: Y) extends EngineEvent
case class SpeedEvent(@BooleanBeanProperty inc: Boolean) extends EngineEvent
case class TogglePlayEvent(@BeanProperty msg: String = "") extends EngineEvent
case class TrialDoneEvent(@BeanProperty trial: Trial) extends EngineEvent
case class TrialChangeEvent(@BooleanBeanProperty next: Boolean) extends EngineEvent
case class TrialEvent(@BeanProperty trial: Trial) extends EngineEvent
case class BuilderEvent(@BeanProperty builder: ModelBuilder) extends EngineEvent
case class SignalEvent(@BeanProperty signal: Signal, @BooleanBeanProperty on: Boolean) extends EngineEvent

/**
 * needed for corner case where want notify AFTER skin.init(model) call by sysUI in rebuild
 * the primary driver for this event is SkinEditor.
 * the rest of system listens to Engine.skinsModel
 */
case class SkinBuiltEvent(@BeanProperty skin: SkinType) extends EngineEvent
case class BuiltModelEvent(@BeanProperty model: BuiltModel) extends EngineEvent

class CaptureEvent extends EngineEvent
