package memnets.core

import memnets.model._
import memnets.ui._
import org.junit._
import org.scalatest.MustMatchers
import org.scalatestplus.junit.JUnitSuite

import scala.collection.mutable.ArrayBuffer

class EngineTest extends JUnitSuite with MustMatchers with Logging {
  type MS = Skin[Unit, String]
  type MT = TickableUI[Unit]
  implicit def m2opt(mt: MT): Option[MT] = Option(mt)
  @Test def engine: Unit = {
    object mock extends ModelUIBase[Unit, MS] {
      type COL = String
      def defaultSkin: MS = NullSkin
      override def tick(te: Tick): Unit = {
        super.tick(te)
        ticks += 1
        _dirty = false
      }
      override def reset(fullReset: Boolean): Unit = {
        super.reset(fullReset)
        resets += 1
      }
      def clearDirty(): Unit = {
        _dirty = false
      }
      var ticks = 0
      var resets = 0
      def addHelper(tfx: T): Unit = {}
    }
    val engine = new Engine(modelUI = mock, appUI = NullAppUI)
    assert(engine.isPlaying === false)
    assert(engine.trialsModel.getItemCount === 0)
    val engineEvents = ArrayBuffer[EngineEvent]()
    val time = 1000
    val builder = ModelBuilder { b =>
      import b._
      val y = Y()
      onEvent = e => {
        logger.debug("engine event = " + e)
        engineEvents += e
      }
      Trial(time = time)
      Trial(time = 2 * time)
      Step(y = y, on = 1)
      YGoal(tgt = y, expected = 0.9)
    }
    assert(engine.model === null)
    engine.setBuilder(builder)
    val cxt = engine.model
    assert(cxt !== null)
    assert(cxt.system !== null)
    assert(engine.trialsModel.getItemCount === cxt.trials.size)

    assert(engine.trial === cxt.trials.head)
    assert(engine.sim.trial === engine.trial)
    assert(mock.system === cxt.system)
    assert(mock.trial === engine.trial)
    assert(engine.isPlaying === true)

    assert(cxt.system.variables.length === 1)
    assert(mock.find(cxt.system.variables(0)).isDefined)
    assert(mock.tickables.size === 1)

    assert(engineEvents.size > 0)
    logger.debug("events = " + engineEvents.mkString(","))

    engineEvents.clear()
    assert(engineEvents.size === 0)
    assert(mock.dirty === true)

    mock.clearDirty()

    logger.debug("change trial")
    engine.changeTrial(next = true)
    assert(engine.trial === cxt.trials(1))
    assert(mock.system === cxt.system)
    assert(mock.trial === cxt.trials(1))
    assert(engine.trial.goals.size === 1)
    assert(mock.tickables.size === 2)
    assert(mock.find(engine.trial.goals.head.head).isDefined)
    logger.debug(engineEvents.mkString(","))
    assert(engineEvents.size === 6)
    assert(engineEvents(0).isInstanceOf[TrialChangeEvent])
    assert(engineEvents(1).isInstanceOf[TrialEvent])
    assert(engineEvents(2).isInstanceOf[ResetEvent])
    assert(engineEvents(3).isInstanceOf[GoalsEvent])
    assert(engineEvents(4).isInstanceOf[GoalEvent])
    assert(engineEvents(5).isInstanceOf[GameIntroOverEvent])

    assert(mock.dirty === true)
    engineEvents.clear()
    engine.setPlaying(false)
    assert(engine.isPlaying === false)
    mock.clearDirty()
    assert(mock.ticks === 0)
    assert(mock.dirty === false)
    engine.tick(1L)
    assert(mock.ticks === 0, "no tick() when paused")

    engine.process(TogglePlayEvent())
    assert(engine.isPlaying === true)

    engineEvents.clear()
    assert(mock.ticks === 0)
    assert(mock.dirty === false)
    assert(engine.trial.inputs.size === 1)

    engine.tick(2L)
    assert(mock.ticks === 1, "tick() when play")

    assert(engineEvents.size === 1)
    assert(engineEvents(0).isInstanceOf[SignalEvent])
    assert(mock.tickables.size === 3)
    assert(mock.find(engine.trial.inputs.head).isDefined)

    // todo: test ICS...
  }
}
