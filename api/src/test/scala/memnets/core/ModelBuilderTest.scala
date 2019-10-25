package memnets.core

import memnets.model._
import org.junit.Test
import org.scalatest.MustMatchers
import org.scalatestplus.junit.JUnitSuite

class ModelBuilderTest extends JUnitSuite with MustMatchers with Logging {
  @Test def trial: Unit = {
    val time = (1 m) + (5 s)
    val tname = "testTrial"
    val builder = ModelBuilder("test", SciBranch.Astronomy) { b =>
      import b._
      val y = Y()
      val trial = Trial(time = time, name = tname)
      assert(trial.ticks === time)
      assert(trial.name === tname)
    }
    val model = builder.build()
    assert(model.trials.size === 2)
    val trial = model.trials.head
    assert(trial.ticks === time)
    assert(trial.name === tname)
    assert(trial.description === tname)
    assert(trial.getTimeText === "01:05")
  }
  @Test def trial_auto_name: Unit = {
    val builder = ModelBuilder("test", SciBranch.Astronomy) { b =>
      import b._
      val y = Y()
      Trial()
    }
    val model = builder.build()
    assert(model.trials.size === 2)
    assert(model.trials(0).name === "")
    assert(model.trials(0).description === "Trial 1")
    assert(model.trials(1).name === "no inputs")
    assert(model.trials(1).description === "no inputs")
  }
  @Test def trials: Unit = {
    val time = 5000
    val builder = ModelBuilder("test", SciBranch.Astronomy) { b =>
      import b._
      val y = Y()
      Trial(time = time)
      Trial(time = 2 * time)
    }
    val model = builder.build()
    assert(model.trials.size === 3)
    assert(model.trials(0).ticks === time)
    assert(model.trials(1).ticks === 2 * time)
  }
  @Test def reset: Unit = {
    val tname = "testTrial"
    val mname = "testModel"
    val builder = ModelBuilder(mname, SciBranch.Astronomy) { b =>
      import b._
      val y = Y()
      Trial(time = 1000, name = tname)
    }
    var model = builder.build()
    assert(model.trials.size === 2)
    var trial = model.trials.head
    assert(trial.name === tname)
    val modelA = model.system
    assert(modelA.variables.length === 1)
    assert(model.name === mname)

    model = builder.build()
    assert(model.trials.size === 2)
    trial = model.trials.head
    assert(trial.name === tname)
    val modelB = model.system
    assert(modelB.variables.length === 1)
    assert(model.name === mname)
    assert(modelA !== modelB)

  }

  @Test def goal: Unit = {
    val gname = "testgoal"
    val builder = ModelBuilder("test", SciBranch.Astronomy) { b =>
      import b._
      val y = Y()
      val goal = YGoal(tgt = y, expected = 3.0, reward = 6, desc = gname)
      assert(goal.tgt === y)
      assert(goal.expected === 3.0)
      assert(goal.reward === 6)
      assert(goal.description === gname)
      assert(goal.isGood === true)
      assert(goal.isCompleted === false)
      assert(goal.progress === 0.0)
    }
    val model = builder.build()
    assert(model.trials.size === 2)
    val trial = model.trials.head
    assert(trial.goals.size === 1)
    assert(trial.goals.head.size === 1)
    assert(trial.goals.head.head.description === gname)
  }
  @Test def signal_ic: Unit = {
    val builder = ModelBuilder("test", SciBranch.Astronomy) { b =>
      import b._
      val y = Y()
      // dur is ignored for IC
      val sig = Step(y = y, on = 0, dur = 1, scale = 5.0)
      assert(sig.isUser === false)
      assert(sig.on === 0)
      assert(sig.off === 1)
      assert(sig.scale === 5.0)
    }
    val model = builder.build()
    assert(model.trials.size === 2)
    val trial = model.trials.head
    assert(trial.inputs.size === 1) // ics are in inputs
    assert(trial.ics.size === 1)
  }
  @Test def signal: Unit = {
    val builder = ModelBuilder("test", SciBranch.Astronomy) { b =>
      import b._
      val y = Y()
      // dur is ignored for IC
      val sig = Step(y = y, on = 5, dur = 200, scale = 5.0)
      assert(sig.isUser === false)
      assert(sig.on === 5)
      assert(sig.off === 205)
      assert(sig.period === 200)
      assert(sig.scale === 5.0)
    }
    val model = builder.build()
    assert(model.trials.size === 2)
    val trial = model.trials.head
    assert(trial.inputs.size === 1)
    assert(trial.ics.size === 0)
  }
}
