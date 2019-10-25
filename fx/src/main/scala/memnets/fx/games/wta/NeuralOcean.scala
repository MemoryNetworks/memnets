package memnets.fx.games.wta

import memnets.core._
import memnets.model._
import memnets.models.CommonTopics

object NeuralOcean extends Game {
  val FILE_KEY = "FILE_KEY"
  val SPLASH_LITE = "SPLASH_LITE"
  val game = ModelBuilder(
    "Neural Ocean",
    SciBranch.Neuroscience,
    ModelType.Game,
    tags = Seq("sWTA", "memory traces", "dynamic kernel"),
    desc = """This game pioneers a whole new application of dynamic systems: puzzle games.
             |
             |Solve a stage by finding all 3 colored spheres and boosting each to leave a trace.
             |
             |If the traces are strong enough, together they will combine to unlock the next stage.
             |
             |Make sure to leave a strong trace at each sphere, and move quickly to the next sphere.
             |
             |Some spheres are hidden underneath walls.  You'll have to figure out how to unlock them.
             |
             |Watch out for triangle conveyors that can trap you (might need to reset to start position).
             |
             |Wormholes can help quickly transport you around the board.
             |
             |Each stage may or may not alter how the board works...
             |
             |>>> CONTROLS <<<
             |Use the touchscreen D-pad or direction keys to move the peak around.
             |A touchpad/mouse can also be used to drag the D-pad.
             |
             |Toggle the touchscreen "A" button, click on it, or press spacebar or "1" key to boost.
             |Toggle the touchscreen "B" button, click on it, or press "2" key to reset.
             |
             |Touch input only responds to one input at a time, so temporarily let go of D-pad to toggle
             |
             |>>> INFO <<<
             |
             |This dynamic system combines:
             |
             |soft-winner-take-all (sWTA)
             |dynamic kernels
             |long-term memory traces
             |inhibition units
             |""".stripMargin,
    topic = CommonTopics.swtaTopic
  ) { b =>
    import b._

    val gamePad = DPad()
    val buttons = GameButtons(2)
    val boost = buttons(0)
    val restart = buttons(1)

    val file = fileHint("/games/wta/level1.data")
    val (board, ctrl) = BoardReader(file)
    import board.{sys => _, _}
    track(ctrl: _*)

    val splash = Sound(SPLASH_LITE)
    boost.onPressed { sel =>
      grid.boostTie.value = if (sel) -2.0 else 0.0
      if (sel) splash.start(0.1)
    }
    restart.onPressed { sel =>
      grid.boostTie.value = if (sel) 1.0 else 0.0
    }
    system.onReset = {
      ctrl(0).update(4.0)
    }
    val amp = 1.3
    system.onTick = { te =>
      grid.rightTie.value = gamePad.right * amp
      grid.leftTie.value = gamePad.left * amp
      grid.upTie.value = gamePad.up * amp
      grid.downTie.value = gamePad.down * amp
    }

    skin = new NeuralOceanSkin(board, ctrl)

    Trial(time = 5 m)
    for (c <- ctrl.tail)
      YGoal(tgt = c, expected = 5.0, reward = 1)

    // assumes level 1
    lazy val testInputs = Array(
      ComboInput(combos(0), start = 300, "1st combo"),
      ComboInput(combos(2), start = 800, "Should have no effect"),
      ComboInput(combos(1), start = 1300, "2nd combo"),
      ComboInput(combos(2), start = 1800, "3rd combo"),
    )
    validator = { te =>
      val tinputs = testInputs
      if (te.t > 100 && te.t < 400)
        combos(0)(0).update(1.0)
      var i = 0
      while (i < tinputs.length) {
        tinputs(i).tick(te.t)
        i += 1
      }
    }
  }
}
