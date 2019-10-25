package memnets.fx.games

import memnets.core.SciBranch._
import memnets.core._
import memnets.fx._
import memnets.fx.fx3d._
import memnets.model.Viz.User
import memnets.model._
import memnets.models.CommonTopics
import scalafx.scene._
import scalafx.scene.paint.Color
import scalafx.scene.shape.Cylinder
import scalafx.scene.transform.Rotate

import scala.collection.mutable.ArrayBuffer

object RezGlide extends Game {
  val game = ModelBuilder(
    "Rez Glide",
    Physics,
    ModelType.Game,
    tags = List("auditory science", "signal processing", "spectral analysis", "cochlear implants", "ear"),
    desc =
      """This game helps you to experience resonance directly.
        |
        |Glide your finger up and down on the touchscreen D-pad at the right rhythm to score points.
        |
        |Resonating ("Rez") with the target frequency moves the pendulum closer to the goal, but be careful to avoid the red targets.
        |
        |Press the "A" button to dampen all pendulums if you get too close to red.
        |
        |a touchpad/mouse can be used to drag the D-pad
        |up/down on the keyboard also controls the D-pad
        |""".stripMargin,
    topic = CommonTopics.resonance
  ) { b =>
    import b._

    var baseFreq = 0.8 // slow
    val defaultDampen = -0.17
    var dampen = defaultDampen
    val oscs = ArrayBuffer.tabulate(24) { i =>
      val osc = Osc(freq = baseFreq, damping = dampen)
      baseFreq = 1.05 * baseFreq
      dampen = 0.95 * dampen
      // want red (low) to ultraviolet (high)
      osc.x.ui.color = Color.hsb((320 + (i - 1) * 8) % 320, 0.8, 1.0, 1.0)
      osc
    }
    oscs.center(Loc(), 35)
    val user = Y("user", decay = -1.0, tau = 1.0)
    user.ui.viz = User
    user.ui.color = USER_COLOR

    user --> oscs w = 0.17
    user.ui.loc = Loc().down(200)

    val gamePad = DPad()
    gamePad.keyAdj = 12.0
    val gameButtons = GameButtons(1)
    gameButtons(0).onPressed { sel =>
      var dampen = if (sel) -1.0 else defaultDampen
      logr.debug("dampen adj = " + dampen)
      for (osc <- oscs) {
        osc.decay.w = dampen
        dampen = 0.95 * dampen
      }
    }
    user.f("dpad") { te =>
      6.0 * (gamePad.up - gamePad.down)
    }

    skin = new Skin3DFXAdapter(new PendulumSkin3DFX) {
      name = "Game"
      skin3d.showCanvas3d = false
      override def init(model: DynamicSystem): Unit = {
        // scene3d not valid until this call...
        super.init(model)
        sc3d.addContent(Plane3DFX(texture = Textures.DESKTOP, w = 512, d = 1024))
        sc3d.rotate = 90.0
        sc3d.tiltDown = 85.0
        sc3d.zoom = 2.0
        val user3D = new Group with Tickable3DFX {
          import memnets.fx.fx3d._
          import scalafx.Includes._
          val posColor = yColorMap(user).delegate
          val negColor = Color.Black
          val rod = new Cylinder(2.0, 500.0) with Phong
          rod.transforms += new Rotate(90.0, Rotate.XAxis)
          children += rod
          Textures(Textures.GOLD, rod, share = false)
          def node: Option[Node] = Some(this)
          def tick(te: Tick): Unit = {
            val act = user.act
            delegate.setTranslateX(15.0 * user.act)
            rod.phong.delegate.setDiffuseColor(if (act < -0.001) negColor else posColor)
          }
        }
        sc3d += user3D
      }
    }
    // returns tgt at i +/- range (j) and bad tgt at j-1
    def pick(i: Int, range: Int = 2): (Osc, Osc) = {
      val j = i.pick(range = range)
      logr.trace(f"pick $i +/-$range -> $j")
      (oscs(j), oscs(j - 1))
    }
    val amp = 5.5
    // NOTE : osc.length = 24

    // don't include blockers first 2 trials
    Trial(time = 2 m)
    // close together
    OscGoal(tgt = oscs(14), expected = amp)
    OscGoal(tgt = oscs(12), expected = amp)

    Trial(time = 2 m)
    // bit further apart
    val (f2a, _) = pick(21)
    OscGoal(tgt = f2a, expected = amp)
    val (f2b, _) = pick(5)
    OscGoal(tgt = f2b, expected = amp)

    Trial(time = 2 m)

    // start using blockers
    val (f3a, g3a) = pick(17)
    OscGoal(tgt = f3a, expected = amp)
    OscGoal(tgt = g3a, expected = amp, reward = -1)

    val (f3b, g3b) = pick(9)
    OscGoal(tgt = f3b, expected = amp)
    OscGoal(tgt = g3b, expected = amp, reward = -1)

    Trial(time = 3 m)

    val (f4a, g4a) = pick(21)
    OscGoal(tgt = f4a, expected = amp)
    OscGoal(tgt = g4a, expected = amp - 0.5, reward = -1)

    val (f4b, g4b) = pick(13)
    OscGoal(tgt = f4b, expected = amp)
    OscGoal(tgt = g4b, expected = amp - 1.0, reward = -1)

    val (f4c, g4c) = pick(4)
    OscGoal(tgt = f4c, expected = amp)
    OscGoal(tgt = g4c, expected = amp - 1.0, reward = -1)

    // extra +goal to go for 1st if bad w/ blockers.
    OscGoal(tgt = oscs(17), expected = 0.8 * amp)

    Trial(name = "flappy?", time = 6 m)
    for (i <- 0 until 8) {
      val j = i * 3
      OscGoal(tgt = oscs(j + 1), expected = amp)
      OscGoal(tgt = oscs(j), expected = amp - 1.0, reward = -1)
    }
  }
}
