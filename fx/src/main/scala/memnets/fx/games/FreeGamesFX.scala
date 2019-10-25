package memnets.fx.games

import memnets.core.SciBranch.ComputerSci
import memnets.core._
import memnets.fx._
import memnets.fx.fx3d._
import memnets.fx.games.wta.NeuralOcean
import memnets.ml.PageRank
import memnets.model._
import memnets.models.biology.CoupledMotors
import memnets.ui.{Phase3D, Skin}
import scalafx.scene.Node
import scalafx.scene.paint.Color

import scala.collection.mutable.ListBuffer
import scala.math.random

object FreeGamesFX extends Library {
  this._builders += RezGlide
  this._builders += NeuralOcean

  // extra models relying on fx thrown in here
  val coupledMotor = ModelBuilder(
    "Coupled Motor Neurons",
    SciBranch.Biology,
    tags = List("phase transition", "nonlinear", "Kelso"),
    library = this
  ) { b =>
    import b._

    val coupMotors = new CoupledMotors()
    import coupMotors._

    e.ui.color = Color.Aqua.sat(0.8)
    i.ui.color = Color.Aqua.sat(0.2)
    e2.ui.color = Color.CornflowerBlue.sat(0.8)
    i2.ui.color = Color.CornflowerBlue.sat(0.4)

    loc = Loc().up(10).left(300)
    tracked ++= Seq(e, e2)
    Phase3D(e, e2, i, scale = 20)
    phasePlot.scale = 0.8
    phasePlot.zoom = 1.4
    phasePlot.onCreateScene = { sc =>
      val scene = sc.asInstanceOf[Scene3DFX]
      val rad = Pendulum3DFX.RADIUS
      val efx = new Pendulum3DFX(e, i, e.ui.colorfx, rad)
      val efx2 = new Pendulum3DFX(e2, i2, e2.ui.colorfx, rad)
      efx.translateZ = rad
      efx2.translateZ = -rad
      scene += (efx, efx2)
    }

    skin = Skin { x =>
      x.backImageOn = false
      x.zoom = 1.5
    }

    Step(e, on = 0, scale = 5.0)
  }
  val pageRankWeb = ModelBuilder(
    "Scale-free Network",
    ComputerSci,
    tags = Seq("machine learning") ++ List("preferential attachment", "small world", "scale-free"),
    library = this
  ) { cxt =>
    import cxt._

    val pageRank = new PageRank()
    import pageRank._

    val rows = 25
    val cols = 40
    val n = rows * cols
    val pad = 30.0
    val xSpacing = (Display.width - pad) / cols
    val ySpacing = (Display.height - pad) / rows
    val colors: Double = 320.0 / cols
    val notAdded = ListBuffer.tabulate(n) { i =>
      val p = page("Page" + i)
      val r = i / cols
      val c = i % cols
      p.ui.color = Colorf.hsb((c * colors) % 320, 0.8, 1.0, 1.0)
      p.ui.loc = Loc(x = pad + c * xSpacing, y = pad + r * ySpacing)
      p
    }
    // start w/ two randoms
    val added = ListBuffer[Y](notAdded.randomPicks(n = 2): _*)

    // keep track to avoid expensive query
    val inCounts = Array.ofDim[Int](notAdded.length)
    added.head --> added.last
    inCounts(added.last.id) += 1

    notAdded -= added.head
    notAdded -= added.last

    tracked ++= added

    for (p <- notAdded) {
      val inSum = inCounts.sum
      for ((p2, in) <- added.zip(inCounts)) {
        if (random() > in / inSum) {
          p --> p2
          inCounts(p2.id) += 1
        }
      }
      added += p
    }
    // pagerank keeps own list of pages created, so ok that notAdded empty
    updateWeights()
    // just to signal no auto
    system.onLayout = {
      // do nothing
      logr.debug("nothing")
    }

    //    val cir = new CircleSkin(showEdges = false) with FadingSkin
    val cir = new CircleSkinFX(rad = 10.0, showEdges = false)
    cir.topPaneOn = false
    cir.style = "-fx-background-color: rgba(0.0,0.0,0.0,0.9); -fx-background-image: null;" // no back
    skin = cir

    system.elements += new ElementBase with TickableFX {
      def node: Option[Node] = None
      var topK = pages.randomPicks(1)
      def tick(te: Tick): Unit = {
        if (te.t % 60 == 0) {
          logr.debug("new top K")
          topK = pages.randomPicks(1)
        }
        val gc = RenderContextFX.gc
        val r = cir.rad
        gc.globalAlpha = 0.7
        for (ys <- topK) {
          val act = ys.act
          val x = ys.ui.loc.x
          val y = ys.ui.loc.y
          for (e <- te.system.sparse.outEdges(ys)) {
            val tgt = te.system.variables(e.tgt)
            val tLoc = tgt.ui.loc
            val w: Double = e.w
            var lw = (act * w) / 0.01
            if (lw > 6.0) lw = 6.0
            // todo : not right...
            val colfx = ys.ui.colorfx
            gc.stroke = if (colfx != null) colfx else Color.Gray
            gc.lineWidth = lw
            gc.strokeLine(x, y, tLoc.x, tLoc.y)
          }
        }
      }
    }
  }
}
