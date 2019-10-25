package memnets.fx.games.wta

import memnets.fx._
import memnets.model.{stringToOpt => _, _}
import memnets.models.neuro.swta.SoftWTA
import memnets.utils._
import scalafx.scene.paint.Color

object BoardReader extends Logging {
  implicit class CharExt(val ch: Char) extends AnyVal {
    def parse: Int = Integer.parseInt(ch.toString)
  }
  val digits = "123456789"
  val letters = "ABCDEFGHI"
  val letters2 = "ZYXWVUTSR"
  def apply(file: String, tau: Double = 5.0)(implicit sys: DynamicSystem): (Board, SoftWTA) = {
    var rows, cols, n = 0
    val meta = file.changeFileExt("meta")
    meta.processStream { (i, line) =>
      if (i == 0) {
        val info = line.split(",").map(Integer.parseInt(_))
        rows = info(0)
        cols = info(1)
        n = info(2)
      } else
        ???
    }
    // create ctrl first so y.id easier to debug
    val ctrl = new SoftWTA(n = n + 1, tau = tau)
    val colMap = YGradientFX(GradientHints(hue = 0, minDivs = 4, spectrum = 130, saturation = 0.8f))
    for ((e, i) <- ctrl.excites.zipWithIndex) {
      e.ui.color = colMap(i, ctrl.length)
      e.threshold = if (i == 0) 0.6 else 0.7 // assumes 3 combos + ff*ss
    }
    ctrl.head.ui.color = Color.Gray
    ctrl.loc = Loc().down(190)
    ctrl.e2eT.value = 0.2 // make sure 0.2
    val ctrlFF = Param(name = "ctrlFF", max = 2.0, init = 0.05)
    chain(ctrl, ctrlFF, KernelType.NoWrap)

    implicit val board = new Board(rows = rows, cols = cols)
    val combos = Array.tabulate(n) { i =>
      Combo(ctrl(i), ctrl(i + 1))
    }

    file.processStream { (i, line) =>
      require(line.length == cols, s"line $i has length = ${line.length}")
      var c = 0
      val r = i
      while (c < cols) {
        val ch = line.charAt(c)
        ch match {
          case '.' =>
          case '+' | '=' =>
            Wall(r, c)
          case '!' =>
            Unknown(r, c)
          case '@' =>
            WormHole(r, c)
          case '>' =>
            val len = line.charAt(c + 1).parse
            ConveyorBelt(r, c, len)
            c += 1 // advance position
          case '<' =>
            val rows2 = line.charAt(c + 1).parse
            val cols2 = line.charAt(c + 2).parse
            ConveyorBox(r, c, rows2, cols2)
            c += 2 // advance position
          case '$' =>
            Start(r, c)
          case dig if digits.contains(dig) =>
            combos(digits.indexOf(dig)).add(r, c)
          case let2 if letters2.contains(let2) =>
            // combo hidden by wall
            Wall(r, c)
            combos(letters2.indexOf(let2)).add(r, c)
          case let if letters.contains(let) =>
            // wall hidden when active ctl
            val wall = Wall(r, c)
            val pos = letters.indexOf(let)
            val ctl = ctrl(pos + 1)
            ctl --> wall w = -100.0
            logger.debug(s"ctl hide wall: $pos, ($r, $c)")
          case default =>
            logger.warn(s"unrecognized data ($r,$c) = " + default)
            ???
        }
        c += 1
      }
    }
    val cxn = file.changeFileExt("cxn")
    cxn.processStream { (i, line) =>
      if (!line.startsWith("//") && !line.trim.isEmpty) {
        val data = line.split(",").map(Integer.parseInt(_))
        require(data.length == 6, s"line $i: bad data length ${data.length}")
        // YIKES! helper method...
        def tgtWall(): Wall = board(data(3) - 1, data(4) - 1).find(_.isInstanceOf[Wall]).get.asInstanceOf[Wall]
        def parseW() = data(5).toDouble / 10.0
        data(2) match {
          case -1 =>
            // mainly wormholes use this
            board.grid(data(0) - 1, data(1) - 1) --> board.grid(data(3) - 1, data(4) - 1) w = parseW()
          case -2 =>
            // targets are always walls
            board.memGrid(data(0) - 1, data(1) - 1) --> tgtWall() w = parseW()
          case -3 =>
            // src and target are walls
            val srcWall = board(data(0) - 1, data(1) - 1).find(_.isInstanceOf[Wall]).get.asInstanceOf[Wall]
            srcWall --> tgtWall() w = parseW()
        }
      }
    }
    (board, ctrl)
  }
}
