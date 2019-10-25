package memnets.fx.games.wta

import memnets.fx._
import memnets.linalg.W
import memnets.model._
import memnets.models.neuro.swta.SoftWTAGrid
import scalafx.scene.paint.Color

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Random

trait BoardItem
object Board {
  type BItems = List[BoardItem]
  val unknownColor = Color.DeepPink.sat(0.6)
}
class Board(val rows: Int, val cols: Int, val tau: Double = 5.0, val defaultThresh: Double = 4.0)(
    implicit val sys: DynamicSystem)
    extends GridLike[Board.BItems]
    with Logging {
  import Board._
  val grid = new SoftWTAGrid(rows, cols, tau = tau, unitViz = Viz.Skip)
  val memAct = (x: Double) => if (x < 0.0) 0.0 else if (x > 1.0) 1.0 else x
  val memTie = Param("mem", max = 1.0, init = 0.4)
  object memGrid extends YGrid(rows, cols) {
    override def create(r: Int, c: Int) = {
      val m = Y("mem", decay = 0.0, tau = 30.0 * tau, threshold = 0.2)
      m.out = memAct
      grid(r, c) --> m tie = memTie
      m.ui.color = Color.White
      m.ui.skip()
      m
    }
  }
  object thresGrid extends YGrid(rows, cols) {
    override def create(r: Int, c: Int) = {
      // not adding thresh here (in Wall) so can see pattern in Source
      val thres = Y("thres", decay = -1.0, act = Activation.Relu, tau = 10.0)
      thres.ui.skip()
      thres
    }
  }
  // can override to customize...
  /*
  def createGridSource(grid : YGrid, loc : Loc) : GridSource = {
    grid.skip()
    grid.loc = loc
    val gs = new GridSource(grid)
    gs.imageHints = ImageHints(scaleX = 0.6, scaleY = 0.6)
    sys.elements += gs
    gs
  }
   */
  def createGridSource(grid: YGrid, loc: Loc): YGrid = {
//    grid.skip()
    grid.loc = loc
    grid.hints = GridHints(scaleX = 0.6, scaleY = 0.6)
//    sys.elements += gs
    grid
  }

  val rowCtr = Loc().up(20)
  val thresSource = createGridSource(thresGrid, rowCtr.left(310))
  val gridSource = createGridSource(grid, rowCtr)
  val memSource = createGridSource(memGrid, rowCtr.right(310))

  val rowData = Array.fill[BItems](rows, cols) { Nil }
  def apply(r: Int, c: Int): BItems = rowData(r)(c)
  def update(r: Int, c: Int, bi: BoardItem): Unit = rowData(r)(c) = bi :: apply(r, c)
  def randPicks(n: Int): Seq[(Int, Int)] = for (i <- 0 until n) yield randPick()
  def randPick(): (Int, Int) = {
    var res: Option[(Int, Int)] = None
    while (res.isEmpty) {
      val pick = (Random.nextInt(rows), Random.nextInt(cols))
      if (apply(pick._1, pick._2).isEmpty)
        res = Some(pick)
    }
    res.get
  }
  val combos = ArrayBuffer[Combo]()
}
trait ConveyorBase extends BoardItem {}
case class Start(r: Int, c: Int)(implicit board: Board) extends BoardItem {
  board(r, c) = this
  board.grid(r, c).threshold = -0.001 // start loc
}
case class WormHole(row: Int, col: Int)(implicit board: Board) extends BoardItem with Linkable {
  board(row, col) = this
  val start = board.grid(row, col)
  start.ui.color = Color.web("#222")
  def src: Y = start
}
case class WallBox(row: Int, col: Int, rows: Int, cols: Int, skipRow: Int = -1, skipCol: Int = -1)(
    implicit board: Board) {
  val units = ListBuffer[Wall]()
  for {
    r <- 0 until rows
    c <- 0 until cols
  } {
    // one or the other here
    if (r != skipRow && c != skipCol) {
      if (r == 0 || r == rows - 1)
        units += Wall(row + r, col + c)
      else if (c == 0 || c == cols - 1)
        units += Wall(row + r, col + c)
    }
  }
}
case class ConveyorBelt(row: Int, col: Int, length: Int, color: Color = Color.Crimson)(implicit board: Board)
    extends ConveyorBase {
  import board.sys
  val units = (0 until length).map { i =>
    val c = col + i
    board(row, c) = this
    board.grid(row, c)
  }
  chain(units, 1.3)
  for (u <- units)
    u.ui.color = color
}
case class ConveyorBox(row: Int, col: Int, rows: Int, cols: Int, color: Color = Color.Crimson)(implicit board: Board)
    extends ConveyorBase
    with Logging {
  import board.sys
  val units = ListBuffer[Y]()
  logger.debug(s"ConveyorBox[r= $row, c= $col, rows= $rows, cols= $cols]")
  private def create(r: Int, c: Int): Y = {
    logger.debug(s"unit created: [$r, $c]")
    board(r, c) = this
    board.grid(r, c)
  }
  // top row
  for (c <- 0 until cols)
    units += create(row, col + c)
  // right ool
  for (r <- 1 until rows)
    units += create(row + r, col + cols - 1)
  // bottom // skip guy above
  for (c <- cols - 2 until -1 by -1)
    units += create(row + rows - 1, col + c)
  // left ool // skip guy above + 1st row
  for (r <- rows - 2 until 0 by -1)
    units += create(row + r, col)

  logger.debug("units size:" + units.length)
  chain(units, 1.3)
  for (u <- units)
    u.ui.color = color
}
case class Wall(row: Int, col: Int)(implicit board: Board) extends BoardItem with Linkable {
  board(row, col) = this
  val thres = board.thresGrid(row, col)
  thres.threshold = -board.defaultThresh
  val unit = board.grid(row, col)
  thres --> unit w = -10.0
  def src: Y = thres
}
case class Unknown(row: Int, col: Int)(implicit board: Board) extends BoardItem with Linkable {
  board(row, col) = this
  val unit = board.grid(row, col)
  val mem = board.memGrid(row, col)
  unit.ui.color = Board.unknownColor
  mem.ui.color = Board.unknownColor
  def src: Y = mem
}

case class Combo(prior: Y, ctl: Y)(implicit board: Board) extends BoardItem {
  board.combos += this
  private val entries = ListBuffer[(Y, Y, W)]()
  def size = entries.length
  def apply(i: Int): Y = entries(i)._2
  def apply(y: Y): Y = apply(entries.indexWhere(_._1 == y))
  def update(i: Int, v: Double): Unit = { entries(i)._2.update(v) }
  def add(r: Int, c: Int): Unit = {
    board(r, c) = this
    val u = board.grid(r, c)
    val m = board.memGrid(r, c)
    u.ui.color = ctl.ui.color.get
    m.ui.color = ctl.ui.color.get
    val m2c = m --> ctl
    val tuple = (u, m, m2c)
    entries += tuple
    val size = entries.length
    for (link <- entries) link._3.w = 1.0 / size
    ctl --> m w = -0.3 // turn off
  }
}
case class ComboInput(combo: Combo, start: Int, msg: String = "", dur: Int = 300) extends Logging {
  val indices = 0 until combo.size
  def tick(t: Int): Unit = {
    val t2 = t - start
    if (t2 >= 0 && t2 <= dur) {
      if (t2 == 0)
        logger.debug(this.toString)
      for (i <- indices) {
        val t3 = t2 - i * 100
        val amp = Math.max(0.0, t3 * 0.03)
        combo(i) = if (amp > 1.0) 1.0 else amp
      }
    } else if (t2 == 20) {
      for (i <- indices)
        combo(i) = 0.0
    }
  }
  override def toString: String = s"ComboInput[start=$start, msg= $msg]"
}
