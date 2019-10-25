package memnets.fx.games.wta

import memnets.model._
import org.junit._
import org.scalatest.MustMatchers
import org.scalatestplus.junit.JUnitSuite

import scala.reflect.Manifest

class BoardReaderTest extends JUnitSuite with MustMatchers with Logging {
  @Test def read: Unit = {
    implicit val sys = DynamicSystem()
    val (board, ctrl) = BoardReader("/games/wta/level1.data")
    assert(board.rows === 24)
    assert(board.cols === 24)
    assert(ctrl.length === 4)

    def check[S](r: Int, c: Int, i: Int = -1)(f: S => Any = (x: S) => {})(implicit man: Manifest[S]): S = {
      val items = board(r, c)
      if (i < 0)
        assert(items.length === 1, s"check at ($r, $c) failed")
      val bi = items(if (i < 0) 0 else i)
      val s = bi match {
        case s: S =>
          s
        case default =>
          assert(false, "wrong type: " + bi.getClass.getSimpleName)
          ???
      }
      f(s)
      s
    }
    // validate
    check[WormHole](0, 23) { wh =>
      var opt = sys.sparse.find(wh.start, board.grid(23, 0))
      assert(opt.isDefined)
      val cxn1 = opt.get
      assert(cxn1.w === 0.4)
      opt = sys.sparse.find(wh.start, board.grid(9, 12))
      assert(opt.isDefined)
      val cxn2 = opt.get
      assert(cxn2.w === 0.7)
    }
    check[Wall](1, 1)()
    check[Combo](2, 0) { cb =>
      assert(cb.ctl === ctrl(1))
    }

    check[Combo](3, 4, i = 0) { cb =>
      assert(cb.ctl === ctrl(1))
    }
    check[Wall](3, 4, i = 1)()

    check[Unknown](6, 10)()
    check[Combo](7, 13) { cb =>
      assert(cb.ctl == ctrl(3))
    }
    check[ConveyorBox](7, 15)()
    check[ConveyorBox](7, 16)()
    check[ConveyorBox](7, 17)()
    check[ConveyorBox](8, 17)()
    check[ConveyorBox](9, 17)()

    check[Wall](9, 12) { w =>
      val opt = sys.sparse.find(ctrl(2), w)
      assert(opt.isDefined)
      assert(opt.get.w === -100.0)
    }
    check[ConveyorBelt](20, 4)()
    check[ConveyorBelt](20, 5)()
    check[ConveyorBelt](20, 6)()
    check[ConveyorBelt](20, 7)()
    check[ConveyorBelt](20, 8, i = 1)()
    check[Wall](20, 8, i = 0)()

    sys.destroy()
  }
}
