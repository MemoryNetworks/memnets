package memnets.model

import org.junit.Test
import org.scalatest.MustMatchers
import org.scalatestplus.junit.JUnitSuite

class GridLikeTest extends JUnitSuite with MustMatchers {
  object TestGrid extends GridLike[Int] {
    val data = Array.tabulate(4) { j =>
      val i = j + 1
      Array(1 * i, 2 * i, 3 * i, 4 * i, 5 * i, 6 * i, 7 * i, 8 * i, 9 * i, 10 * i)
    }
    def rows = data.length
    def cols = data(0).length
    def apply(r: Int, c: Int): Int = data(r)(c)
  }
  @Test def subGridIdentity: Unit = {
    val identity = TestGrid.subGrid(TestGrid.rows, TestGrid.cols)
    assert(identity.rows === TestGrid.rows)
    assert(identity.cols === TestGrid.cols)

    assert(identity(0, 0) === 1)
    assert(identity(0, 1) === 2)
    assert(identity(0, 9) === 10)

    assert(identity(1, 0) === 2)
    assert(identity(1, 1) === 4)
    assert(identity(1, 2) === 6)
    assert(identity(1, 9) === 20)
  }
  @Test def subGrid23: Unit = {
    val sub = TestGrid.subGrid(2, 3)
    assert(sub.rows === 2)
    assert(sub.cols === 3)

    assert(sub(0, 0) === 1)
    assert(sub(0, 1) === 5)
    assert(sub(0, 2) === 10)

    assert(sub(1, 0) === 4)
    assert(sub(1, 1) === 20)
    assert(sub(1, 2) === 40)
  }
  @Test def subGrid25: Unit = {
    val sub = TestGrid.subGrid(2, 5)
    assert(sub.rows === 2)
    assert(sub.cols === 5)

    assert(sub(0, 0) === 1)
    assert(sub(0, 1) === 3)
    assert(sub(0, 2) === 5)
    assert(sub(0, 3) === 7)
    assert(sub(0, 4) === 10)

    assert(sub(1, 0) === 4)
    assert(sub(1, 1) === 12)
    assert(sub(1, 2) === 20)
    assert(sub(1, 3) === 28)
    assert(sub(1, 4) === 40)
  }

}
