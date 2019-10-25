package memnets.model

import org.junit.Test
import org.scalatest.MustMatchers
import org.scalatestplus.junit.JUnitSuite

class KernelTest extends JUnitSuite with MustMatchers {
//  val epsilon = 1e-10d
//  implicit val floatEq = TolerantNumerics.tolerantDoubleEquality(epsilon)
  def check(w: Double, indices: Int*)(implicit sys: DynamicSystem): Unit = {
    for (win <- indices.map(sys.variables(_)).sliding(2)) {
      val e = sys.sparse.find(win(0), win(1))
      assert(e.isDefined, s"${win(0)} --> ${win(1)}")
      assert(e.get.w === w)
    }
  }
  @Test def one_wrap: Unit = {
    implicit val sys = DynamicSystem()
    val data = Array.fill(4) { Y() }
    val ffTie = Param("ff", 1.8, 0.4)
    chain(data, ktype = KernelType.Wrap, tie = ffTie)
    check(ffTie.getValue, 0, 1, 2, 3, 0)
  }
  @Test def one_nowrap: Unit = {
    implicit val sys = DynamicSystem()
    val data = Array.fill(4) { Y() }
    val ffTie = Param("ff", 1.8, 0.4)
    chain(data, ktype = KernelType.NoWrap, tie = ffTie)
    check(ffTie.getValue, 0, 1, 2, 3)
    val e = sys.sparse.find(sys.variables(3), sys.variables(0))
    assert(e.isEmpty)
  }
  @Test def two_wrap: Unit = {
    implicit val sys = DynamicSystem()
    val data = Array.fill(4) { Y() }
    val w = 0.5
    kernel(data, KernelType.Wrap, w)
    check(w, 0, 1, 2, 3, 0)
    check(w, 3, 2, 1, 0, 3)
  }
  @Test def four_wrap: Unit = {
    implicit val sys = DynamicSystem()
    val data = Array.fill(4) { Y() }
    val initCount = sys.sparse.matrix.weights.length
    val w = 0.3
    val w2 = 0.5
    kernel(data, KernelType.Wrap, w, w2)
    check(w, 0, 1, 2, 3, 0)
    check(w, 3, 2, 1, 0, 3)
    check(w2, 0, 2, 0)
    check(w2, 1, 3, 1)
    check(w2, 2, 0, 2)
    check(w2, 3, 1, 3)
    assert(sys.sparse.matrix.weights.length - initCount === 16, "")
  }
  @Test def four_nowrap: Unit = {
    implicit val sys = DynamicSystem()
    val data = Array.fill(5) { Y() }
    val w = 0.3
    val w2 = 0.5
    val initCount = sys.sparse.matrix.weights.length
    kernel(data, KernelType.NoWrap, w, w2)
    check(w, 2, 3)
    check(w, 2, 1)
    check(w2, 2, 4)
    check(w2, 2, 0)
    assert(sys.sparse.matrix.weights.length - initCount === 4, "")
  }
}
