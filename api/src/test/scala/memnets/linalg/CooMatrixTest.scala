package memnets.linalg

import breeze.linalg.DenseVector
import org.junit.Test
import org.scalatest.MustMatchers
import org.scalatestplus.junit.JUnitSuite

import scala.util.Random

class CooMatrixTest extends JUnitSuite with MustMatchers with Logging {
  @Test def mult: Unit = {
    val a = DenseVector(100.0f, 10.0f, 1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1000.0f)

    val matrix = CooMatrix.zeros[Float](rows = 5, cols = a.length)

    matrix(0, 0) = 1.0f
    matrix(0, 1) = 2.0f

    matrix(1, 2) = 3.0f

    matrix(4, 9) = 0.25f

    // 1.0, 2.0, 0.0, ......
    // 0.0, 0.0, 3.0, ......
    // 0.0, ...
    // 0.0, ...
    // 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.25

    val out = DenseVector.zeros[Float](size = matrix.rows)

    matrix * (a.data, out.data)

    logger.debug("out: " + out)
    assert(out(0) === 120.0f)
    assert(out(1) === 3.0f)
    assert(out(2) === 0.0f)
    assert(out(3) === 0.0f)
    assert(out(4) === 250.0f)

    val a2 = DenseVector(0.1f, 1.0f, 10.0f, 100.0f, 1000.0f)
    val out2 = DenseVector.zeros[Float](size = matrix.cols)

    matrix *^ (a2.data, out2.data)
    logger.debug("out2: " + out2)

    assert(out2(0) === 0.1f)
    assert(out2(1) === 0.2f)
    assert(out2(2) === 3.0f)
    assert(out2(9) === 250.0f)
  }
  @Test def sort: Unit = {
    val matrix = CooMatrix.zeros[Double](rows = 10, cols = 20)

    val len = 30
    for {
      i <- 0 until len
      r = Random.nextInt(matrix.rows)
      c = Random.nextInt(matrix.cols)
    } matrix(r, c) = Random.nextDouble()

    assert(len == matrix.activeSize)
    assert(len == matrix.activeIterator.length)

    logger.debug("unsorted " + matrix.activeKeysIterator.take(40).toList.mkString(","))

    def sortByColHelper(): Boolean = {
      var sorted = true
      for {
        window <- matrix.activeKeysIterator.sliding(2)
        (r, c) = window(0)
        (r2, c2) = window(1)
      } {

        val locsort =
          if (c < c2)
            true
          else if (c == c2)
            r <= r2
          else
            false
        sorted = sorted && locsort
      }
      sorted
    }
    assert(sortByColHelper() === false)

    matrix.sort(sortType = COOSortType.SortByCol)

    logger.debug("sorted " + matrix.activeKeysIterator.take(40).toList.mkString(","))

    assert(sortByColHelper() === true)

    def sortByRowHelper(): Boolean = {
      var sorted = true
      for {
        window <- matrix.activeKeysIterator.sliding(2)
        (r, c) = window(0)
        (r2, c2) = window(1)
      } {

        val locsort =
          if (r < r2)
            true
          else if (r == r2)
            c <= c2
          else
            false
        sorted = sorted && locsort
      }
      sorted
    }

    assert(sortByRowHelper() === false)

    matrix.sort(sortType = COOSortType.SortByRow)

    logger.debug("sorted " + matrix.activeKeysIterator.take(40).toList.mkString(","))

    assert(sortByRowHelper() === true)
  }

  @Test def apply: Unit = {
    val matrix = CooMatrix.zeros[Double](rows = 10, cols = 20)

    matrix(0, 0) = 1.0f
    matrix(0, 1) = 2.0f

    matrix(1, 2) = 3.0f
    matrix(3, 2) = 3.0f
    matrix(2, 2) = 3.0f

    matrix(7, 3) = 3.0f
    matrix(4, 3) = 3.0f
    matrix(9, 3) = 3.0f

    matrix(2, 5) = 0.3f
    matrix(5, 2) = 3.0f

    matrix(9, 4) = 0.50f
    matrix(4, 9) = 0.25f

    matrix(6, 4) = 0.50f
    matrix(6, 3) = 0.50f
    matrix(6, 2) = 0.25f

    matrix(7, 1) = 0.50f
    matrix(7, 6) = 0.50f
    matrix(7, 9) = 0.25f

    logger.debug("active " + matrix.activeIterator.toList.mkString(","))

    for (((r, c), w) <- matrix.activeIterator)
      withClue(s"($r, $c)= $w:  ") { matrix(r, c) must equal(w +- 1e-5) }

    matrix.sort()

    for (((r, c), w) <- matrix.activeIterator) {
      logger.debug(s"sorted apply ($r, $c)= $w")
      withClue(s"($r, $c)= $w:  ") { matrix(r, c) must equal(w +- 1e-5) }
    }

    // test empty value
    matrix(13, 1) must equal(0.0 +- 1e-5)
  }
}
