package memnets.linalg

import java.util

import breeze.linalg
import breeze.linalg.{Matrix, MatrixLike, View}
import memnets.utils._

import scala.reflect.ClassTag

object CooMatrix {
  def zeros[@specialized(Float, Double) T: ClassTag](rows: Int, cols: Int): CooMatrix[T] = new CooMatrix[T](rows, cols)
}

final class CooMatrix[@specialized(Float, Double) T: ClassTag](val rows: Int, val cols: Int, initCapacity: Int = 256)
    extends CooMatrixLike[T]
    with Matrix[T]
    with MatrixLike[T, CooMatrix[T]]
    with Logging {
  override def repr: CooMatrix[T] = this

  // prevents compiler from boxing T
  implicit def d2T(d: Double): T = d.asInstanceOf[T]
  implicit def t2D(t: T): Double = t.asInstanceOf[Double]
  private var default: T = _
  private var _activeSize = 0
  private[memnets] var _rowData = Array.ofDim[Int](initCapacity)
  private[memnets] var _colData = Array.ofDim[Int](initCapacity)
  private[memnets] var _wData = Array.ofDim[T](initCapacity)
  private[memnets] var _rowIndex = Array.ofDim[Int](rows)
  private[memnets] object _activeIndexed extends IndexedSeq[((Int, Int), T)] {
    def length: Int = _activeSize
    def apply(i: Int): ((Int, Int), T) = ((rowData(i), colData(i)), wData(i))
  }
  private var _lastSortIndex = 0
  private var _sortType: COOSortType = COOSortType.SortByRow

  def rowData = _rowData
  def colData = _colData
  def wData = _wData
  def activeSize: Int = _activeSize
  def clear(): Unit = {
    _activeSize = 0
    _lastSortIndex = 0
  }
  def sortType: COOSortType = _sortType
  def sorted = _activeSize == _lastSortIndex
  def apply(i: Int, j: Int): T = {
    if (sorted && _sortType == COOSortType.SortByCol) {
      var res = default
      val len = activeSize
      val rData = rowData
      val cData = colData
      var k = _rowIndex(i)
      var found = false
      while (k < len && rData(k) == i && !found) {
        if (j == cData(k)) {
          res = wData(k)
          found = true
        }
        k += 1
      }
      res
    } else {
      var res = default
      val rData = rowData
      val cData = colData
      val len = activeSize
      var k = 0
      var found = false
      while (k < len && !found) {
        if (i == rData(k) && j == cData(k)) {
          res = wData(k)
          found = true
        }
        k += 1
      }
      res
    }
  }
  def update(i: Int, j: Int, w: T): Unit = {
    require(i < rows, s"$i < rows= $rows failed")
    require(j < cols, s"$j < cols= $cols failed")

    if (_activeSize == _rowData.length) {
      _rowData = _rowData.grow()
      _colData = _colData.grow()
      _wData = _wData.grow()
    }
    _rowData(_activeSize) = i
    _colData(_activeSize) = j
    _wData(_activeSize) = w
    _activeSize += 1
  }
  def sort(sortType: COOSortType = COOSortType.SortByRow): Unit = {
    if (!sorted || _sortType != sortType) {
      logger.debug("sorting by " + sortType)
      _sortType = sortType
      // NOTE : very expensive.  will create a complete copy b4 sorting
      val size = activeSize
      val srcs = _rowData
      val tgts = _colData
      val w = _wData
      // don't use tabulate here
      val copy = Array.ofDim[COOEntry[T]](size)
      var i = 0
      while (i < size) {
        copy(i) = new COOEntry[T](srcs(i), tgts(i), w(i))
        i += 1
      }
      logger.trace(s"b4 sort : ${copy.iterator.take(10).mkString(", ")}")
      sortType match {
        case COOSortType.SortByRow =>
          java.util.Arrays.parallelSort(copy, (a: COOEntry[T], b: COOEntry[T]) => {
            if (a.src > b.src)
              1
            else if (a.src < b.src)
              -1
            else {
              if (a.tgt > b.tgt)
                1
              else if (a.tgt < b.tgt)
                -1
              else
                0
            }
          })
        case COOSortType.SortByCol =>
          java.util.Arrays.parallelSort(copy, (a: COOEntry[T], b: COOEntry[T]) => {
            if (a.tgt > b.tgt)
              1
            else if (a.tgt < b.tgt)
              -1
            else {
              if (a.src > b.src)
                1
              else if (a.src < b.src)
                -1
              else
                0
            }
          })
      }
      logger.trace(s"sorted : ${copy.iterator.take(10).mkString(", ")}")

      util.Arrays.fill(_rowIndex, -1)
      var lastSrc = -1

      i = 0
      while (i < size) {
        val wt = copy(i)
        val src = wt.src
        if (sortType == COOSortType.SortByRow && src != lastSrc) {
          _rowIndex(src) = i // start index
          lastSrc = src
        }
        srcs(i) = src
        tgts(i) = wt.tgt
        w(i) = wt.w
        i += 1
      }
      _lastSortIndex = activeSize
    } else
      logger.debug("no sort needed")
  }

  /** NOTE: impl could be more efficient... */
  def copy: Matrix[T] = {
    val copy = new CooMatrix[T](rows = rows, cols = cols)
    for (((r, c), w) <- activeIterator)
      copy(r, c) = w
    copy
  }
  def flatten(view: View): linalg.Vector[T] = ??? // todo
  def activeIterator: Iterator[((Int, Int), T)] = _activeIndexed.iterator
  def activeValuesIterator: Iterator[T] = _activeIndexed.iterator.map(_._2)
  def activeKeysIterator: Iterator[(Int, Int)] = _activeIndexed.iterator.map(_._1)
  override def toString: String = s"COOMatrix[rows= $rows, cols= $cols, active= $activeSize]"
}

private class COOEntry[@specialized(Float, Double) T](val src: Int, val tgt: Int, val w: T) {
  override def toString: String = s"($src,$tgt,$w)"
}
