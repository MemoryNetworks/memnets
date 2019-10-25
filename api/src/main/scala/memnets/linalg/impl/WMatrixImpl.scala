package memnets.linalg.impl

import memnets.linalg._
import memnets.utils._

import scala.reflect.ClassTag

/** NOTE: due to specialized inheritance issue, not just subclassing COOMatrix... */
private[memnets] final class WMatrixImpl[@specialized(Float, Double) Z: ClassTag](initCapacity: Int = 2048)(
    implicit val converter: DConverter[Z])
    extends CooMatrixLike[Z]
    with WMatrix
    with Logging {
  val numberType = converter.numberType

  object weights extends IndexedSeq[T] {
    def length = _wCount
    def apply(i: Int): T = _weights(i)
  }
  private var _index = Array.ofDim[Int](32)
  private var _wCount = 0
  private[memnets] var _srcs = Array.ofDim[Int](initCapacity)
  private[memnets] var _tgts = Array.ofDim[Int](initCapacity)
  private[memnets] var _wgts = Array.ofDim[Z](initCapacity)
  private val ONE: Z = 1.0.asInstanceOf[Z]
  private var _weights = Array.ofDim[T](initCapacity)
  private var _lastSortedIndex = -1
  private object emptyIterator extends Iterator[T] {
    def hasNext: Boolean = false
    def next(): T = ???
  }
  private final class IndexIterable(src: Int, start: Int) extends Iterator[T] {
    var i = 0
    def hasNext: Boolean = {
      val off = start + i
      if (off < _wCount) _srcs(off) == src else false
    }
    def next(): T = {
      i += 1
      _weights(start + i - 1)
    }
  }
  def activeSize = _wCount
  def rowData: Array[Int] = _tgts
  def colData: Array[Int] = _srcs
  def wData: Array[Z] = _wgts
  def clear(): Unit = { _wCount = 0 }
  def create(src: Int, tgt: Int, unique: Boolean = false): T = {
    val existing: Option[T] = if (!unique) None else findW(src, tgt)
    if (existing.isEmpty) {
      val w = new WImpl(_wCount, this)
      _wCount += 1
      if (_wCount >= _weights.length) {
        _weights = _weights.grow()
        _srcs = _srcs.grow()
        _tgts = _tgts.grow()
        _wgts = _wgts.grow()
      }
      val id = w.id
      _weights(id) = w
      _srcs(id) = src
      _tgts(id) = tgt
      _wgts(id) = ONE
      w
    } else
      existing.get
  }
  def getSrc(edgeId: Int) = _srcs(edgeId)
  def getTgt(edgeId: Int) = _tgts(edgeId)
  @inline def getWeight(edgeId: Int): Double = converter.convertTo(_wgts(edgeId))
  @inline def setWeight(edgeId: Int, w: Double): Unit = _wgts(edgeId) = converter.convertFrom(w)

  override def outEdges(src: Int): Iterator[T] = {
    if (sorted) {
      val start = _index(src)
      if (start > -1)
        new IndexIterable(src, start)
      else
        emptyIterator
    } else
      super.outEdges(src)
  }
  // for now just zero out "dead" tied
  def remove(e: T) = e.w = 0.0
  def sorted = _wCount == _lastSortedIndex
  def sortType: COOSortType = COOSortType.SortByRow
  def sortWeights(varCount: Int): Unit = {
    if (!sorted) {
      val size = _wCount
      // NOTE : very expensive.  will create a complete copy b4 sorting
      val src = _srcs
      val tgt = _tgts
      val w = _weights
      val wTemp = Array.ofDim[WSrcSortable[T]](size) // tabulate allocs lots of Integer, so while
      var i = 0
      while (i < size) {
        wTemp(i) = new WSrcSortable(w(i))
        i += 1
      }
      logger.trace(s"b4 sort : ${wTemp.iterator.take(10).mkString(",")}")
      java.util.Arrays.parallelSort(wTemp)
      logger.trace(s"sorted : ${wTemp.iterator.take(10).mkString(",")}")

      _index = Array.fill[Int](varCount) { -1 }
      var lastSrc = -1
      i = 0
      while (i < size) {
        val wt = wTemp(i)
        val sid = wt.src
        if (sid != lastSrc) {
          _index(sid) = i // start index
          lastSrc = sid
        }
        src(i) = sid
        tgt(i) = wt.tgt
        w(i) = wt.edge
        setWeight(i, wt.w)
        wt.edge.id = i
        i += 1
      }
      _lastSortedIndex = size
      if (_index.length < 20)
        logger.trace("index = " + _index.mkString(","))

    } else
      logger.debug("no sort needed")
  }
}

private[impl] sealed class WSrcSortable[W <: memnets.linalg.W](val src: Int, val tgt: Int, val w: Double, val edge: W)
    extends Ordered[WSrcSortable[W]] {
  def this(e: W) { this(e.src, e.tgt, e.w, e) }
  def compare(that: WSrcSortable[W]) = {
    if (src > that.src)
      1
    else if (src < that.src)
      -1
    else {
      if (tgt > that.tgt)
        1
      else if (tgt < that.tgt)
        -1
      else
        0
    }
  }
  override def toString = edge.toString
}
