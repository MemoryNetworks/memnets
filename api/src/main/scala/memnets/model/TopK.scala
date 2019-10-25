package memnets.model

import scala.beans.BeanProperty
import scala.collection.JavaConverters._

sealed case class TopK(
    @BeanProperty layer: LayerLike,
    entries: IndexedSeq[TopKEntry],
    @BeanProperty t: Int) {

  // Java
  def getTopEntry: TopKEntry = entries(0)
  def getEntries: java.util.List[TopKEntry] = entries.asJava
  override def toString: String = s"TopK[name=${layer.name}, t=$t, entries=${entries.mkString(",")}]"
}

/** NOTE using secondary ctor so index is the only one in generated equals  */
sealed case class TopKEntry(@BeanProperty index: Int)(@BeanProperty val act: Double) extends Ordered[TopKEntry] {
  def this(index: Int, act: Double, y: Yb) = {
    this(index)(act)
    this.y = y
  }
  private var _y: Yb = _
  def y: Yb = _y
  def y_=(y: Yb): Unit = { _y = y }
  def compare(that: TopKEntry): Int = {
    if (act > that.act)
      1
    else if (act < that.act)
      -1
    else
      0
  }
  override def toString: String = f"TopE[$index, $act%.2f]"

  // Java
  def getY: Yb = y
}

trait Influence {
  def influence(i: Int, act: Double): Double
}

/** NOTE: don't use anonymous class in package object.  explicit class here so easier to match in rules */
class MatMulInfluence(val matMul: MatMul) extends Influence {
  def influence(i: Int, act: Double): Double = {
    import matMul._
    if (tgt.lastTopK.isDefined) {
      val topK = tgt.lastTopK.get
      val len = topK.entries.length
      var j = 0
      var sum = 0.0
      while (j < len) {
        val entry = topK.entries(j)
        sum += act * apply(entry.index, i)
        j += 1
      }
      sum
    } else
      act

  }
}

/** NOTE: don't use anonymous class in package object.  explicit class here so easier to match in rules */
class DotInfluence(val dot: Dot) extends Influence {
  def influence(i: Int, act: Double): Double = Math.abs(act * dot(i))
}
object NopInfluence extends Influence {
  @inline def influence(i: Int, act: Double): Double = act
}
