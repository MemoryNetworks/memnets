package memnets.ml

import java.util.Scanner

import memnets.utils._

import scala.collection.mutable.ArrayBuffer

trait Data extends IndexedSeq[Row] {
  def apply(i: Int): Row
  def desc: String
  def desc_=(d: String): Unit
  def features: Features

  /** @return (fold_index, train, test) */
  def folds(n: Int): IndexedSeq[(Int, Data, Data)]
  def length: Int

  /** focus in on supervised learning, so mostly assume output exists  */
  def output: Option[DiscFeature] = features.find(_.isOutput).map(_.asInstanceOf[DiscFeature])
  def print(lines: Int = 10): Unit
  def randomize(): Unit
  def sort(comp: (Row, Row) => Boolean): Unit

  /** @return (train, test) */
  def split(percent: Double = 0.9): (Data, Data)
  override def toString = s"Data[desc= $desc, fnum= ${features.length}, size= $length]"
  def trainingDim: Int = features.iterator.filterNot(_.isOutput).map(_.dim).sum
  def trainingFeatures: Features = features.filterNot(_.isOutput)
}

object Data extends Logging {
  private val regex = "([ \\n\\r\\t,])+"
  def apply(features: Features, array: Array[Row], desc: String = "Unknown dataset"): Data =
    new DataImpl(features, array, desc)
  def read(features: Features, file: String, desc: String = "Unknown dataset"): Data = {
    val rows = new ArrayBuffer[Row]()
    val scan = new Scanner(file.asStream)
    var i = 0
    while (scan.hasNextLine) {
      val line = scan.nextLine
      i += 1
      try {
        val tokens = line.split(regex)
        // skip blank lines and comments...
        if (tokens.length == features.length && !tokens.contains("//")) {
          val row = for ((f, t) <- features.zip(tokens)) yield f.parse(t)
          rows += row
        } else
          logger.warn(s"skipping line $i: $line")
      } catch {
        case th: Throwable =>
          logger.error(s"line $i bad format: $line", th)
      }
    }
    scan.close()
    new DataImpl(features, rows.toArray, desc)
  }
}

private class DataImpl(val features: Features, val array: Array[Row], override var desc: String = "Unknown dataset")
    extends Data
    with Logging {
  import memnets.utils._

  val length = array.length
  def apply(i: Int): Row = array(i)

  /** NOTE : returned seq uses state.  not safe for multi-thread. */
  def folds(n: Int) = new IndexedSeq[(Int, Data, Data)] {
    val folds = array.grouped(DataImpl.this.length / n).toIndexedSeq
    def length = folds.length
    val buffer = new ArrayBuffer[Row](initialSize = length - folds.head.length)
    def apply(i: Int): (Int, Data, Data) = {
      buffer.clear()
      val test = folds(i)
      for ((fold, j) <- folds.zipWithIndex) {
        if (j != i)
          buffer ++= fold
      }
      (i, new DataImpl(features, buffer.toArray, desc + "_train"), new DataImpl(features, test, desc + "_test" + i))
    }
  }
  def print(lines: Int = 10): Unit = {
    logger.debug(s"output : $output")
    logger.debug(s"data (first $lines lines): ")
    for ((row, i) <- take(lines).zipWithIndex)
      logger.debug(s"$i - ${row.mkString(",")}")
  }
  def randomize(): Unit = {
    array.shuffle()
    array.shuffle()
    array.shuffle()
  }
  def sort(comp: (Row, Row) => Boolean): Unit = { util.Sorting.stableSort(array, comp) }
  def split(percent: Double): (Data, Data) = {
    val (train, test) = array.splitAt((percent * length).toInt)
    (new DataImpl(features, train, desc + "_train"), new DataImpl(features, test, desc + "_test"))
  }
}
