package memnets.model

import breeze.linalg.DenseVector

import scala.collection.mutable

trait DenseVectorLike extends mutable.IndexedSeq[Double] {
  def clear(): Unit
  def random(): this.type
  def toDenseVector(output: DenseVector[Double] = null): DenseVector[Double]
  def :=(other: DenseVector[Double]): this.type

  def getLength: Int = length
  def getSize: Int = length
}
