package memnets.model

trait SimSignal extends TickListener with Dsl {
  def tgt: Yb
  def act: Double
  def on: Int
  def isActive: Boolean
}

trait SimTrial extends Dsl {
  type T <: SimSignal
  def ics: Iterator[T]
  def index: Int
  def init(): Unit
  def done(): Unit
  def inputs: scala.collection.IndexedSeq[T]
  def reset(): Unit
  def time: Int

  def getIndex: Int = index
}
