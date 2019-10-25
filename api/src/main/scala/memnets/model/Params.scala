package memnets.model

import memnets.linalg.W

trait Params {
  def all: IndexedSeq[Param]
  def default: Param
  def create(name: String, system: Boolean = false)(implicit sys: DynamicSystem): Param
  def remove(t: Param): Unit

  /** if unique = true (default), then ALL tags created will attempt to find existing */
  def unique: Boolean
  def unique_=(unique: Boolean): Unit
  def sorted: Iterable[Param]
  def getWs(p: Param): Iterable[W]
  def getTie(edge: E): Option[Param]
  def setTie(edge: E, p: Option[Param]): Unit
  def getUnique: Boolean = unique
  def setUnique(b: Boolean): Unit = unique = b

  import collection.JavaConverters._
  def getAll: java.util.List[Param] = all.asJava
}
