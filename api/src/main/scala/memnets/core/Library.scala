package memnets.core

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer

object Library {
  implicit def libtoOpt(lib: Library): Option[Library] = Option(lib)
}
class Library {
  private[memnets] val _builders = ArrayBuffer[BldType]()
  def builders: Iterable[BldType] = _builders

  // Java
  def getBuilders: java.util.Collection[BldType] = _builders.asJava

  /** helper using scala objects in Java */
  def toJava(): Library = this
}
