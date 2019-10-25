package memnets.core

import scala.collection.mutable.ArrayBuffer

object Profile {
  private val _profiles = ArrayBuffer[Profile]()
  def profiles: Iterable[Profile] = _profiles
}
case class Profile(name: String, link: String) {
  Profile._profiles += this
  override def toString: String = name
}
object MemNetsProfile extends Profile("MemNets", "www.memnets.com")
