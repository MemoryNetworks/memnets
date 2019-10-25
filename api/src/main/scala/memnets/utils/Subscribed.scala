package memnets.utils

object Subscribed {
  implicit def s2opt(s: Subscribed): Option[Subscribed] = Option(s)
}
trait Subscribed {
  def cancel(): Unit
}
