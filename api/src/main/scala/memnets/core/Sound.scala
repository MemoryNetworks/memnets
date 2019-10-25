package memnets.core

import com.typesafe.scalalogging.StrictLogging

import scala.collection.mutable

/**
 * NOTE : Sounds are only supported on FX platform
 */
object Sound extends StrictLogging {
  type S = String => Sound
  val LOST = "LOST"
  val SCORE_NEG = "SCORE_NEG"
  val SCORE_POS = "SCORE_POS"
  val WON = "WON"
  private val sounds = mutable.AnyRefMap[String, Sound]()
  private val sourceMap = mutable.AnyRefMap[String, S]()
  def apply(name: String, speed: Double = 1.0): Sound = {
    try {
      sounds.getOrElseUpdate(name, {
        sourceMap
          .get(name)
          .map(x => {
            val s = x(name)
            s.speed = speed
            s
          })
          .getOrElse(new NullSound)
      })
    } catch {
      case th: Throwable =>
        logger.error("couldn't load sound: " + name, th)
        new NullSound
    }
  }
  def stopAll(): Unit = {
    for (s <- sounds) s._2.end
  }
  def update(name: String, loader: S): Unit = sourceMap.put(name, loader)

}
trait MediaSound {
  def isSoundTrack: Boolean
}

trait Sound {
  def end(): Unit
  def playing: Boolean
  def speed: Double
  def speed_=(value: Double): Unit
  def start(volume: Double = 1.0): Unit
}
class NullSound extends Sound {
  var speed = 1.0
  private var _playing = false
  def end(): Unit = _playing = false
  def playing = _playing
  def start(vol: Double = 1.0): Unit = { _playing = true }
}
