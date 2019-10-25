package memnets.linalg

abstract class W {
  private[memnets] def id: Int
  private[memnets] def id_=(i: Int): Unit
  def src: Int
  def tgt: Int
  def w: Double
  def w_=(f: Double): this.type

  // Java
  final def isLoop: Boolean = src == tgt
  final def getSrc: String = src.toString
  final def getTgt: String = tgt.toString
  final def getW: Double = w
  final def setW(x: Double): Unit = w = x
}
