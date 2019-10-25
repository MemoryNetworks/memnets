package memnets.model

trait Loc {
  def x: Double
  def y: Double
  def z: Double // only used in 3D

  def copy() = Loc(x, y)
  def up(i: Double = 100.0) = Loc(x, y - i)
  def down(i: Double = 100.0) = Loc(x, y + i)
  def left(i: Double = 100.0) = Loc(x - i, y)
  def right(i: Double = 100.0) = Loc(x + i, y)
  def midpt(other: Loc): (Loc, Double, Double) = {
    val dx = other.x - x
    val dy = other.y - y
    (Loc(x + dx / 2, y + dy / 2), dx, dy)
  }

  // Java
  def getX: Double = x
  def getY: Double = y
  def getZ: Double = z
}
object Loc {
  // NOTE : using lazy to wait for w,h to be set
  lazy val SINGLETON_LOC = Loc()
  def apply(): Loc = LocImpl(Display.resolution.width / 2.0, Display.resolution.height / 2.0)
  def apply(x: Double, y: Double, z: Double = 0.0): Loc = LocImpl(x, y, z)
}
final case class LocImpl(x: Double = 0.0, y: Double = 0.0, z: Double = 0.0) extends Loc
