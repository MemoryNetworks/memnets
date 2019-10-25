package memnets.linalg

object DConverter {
  implicit object DConverterD extends DConverter[Double] {
    def numberType = NumberType.Doubles
    @inline def convertTo(d: Double): Double = d
    @inline def convertFrom(d: Double) = d
  }
  implicit object DConverterF extends DConverter[Float] {
    def numberType = NumberType.Floats
    @inline def convertTo(f: Float): Double = f
    @inline def convertFrom(d: Double): Float = d.asInstanceOf[Float]
  }
}
trait DConverter[T] {
  def numberType: NumberType
  def convertTo(t: T): Double
  def convertFrom(d: Double): T
}
