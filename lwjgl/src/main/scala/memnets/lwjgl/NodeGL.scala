package memnets.lwjgl

/** quick and dirty.  much more work to be done here  */
trait NodeGL extends PhongGL {
  var visible = true
}
class SphereGL extends NodeGL {
  var radius = 1.0f
  val loc = Pt3Df()
}

sealed case class Pt3Df(var x: Float = 0.0f, var y: Float = 0.0f, var z: Float = 0.0f) {
  def clear(): Unit = {
    x = 0.0f
    y = 0.0f
    z = 0.0f
  }
}
