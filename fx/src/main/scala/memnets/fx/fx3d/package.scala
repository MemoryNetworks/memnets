package memnets.fx

import com.sun.javafx.geom.Vec3d
import scalafx.scene._
import scalafx.scene.paint._
import scalafx.scene.shape._

package object fx3d {
  type JVec3d = com.sun.javafx.geom.Vec3d
  type JGroup = javafx.scene.Group
  type JNode = javafx.scene.Node
  implicit def t3dfx2Option[T <: Tickable3DFX](fx: T): Option[Tickable3DFX] = Some(fx)

  implicit class PhongExt(val src: PhongMaterial) extends AnyVal {
    def copyTo(tgt: PhongMaterial): Unit = {
      tgt.diffuseColor = src.diffuseColor.value
      tgt.diffuseMap = src.diffuseMap.value
      tgt.specularColor = src.specularColor.value
      tgt.specularPower = src.specularPower.value
      tgt.specularMap = src.specularMap.value
      tgt.bumpMap = src.bumpMap.value
      tgt.selfIlluminationMap = src.selfIlluminationMap.value
    }
  }
  // NOTE : need this on Node, not Shape3D
  implicit class NodeExt[T <: Node](val src: T) extends AnyVal {
    def intersects3D(other: Node): Boolean = {
      src.boundsInParent.value.intersects(other.boundsInParent.value)
    }
    def contains3D(other: Node): Boolean = {
      src.boundsInParent.value.contains(other.boundsInParent.value)
    }
    def scaleAll(scale: Double): Unit = {
      val del = src.delegate
      del.setScaleX(scale)
      del.setScaleY(scale)
      del.setScaleZ(scale)
    }
  }
  implicit class Vec3dExt(val vec: Vec3d) extends AnyVal {
    def clear(): Unit = {
      vec.x = 0.0
      vec.y = 0.0
      vec.z = 0.0
    }
  }

//  case class Pt3D(var x : Double, var y : Double, var z : Double) {
//    def clear { x = 0.0; y = 0.0; z = 0.0 }
//  }

  def tiledPlane(f: => Box): Group = {
    val tileGroup = new Group()
    tileGroup.autoSizeChildren = false
    val n = 3
    val off = Math.floor(n / 2.0)
    for {
      r <- 0 until n
      c <- 0 until n
    } {
      val tile = f
      val xdim = (-off + r) * (tile.width.value)
      val zdim = (-off + c) * (tile.width.value)
      tile.translateX = xdim
      tile.translateZ = zdim
      tileGroup.children.add(tile)
    }
    tileGroup
  }
}
