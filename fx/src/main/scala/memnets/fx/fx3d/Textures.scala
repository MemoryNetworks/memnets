package memnets.fx.fx3d

import java.util.function.Consumer

import memnets.fx._
import memnets.model._
import scalafx.scene.paint._
import scalafx.scene.shape.Shape3D

trait Texture {

  /** if phong is lazy, this should be false */
  def isSame(material: JMaterial): Boolean = phong.delegate == material

  /**  allows lazy eval */
  def phong: PhongMaterial
}

/** using lookup methods */
object Textures extends Logging {
  type JPhongMaterial = javafx.scene.paint.PhongMaterial
  type PH = Consumer[PhongMaterial]

  val NOP: PH = ph => {}

  implicit def ph2txt(ph: PhongMaterial): Texture = new Texture {
    def phong = ph
  }

  private val registry = collection.mutable.AnyRefMap[String, Texture]()

  val GLASS = "glass"
  val OCEAN = "ocean"
  val OCEAN_GLASS = "ocean_glass"
  val DESKTOP = "desktop"
  val WOOD = "wood"
  val METAL = "metal"
  val CLOUD = "cloud"
  val SKY = "sky"
  val GOLD = "gold"
  val SILVER = "silver"

  def customize(origName: String, customName: String)(f: PH): Option[PhongMaterial] = {
    var phOpt: Option[PhongMaterial] = None
    for (ph <- registry.get(origName).map(_.phong)) {
      logger.trace(s"customizing : $origName to $customName")
      val custom = new PhongMaterial()
      ph.copyTo(custom)
      f.accept(custom)
      register(customName, custom)
      phOpt = Some(custom)
    }
    phOpt
  }
  def lookup(name: String): Option[PhongMaterial] = registry.get(name).map(_.phong)
  def apply(name: String, shape3d: Shape3D, share: Boolean = true): Boolean = {
    var found = false

    val phgOpt = registry.get(name).map(_.phong)
    if (phgOpt.isDefined) {
      found = true
      val ph = phgOpt.get
      shape3d match {
        case tgt: Phong if !share =>
          ph.copyTo(tgt.phong)
        case default =>
          if (!share)
            logger.warn("shape must implement Phong trait not to share")
          shape3d.setMaterial(ph)
      }
    }

    found
  }

  /** primarily for PickResult, which doesn't support scalafx Shape3D */
  def texturize(name: String, shape3d: javafx.scene.shape.Shape3D, share: Boolean = true): Option[JPhongMaterial] = {
    val phOpt = registry.get(name).map(_.phong.delegate)
    if (phOpt.isDefined)
      shape3d.setMaterial(phOpt.get)
    phOpt
  }
  def texureNames = registry.keys

  def reverseLookup(material: javafx.scene.paint.Material): Option[(String, Texture)] = {
    registry.find(_._2.isSame(material))
  }
  def register(name: String, data: Texture): Unit = {
    //  logger.info(s"registering texture : $name")
    registry(name) = data
  }

  // some defaults
  val DEFAULT = new PhongMaterial {
    diffuseColor = Color.Silver.interpolate(Color.Black, 0.4)
    specularColor = Color.Silver
    specularPower = 5.0
  }
  val oceanMaterial = new PhongMaterial {
    diffuseColor = Color.Aqua.interpolate(Color.Black, 0.15)
    specularColor = Color.Black
    specularPower = 100.0
  }
  val oceanGlassMaterial = new PhongMaterial {
    diffuseColor = Color.web("#60adcf").opacity(0.5)
    specularColor = Color.web("#60adcf").interpolate(Color.Black, 0.3)
    specularPower = 20.0
  }
  val silverMaterial = new PhongMaterial {
    diffuseColor = Color.LightSlateGray.sat(0.2)
    specularColor = Color.CornflowerBlue
    specularPower = 10.0
  }
  val metalMaterial = new PhongMaterial {
    diffuseColor = Color.web("#667")
    specularColor = Color.CornflowerBlue
    specularPower = 10.0
  }
  val goldMaterial = new PhongMaterial {
    diffuseColor = Color.Gold.sat(0.6)
    specularColor = Color.Gold
    specularPower = 20.0
  }
  val woodMaterial = new PhongMaterial {
    diffuseColor = Color.Burlywood.interpolate(Color.Black, 0.1)
    specularColor = Color.web("111")
    specularPower = 100.0
  }
  val desktopMaterial = new PhongMaterial {
    diffuseColor = Color.LightSlateGray
    specularColor = Color.web("111")
    specularPower = 100.0
  }
  val glassMaterial = new PhongMaterial {
    diffuseColor = Color.web("#444").opac(0.1)
    specularColor = Color.web("#666")
    specularPower = 20.0
  }
  val skyMaterial = new PhongMaterial {
    diffuseColor = Color.LightSkyBlue
    specularColor = Color.web("#222")
    specularPower = 100.0
  }

  register(OCEAN, oceanMaterial)
  register(OCEAN_GLASS, oceanGlassMaterial)
  register(SILVER, silverMaterial)
  register(GOLD, goldMaterial)
  register(WOOD, woodMaterial)
  register(DESKTOP, desktopMaterial)
  register(GLASS, glassMaterial)
  register(SKY, skyMaterial)
  register(METAL, metalMaterial)
}
