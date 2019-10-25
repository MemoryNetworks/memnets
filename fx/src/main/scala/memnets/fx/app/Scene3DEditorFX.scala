package memnets.fx.app

import javafx.fxml.FXMLLoader
import javafx.scene.paint.Color
import memnets.fx._
import memnets.fx.fx3d._
import memnets.fx.utils._
import memnets.model._
import memnets.utils.{BeanSupport, _}
import org.controlsfx.control.PropertySheet
import org.controlsfx.property.BeanPropertyUtils
import org.controlsfx.property.editor.PropertyEditor
import scalafx.Includes._
import scalafx.beans.property._
import scalafx.event.subscriptions.Subscription
import scalafx.scene.layout._
import scalafx.scene.shape.Shape3D

import scala.collection.mutable

class Scene3DEditorFX extends Logging {
  private val loader = new FXMLLoader("scene-editor.fxml".asURL)
  private var sub: Option[Subscription] = None
  private var subTexture: Option[Subscription] = None

  val texturePick = BooleanProperty(false)
  val fx = new AnchorPane(loader.load())
  private val lightEditor = new PropertyEditorFX(
    categoryOn = false,
    new AnchorPane(fx.findById("lightPane"))
  )

  private val validTextures = new mutable.ArrayBuffer[String]()
  validTextures += "none"
  validTextures ++= Textures.texureNames.toList.sorted
  private val phongEditor = new PropertyEditorFX(
    categoryOn = false,
    new AnchorPane(fx.findById("phongPane")),
    propFactory = new DefaultDirtyPropertyEditorFactory {
      override def call(item: PropertySheet.Item): PropertyEditor[_] = {
        if (item.getName == "texture") {
          import org.controlsfx.property.editor._

          import collection.JavaConverters._
          Editors.createChoiceEditor(item, validTextures.asJava)
        } else
          super.call(item)
      }
    }
  )
  def setScene(scene: Option[Scene3DFX]): Unit = {
    lightEditor.clearItems()
    phongEditor.clearItems()
    for (sub <- sub) sub.cancel()
    for (sub <- subTexture) sub.cancel()
    for (sc3d <- scene) {
      lightEditor.setItems(BeanPropertyUtils.getProperties(new LightAdapter(sc3d, texturePick)))
      sub = sc3d.subScene3d.picked ==> { picked =>
        if (texturePick.value) {
          val texture = Textures.reverseLookup(picked.getMaterial).map(_._1).getOrElseP("none")
          createAdapter(picked, texture)
        }
      }
    }
  }
  protected def createAdapter(picked: Shape3D, texture: String): Unit = {
    logger.trace("picked")
    val jphg = picked.getMaterial.as[JPhong].getOrElse(new JPhong())
    val adapter = new PhongAdapter(jphg, textureUsed = texture)
    phongEditor.setItems(BeanPropertyUtils.getProperties(adapter))

    for (subT <- subTexture) subT.cancel()
    subTexture = adapter.textureProperty ==> { value =>
      if (adapter.textureUsed != value) {
        val phOpt = Textures.lookup(value)
        if (phOpt.isDefined) {
          logger.debug("applying texture: " + value)
          Textures.texturize(value, picked, share = false)
          createAdapter(picked, value)
        } else if (value == "none") {
          logger.debug("clearing texture")
          picked.setMaterial(null)
          createAdapter(picked, "none")
        } else if (value == "unknown") {}
      }
    }
  }
}

class PhongAdapter(
    phong: JPhong,
    val textureUsed: String = "unknown"
) extends BeanSupport
    with Logging {

  def getDiffuseCol = phong.getDiffuseColor
  def setDiffuseCol(value: JColor) = phong.setDiffuseColor(value)
  def getSpecularCol = phong.getSpecularColor
  def setSpecularCol(value: JColor) = phong.setSpecularColor(value)

  def getSpecPower = phong.getSpecularPower
  def setSpecPower(value: Double) = phong.setSpecularPower(value)

  // todo: custom drop-down with valid textures would be nice
  val textureProperty = StringProperty(textureUsed)
  def getTexture = textureProperty.get()
  def setTexture(value: String): Unit = {
    textureProperty.set(value)
  }
}

class LightAdapter(scene: Scene3DFX, pickTextureOn: BooleanProperty) {
  def isTexturePick = pickTextureOn.value
  def setTexturePick(value: Boolean) = pickTextureOn.value = value

  def isAmbientOn = scene.ambientLight.isLightOn
  def setAmbientOn(value: Boolean): Unit = scene.ambientLight.setLightOn(value)
  def getAmbientCol: Color = scene.ambientLight.getColor
  def setAmbientCol(col: Color) = scene.ambientLight.setColor(col)

  def isSunOn = scene.sun.isLightOn
  def setSunOn(value: Boolean) = scene.sun.setLightOn(value)
  def getSunCol: Color = scene.sun.getColor
  def setSunCol(col: Color) = scene.sun.setColor(col)

  def getSunX = scene.sun.translateX.value
  def getSunY = scene.sun.translateY.value
  def getSunZ = scene.sun.translateZ.value

  def setSunX(value: Double) = scene.sun.translateX.value = value
  def setSunY(value: Double) = scene.sun.translateY.value = value
  def setSunZ(value: Double) = scene.sun.translateZ.value = value
}

/*
  // extra editors section
  // todo : add another tab?, move into base app val cheatsFX = new QuestionFX()
  // hack...
  worldEditor.yCol.value ==> { col =>
    logger.trace("y color change")
    if (!worldEditor.yCol.disable.value) {
      val y = sysFX.selectedY.value
      for (fx <- sysFX.find(y)) {
        logger.trace("valid fx found, updating")
        y.ui.color = new Color(col)
        fx.init()
      }
    }
  }
  override protected def onSelection(y : Y): Unit = {
    worldEditor.setY(y)
  }
 */
