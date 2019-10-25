package memnets.fx

import javafx.scene.input.KeyCode
import javafx.scene.input.KeyEvent._
import memnets.core.{AnglePad, DPad, DPadNoDown, GamePadBase}
import memnets.model._
import scalafx.Includes._
import scalafx.geometry.Pos
import scalafx.scene.Group
import scalafx.scene.input.KeyEvent

class GamePadAdapter(val gamePadBase: GamePadBase) {
  import memnets.fx.utils.BeanUtils._
  val disabled = bool(gamePadBase, "disabled")
  val active = bool(gamePadBase, "active")
  val visible = bool(gamePadBase, "visible")
}
class GamePadFX(gamePadBase: GamePadBase) extends GameControlFX with Logging {
  import Math._

  import KeyCode._
  val adapter = new GamePadAdapter(gamePadBase)
  val ui = new Group("gamepad.fxml".loadFXML)
  ui.userData = gamePadBase
  def pos = Pos.CenterLeft
  def fxNode = ui
  val gamePadGroup = new Group(ui.findById("gamePadGroup"))
  val gamePadBack = ui.findById[JStackPane]("gamePadBack")
  val gamePadArc: javafx.scene.shape.Arc = ui.findById("gamePadArc")
  val gamePadText = ui.findTextById("gamePadText")
  val gamePadThumb = ui.findById[JStackPane]("gamePad")
  val emptyTxt = ""
  val radius = 60.0
  trait PadModeImpl {
    def showAngle: Boolean
    def keyAdj: Double
    def origin: Loc
    def calc(): Unit
  }
  class NoDownModeImpl(gamePad: DPadNoDown) extends PadModeImpl {
    val showAngle = false
    val keyAdj = gamePad.keyAdj.getOrElse(6.0)
    val origin = Loc(0, radius)
    def calc() = {
      val x = gamePadThumb.translateXProperty.get / radius
      val y = -(-radius + gamePadThumb.translateYProperty.get) / (2.0 * radius)
      import gamePad._
//      right.value = if (x > 0.0) x else 0.0
//      left.value = if (x < 0.0) -x else 0.0
//      up.value = y

      right = if (x > 0.0) x else 0.0
      left = if (x < 0.0) -x else 0.0
      up = y
    }
  }
  class CenterModeImpl(gamePad: DPad) extends PadModeImpl {
    val showAngle = false
    val keyAdj = gamePad.keyAdj.getOrElse(6.0)
    val origin = Loc(0, 0)
    def calc() = {
      val x = gamePadThumb.translateXProperty.get / radius
      val y = -gamePadThumb.translateYProperty.get / radius
      import gamePad._
      right = if (x > 0.0) x else 0.0
      left = if (x < 0.0) -x else 0.0
      up = if (y > 0.0) y else 0.0
      down = if (y < 0.0) -y else 0.0
    }
  }
  class CornerModeImpl(gamePad: AnglePad) extends PadModeImpl {
    val showAngle = true
    val keyAdj = gamePad.keyAdj.getOrElse(1.0)
    val origin = Loc(radius, -radius)
    val originInertia = 4.0
    def calc() = {
      val x = origin.x - gamePadThumb.translateXProperty.get
      val y = origin.y - gamePadThumb.translateYProperty.get
      import gamePad._
      angle = floor(toDegrees(-atan2(y, x)))
      mag = sqrt(pow(x, 2.0) + pow(y, 2.0)) / (2.0 * radius)
      mag = min(mag, 1.0)

      gamePadArc.lengthProperty.set(angle)
      val arcRad = Math.min(1.0, mag) * 160.0
      gamePadArc.radiusXProperty.set(arcRad)
      gamePadArc.radiusYProperty.set(arcRad)

      gamePadText.textProperty.set(
        if (active)
          angleFormat.format(abs(angle), mag * magScale)
        else emptyTxt)
    }
  }
  // todo : make fast formatter...
  val angleFormat = "%.0f\u00b0|%.1f"
  gamePadThumb.visible <== !adapter.disabled
  val padMode = gamePadBase match {
    case cen: DPad =>
      new CenterModeImpl(cen)
    case noDown: DPadNoDown =>
      new NoDownModeImpl(noDown)
    case deg: AnglePad =>
      new CornerModeImpl(deg)
    case default =>
      ???
  }
  gamePadText.visibleProperty.set(padMode.showAngle)
  gamePadArc.visibleProperty.set(padMode.showAngle)
  adapter.visible <==> ui.visible
  gamePadBack.setOpacity(0.25)
  gamePadThumb.onMouseDragged = me => {
    if (!gamePadBase.disabled) {
      gamePadBase.active = true
      adjust(me.getX, me.getY)
      me.consume()
    }
  }
  gamePadThumb.onMouseReleased = me => {
    gamePadBase.active = false
    me.consume()
  }
  def adjust(dX: Double, dY: Double): Unit = {
    adjustX(dX)
    adjustY(dY)
  }
  def adjustX(dX: Double): Unit = {
    val x = gamePadThumb.translateXProperty.get + dX
    gamePadThumb.translateXProperty.set(if (x < -radius) -radius else if (x > radius) radius else x)
  }
  def adjustY(dY: Double): Unit = {
    val y = gamePadThumb.translateYProperty.get + dY
    gamePadThumb.translateYProperty.set(if (y < -radius) -radius else if (y > radius) radius else y)
  }
  override def reset: Unit = {
    super.reset
    gamePadThumb.translateXProperty.set(padMode.origin.x)
    gamePadThumb.translateYProperty.set(padMode.origin.y)
    updatePad
  }

  def updatePad: Unit = { padMode.calc() }
  def tick: Unit = {
    if (gamePadBase.active && !gamePadBase.disabled) {
      val keyAdj = padMode.keyAdj
      var i = 0
      val len = codes.length
      while (i < len) {
        val code = codes(i)
        if (code.active)
          code.processKey(keyAdj)
        i += 1
      }
      updatePad
    }
  }
  adapter.disabled ==> { b =>
    if (b)
      ui.opacity = 0.2
    else
      ui.opacity = 1.0
  }
  adapter.active ==> { b =>
    if (b) {
      gamePadBack.setOpacity(1.0)
      gamePadThumb.opacityProperty.set(1.0)
      for (a <- codes) a.active = false
    } else {
      //    logger.debug(s"angle = ${angle.value}, mag = ${mag.value}")
      gamePadBack.setOpacity(0.25)
      gamePadThumb.opacityProperty.set(0.5)
    }
    // snap back to origin when user starts/ends interaction
    gamePadThumb.translateXProperty.set(padMode.origin.x)
    gamePadThumb.translateYProperty.set(padMode.origin.y)
    updatePad
  }

  abstract class ActiveKey(val codes: KeyCode*) {
    var active: Boolean = false
    final def hasCode(code: KeyCode): Boolean = codes.contains(code)
    def processKey(keyAdj: Double): Unit
  }
  object LeftKey extends ActiveKey(LEFT, A) { def processKey(keyAdj: Double) = adjustX(-keyAdj) }
  object RightKey extends ActiveKey(RIGHT, D) { def processKey(keyAdj: Double) = adjustX(keyAdj) }
  object UpKey extends ActiveKey(UP, W) { def processKey(keyAdj: Double) = adjustY(-keyAdj) }
  object DownKey extends ActiveKey(DOWN, S) { def processKey(keyAdj: Double) = adjustY(keyAdj) }

  val codes = Array.apply(LeftKey, RightKey, UpKey, DownKey)
  def keyHandler(me: KeyEvent): Unit = {
    if (!gamePadBase.disabled) {
      val code = me.delegate.getCode
      if (code.isArrowKey || code.isLetterKey) {
        var i = 0
        var anyActive = false
        val len = codes.length
        while (i < len) {
          val ak = codes(i)
          if (ak.hasCode(code))
            ak.active = me.delegate.getEventType != KEY_RELEASED
          anyActive = anyActive || ak.active
          i += 1
        }
        // logger.debug("keyHandler: "+anyActive)
        if (gamePadBase.active != anyActive)
          gamePadBase.active = anyActive
      }
    }
  }
}
