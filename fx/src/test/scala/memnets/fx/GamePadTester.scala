package memnets.fx

import memnets.core.{DPad, GameButtons}
import memnets.model.Logging
import scalafx.Includes._
import scalafx.animation.AnimationTimer
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.input._
import scalafx.scene.layout.StackPane
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle

object GamePadTester extends JFXApp with Logging {
  val gamePad = new DPad()
  val gamePadFX = new GamePadFX(gamePad)
  gamePadFX.reset

  stage = new JFXApp.PrimaryStage {
    scene = new Scene(400, 400) {
      content = new StackPane {
        children += Rectangle(400, 400, Color.Black)
        children += gamePadFX.ui
      }
    }
  }
//  gamePad.releaseCount.onChange { (_,_,b) =>
//    logger.debug("releaseCount "+b)
//  }
  var count = 0
  def tick(now: Long): Unit = {
    gamePadFX.tick
    count += 1

    // 10 sec
    if (count == 10 * 60) {
      logger.debug("reset")
      gamePad.reset()
    }
  }

  val keyEvents = gamePadFX.keyHandler _
  gamePadFX.ui.handleEvent(KeyEvent.Any) { keyEvents }
  gamePadFX.ui.focusTraversable = true
  gamePadFX.ui.requestFocus()
  val timer = AnimationTimer(tick)
  timer.start()
}

object GameButtonsTester extends JFXApp with Logging {
  val gameBtns = new GameButtons()
  // gameBtns(1).visible.value = false
  gameBtns(2).visible = false
  val gameFX = new GameButtonsFX(gameBtns)
  gameFX.reset()

  gameBtns.buttons(0).onPressed { sel =>
    logger.debug(s"but0 sel = $sel")
    gameBtns.buttons(1).disabled = gameBtns.buttons(0).pressed
  }

  stage = new JFXApp.PrimaryStage {
    scene = new Scene(400, 400) {
      content = new StackPane {
        children += Rectangle(400, 400, Color.Black.opacity(0.4))
        children += gameFX.ui

      }
      StackPane.setAlignment(gameFX.ui, gameFX.pos)
    }
  }
  def tick(now: Long): Unit = { gameFX.tick }

  val keyEvents = gameFX.keyHandler _
  gameFX.ui.handleEvent(KeyEvent.Any) { keyEvents }
  gameFX.ui.focusTraversable = true
  gameFX.ui.requestFocus()
  val timer = AnimationTimer(tick)
  timer.start()
}
