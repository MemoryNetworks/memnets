package memnets.fx

import memnets.core._

trait GameControlSkinFX extends GameControlSkin[GameControlFX] {
  def control(ctrl: GameControl): Option[GameControlFX] = ctrl match {
    case pad: GamePadBase  => createGamePad(pad)
    case buts: GameButtons => createGameButtons(buts)
    case default           => None
  }
  def createGamePad(pad: GamePadBase): Option[GameControlFX] = new GamePadFX(pad)
  def createGameButtons(gbs: GameButtons): Option[GameControlFX] = new GameButtonsFX(gbs)
}
