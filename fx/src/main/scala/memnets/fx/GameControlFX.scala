package memnets.fx

import memnets.core.GameControl
import scalafx.geometry.Pos
import scalafx.scene.Node
import scalafx.scene.input.KeyEvent

trait GameControlFX {
  def pos: Pos
  def fxNode: Node
  def data: GameControl = fxNode.userData.asInstanceOf[GameControl]
  def reset(): Unit = { data.reset() }
  def tick(): Unit
  def keyHandler(me: KeyEvent): Unit
}
