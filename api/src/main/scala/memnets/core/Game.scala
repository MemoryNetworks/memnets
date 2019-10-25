package memnets.core

object Game {
  implicit def game2bld(game: Game): ModelBuilder = game.game
}
trait Game {
  def game: ModelBuilder
}
