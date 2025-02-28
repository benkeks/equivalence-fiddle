package io.equiv.eqfiddle.game

trait SimpleGame {
  
  type GamePosition = SimpleGame.GamePosition
  type AttackerPosition = SimpleGame.AttackerPosition
  type DefenderPosition = SimpleGame.DefenderPosition
  
  def successors(gn: GamePosition): Iterable[GamePosition]
  
  def predecessors(gn: GamePosition): Iterable[GamePosition]
}

object SimpleGame {
  
  abstract class GamePosition
  abstract class AttackerPosition extends GamePosition
  abstract class DefenderPosition extends GamePosition
  
}