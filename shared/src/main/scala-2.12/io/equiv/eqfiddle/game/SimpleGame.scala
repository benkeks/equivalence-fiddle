package io.equiv.eqfiddle.game

trait SimpleGame[GamePosition <: SimpleGame.GamePosition] {
    
  def successors(gn: GamePosition): Iterable[GamePosition]
  
  def predecessors(gn: GamePosition): Iterable[GamePosition]
}

object SimpleGame {
  
  abstract class GamePosition
  abstract class AttackerPosition extends GamePosition
  abstract class DefenderPosition extends GamePosition
  
}