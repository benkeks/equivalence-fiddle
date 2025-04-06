
package io.equiv.eqfiddle.game

abstract class GameGraphVisualizer[GamePosition <: SimpleGame.GamePosition](val game: SimpleGame[GamePosition] with AbstractGameDiscovery[GamePosition]) {
  
  def positionToID(gn: GamePosition): String
  def positionToString(gn: GamePosition): String
  def moveToLabel(gn1: GamePosition, gn2: GamePosition): String

  def outputDot(attackerWinningRegion: Set[GamePosition]): String = {

    val edgeOutput = for {
      pos <- game.discovered.toSeq
      successor <- game.successors(pos)
      if game.discovered(successor)
      color = "black"
      label = "\"" + moveToLabel(pos, successor) + "\""
    } yield positionToID(pos) + "->" + positionToID(successor) + s"[color=$color, label=$label]"
    
    val nodeOutput = for {
      pos <- game.discovered.toSeq
      posID = positionToID(pos)
      posLabel = positionToString(pos)
      isAttacker = pos.isInstanceOf[SimpleGame.AttackerPosition]
      shape = if (isAttacker) "square" else "circle, width=2, fixedsize=true"
      color = if (attackerWinningRegion(pos)) "red" else "blue"
      style = "bold"
    } yield s"$posID [shape=$shape, color=$color, style=$style, label=" + "\"" + posLabel +"\"]"

    "digraph rel{" +
      nodeOutput.mkString("", ";", ";") +
      edgeOutput.mkString(";") +
      "}"
  }

}
