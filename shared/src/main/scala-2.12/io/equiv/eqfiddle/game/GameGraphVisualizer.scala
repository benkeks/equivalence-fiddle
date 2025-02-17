
package io.equiv.eqfiddle.game

abstract class GameGraphVisualizer(game: SimpleGame with AbstractGameDiscovery) {

  def positionToID(gn: SimpleGame.GamePosition): String
  def positionToString(gn: SimpleGame.GamePosition): String
  def moveToLabel(gn1: SimpleGame.GamePosition, gn2: SimpleGame.GamePosition): String

  def outputDot(attackerWinningRegion: Set[SimpleGame.GamePosition]): String = {

    val edgeOutput = for {
      pos <- game.discovered
      successor <- game.successors(pos)
      color = "black"
      label = "\"" + moveToLabel(pos, successor) + "\""
    } yield positionToID(pos) + "->" + positionToID(successor) + s"[color=$color, label=$label]"
    
    val nodeOutput = for {
      pos <- game.discovered
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
