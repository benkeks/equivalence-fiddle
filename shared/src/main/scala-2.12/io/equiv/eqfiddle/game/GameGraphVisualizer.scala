
package io.equiv.eqfiddle.game

abstract class GameGraphVisualizer(game: SimpleGame with AbstractGameDiscovery) {

  def nodeToID(gn: SimpleGame.GamePosition): String
  def nodeToString(gn: SimpleGame.GamePosition): String
  def edgeToLabel(gn1: SimpleGame.GamePosition, gn2: SimpleGame.GamePosition): String

  def outputDot(attackerWinningRegion: Set[SimpleGame.GamePosition]): String = {

    val edgeOutput = for {
      node <- game.discovered
      successor <- game.successors(node)
      color = "black"
      label = "\"" + edgeToLabel(node, successor) + "\""
    } yield nodeToID(node) + "->" + nodeToID(successor) + s"[color=$color, label=$label]"
    
    val nodeOutput = for {
      node <- game.discovered
      nodeID = nodeToID(node)
      nodeLabel = nodeToString(node)
      isAttacker = node.isInstanceOf[SimpleGame.AttackerPosition]
      shape = if (isAttacker) "square" else "circle, width=2, fixedsize=true"
      color = if (attackerWinningRegion(node)) "red" else "blue"
      style = "bold"
    } yield s"$nodeID [shape=$shape, color=$color, style=$style, label=" + "\"" + nodeLabel +"\"]"

    "digraph rel{" +
      nodeOutput.mkString("", ";", ";") +
      edgeOutput.mkString(";") +
      "}"
  }

}
