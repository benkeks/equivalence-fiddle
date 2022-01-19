
package de.bbisping.coupledsim.game

abstract class GameGraphVisualizer(game: SimpleGame with GameDiscovery) {

  def nodeToID(gn: SimpleGame.GameNode): String
  def nodeToString(gn: SimpleGame.GameNode): String
  def edgeToLabel(gn1: SimpleGame.GameNode, gn2: SimpleGame.GameNode): String

  def outputDot(attackerWinningRegion: Set[SimpleGame.GameNode]): String = {

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
      isAttacker = node.isInstanceOf[SimpleGame.AttackerNode]
      shape = if (isAttacker) "square" else "circle, width=2, fixedsize=true"
      color = if (isAttacker) {if (attackerWinningRegion(node)) "red" else "blue"} else "black"
      style = "bold"
    } yield s"$nodeID [shape=$shape, color=$color, style=$style, label=" + "\"" + nodeLabel +"\"]"

    "digraph rel{" +
      nodeOutput.mkString(";") +
      edgeOutput.mkString(";") +
      "}"
  }

}
