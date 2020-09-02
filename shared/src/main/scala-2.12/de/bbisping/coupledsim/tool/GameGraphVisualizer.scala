
package de.bbisping.coupledsim.tool

import de.bbisping.coupledsim.game.SimpleGame
import de.bbisping.coupledsim.algo.sigref.WeakBisimilarity
import de.bbisping.coupledsim.algo.transform.BuildQuotientSystem
import de.bbisping.coupledsim.algo.cs.GameCoupledSimilarityPlain
import de.bbisping.coupledsim.algo.cs.GameCoupledSimilarity

import scala.io.Source
import de.bbisping.coupledsim.ts.PrettyTSImporter

object GameGraphVisualizer {

  def outputDot(cfgPath: String, showOnlyOptimized: Boolean = true): Unit = {
    val src = Source.fromFile(cfgPath).mkString
    for (ts <- new PrettyTSImporter(src).result) {
      val weakbisim = new WeakBisimilarity(ts).computePartition
      val min = new BuildQuotientSystem(ts, weakbisim).build

      val gameContext = new GameCoupledSimilarityPlain(min)
      val (game, attackerWinningRegion) = gameContext.computeGame()
      val gameOptimizedContext = new GameCoupledSimilarity(min)
      val (smallerGame, _) = gameOptimizedContext.computeGame()

      def nodeToString(gn: SimpleGame.GameNode): String = {
        val name = gn match {
          case gameContext.CSAttackerNode(p0, q0) =>
            val p = p0.name
            val q = q0.name
            s"($p,$q)_a"
          case gameContext.CSDefenderStepNode(a0, p0, q0) =>
          val a = a0.name
          val p = p0.name
          val q = q0.name
          if (a == "tau") s"(τ,$p,$q)_d" else s"($a,$p,$q)_d"
          case gameContext.CSDefenderCouplingNode(p0, q0) =>
          val p = p0.name
          val q = q0.name
            s"(Cpl,$p,$q)_d"
          case _ => ""
        }
        '"' + name + '"'
      }

      val edgeOutput = for {
        node <- game.discovered
        successor <- game.successors(node)
        alsoInOptimizedGame = smallerGame.discovered.exists(_.hashCode == node.hashCode) && smallerGame.discovered.exists(_.hashCode == successor.hashCode)
        if !showOnlyOptimized || alsoInOptimizedGame
        color = if (alsoInOptimizedGame) "black" else "grey"
      } yield nodeToString(node) + "->" + nodeToString(successor) + s"[color=$color]"
      
      val nodeOutput = for {
        node <- game.discovered
        nodeName = nodeToString(node)
        isAttacker = nodeName.endsWith("a\"")
        alsoInOptimizedGame = smallerGame.discovered.exists(_.hashCode == node.hashCode)
        if !showOnlyOptimized || alsoInOptimizedGame
        shape = if (isAttacker) "square" else "circle, width=1, fixedsize=true"
        color = if (isAttacker) {if (attackerWinningRegion(node)) "red" else "blue"} else "black"
        style = if (smallerGame.discovered.exists(_.hashCode == node.hashCode)) "bold" else "dashed"
      } yield s"$nodeName [shape=$shape, color=$color, style=$style]"

      println("digraph rel{")
      println(nodeOutput.mkString(";\n"))
      println(edgeOutput.mkString(";\n"))
      println("}")
    }
  }

}
