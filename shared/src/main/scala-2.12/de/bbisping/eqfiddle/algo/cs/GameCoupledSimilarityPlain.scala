package de.bbisping.eqfiddle.algo.cs

import de.bbisping.eqfiddle.util.Relation
import de.bbisping.eqfiddle.ts.WeakTransitionSystem
import de.bbisping.eqfiddle.util.FixedPoint
import scala.collection.mutable.Queue
import de.bbisping.eqfiddle.algo.AlgorithmLogging
import de.bbisping.eqfiddle.game.WinningRegionComputation
import de.bbisping.eqfiddle.game.GameDiscovery
import de.bbisping.eqfiddle.game.SimpleGame

/**
 * Implementation of coupled similarity derived from CS's characterization as a simple game.
 * [Plain version tries to stay close to the presentation in the thesis]
 */

class GameCoupledSimilarityPlain[S, A, L] (
    val ts: WeakTransitionSystem[S, A, L])
  extends AlgorithmLogging[S] {
  
  case class CSAttackerNode(p: S, q: S) extends SimpleGame.AttackerNode
  case class CSDefenderStepNode(a: A, p: S, q: S) extends SimpleGame.DefenderNode
  case class CSDefenderCouplingNode(p: S, q: S) extends SimpleGame.DefenderNode
  
  class CoupledSimulationBaseGame
    extends SimpleGame with GameDiscovery with WinningRegionComputation {
    
    override def initialNodes: Iterable[SimpleGame.GameNode] = for {
      s1 <- ts.nodes
      s2 <- ts.nodes
    } yield CSAttackerNode(s1, s2)
    
    def successors(gn: GameNode): Iterable[GameNode] = gn match {
      case CSAttackerNode(p0, q0) =>
        val dn = for {
          (a,pp1) <- ts.post(p0)
          p1 <- pp1
        } yield CSDefenderStepNode(a, p1, q0)
        dn ++ List(CSDefenderCouplingNode(p0, q0))
      case CSDefenderStepNode(a, p1, q0) =>
        for {
          q1 <- ts.weakPost(q0, a)
        } yield CSAttackerNode(p1, q1)
      case CSDefenderCouplingNode(p0, q0) =>
        for {
          q1 <- ts.silentReachable(q0)
        } yield CSAttackerNode(q1, p0)
    }
  }

  def computeGame() = {
    val csGame = new CoupledSimulationBaseGame() 

    println("cs plain game size: " + csGame.discovered.size)
    
    val attackerWin = csGame.computeWinningRegion()

    (csGame, attackerWin)
  }
  
  def compute() = {
    val (csGame, attackerWin) = computeGame()
    // the coupled simulation is exactly the attacker nodes not won by the attacker
    val simNodes = for {
      gn <- csGame.discovered
      if gn.isInstanceOf[CSAttackerNode] && !attackerWin(gn)
      CSAttackerNode(p, q) = gn
    } yield (p, q)
    
    new Relation[S](simNodes.toSet)
  }
}