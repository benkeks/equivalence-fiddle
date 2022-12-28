package io.equiv.eqfiddle.algo.cs

import io.equiv.eqfiddle.util.Relation
import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.util.FixedPoint
import scala.collection.mutable.Queue
import io.equiv.eqfiddle.algo.AlgorithmLogging
import io.equiv.eqfiddle.game.WinningRegionComputation
import io.equiv.eqfiddle.game.GameDiscovery
import io.equiv.eqfiddle.game.SimpleGame
import io.equiv.eqfiddle.algo.sigref.BigStepEquivalence

/**
 * Implementation of coupled similarity derived from CS's characterization as a simple game.
 */

class GameCoupledSimilarity[S, A, L] (
    override val ts: WeakTransitionSystem[S, A, L])
  extends GameCoupledSimilarityPlain[S, A, L](ts) {
  
  class CoupledSimulationGameWithApproximation(
      override val initialNodes: Iterable[SimpleGame.GameNode],
      csOverApproximation: Set[CSAttackerNode],
      csUnderApproximation: (S, S) => Boolean)
    extends CoupledSimulationBaseGame
      with GameDiscovery with WinningRegionComputation {
    
    override def successors(gn: GameNode): Iterable[GameNode] = gn match {
      case CSAttackerNode(p0, q0) =>
        if (csUnderApproximation(p0, q0)) {
          List()
        } else {
          super.successors(gn)
        }
      case _: SimpleGame.DefenderNode =>
        super.successors(gn) collect {
          case an: CSAttackerNode if csOverApproximation contains an => an
          case dn: SimpleGame.DefenderNode => dn
        }
    }
  }
  
  def initialsByEnabledSets() = {
    for {
      s1 <- ts.nodes
      s2 <- ts.nodes
      if ts.weakEnabled(s1) subsetOf ts.weakEnabled(s2)
    } yield CSAttackerNode(s1, s2)
  }
  
  def initialsByBigStepEquivalence() = {
    val bsEquiv = new BigStepEquivalence(ts)
    bsEquiv.computePartition()
    
    
    println("preparing join...")
    val sigs = for {
      s <- ts.nodes
    } yield (s, bsEquiv.signature(s))
    
    println("joiiiin...")
    for {
      (s1, s1Sig) <- sigs
      (s2, s2Sig) <- sigs
      if s1Sig.size <= s2Sig.size && (s1Sig subsetOf s2Sig) 
    } yield CSAttackerNode(s1, s2)
  }
  
  override def computeGame() = {
    // parameter for GameDiscovery
    val initialCandidates = initialsByBigStepEquivalence()
    
    println("initial size: " + initialCandidates.size)
    
    val csGame = new CoupledSimulationGameWithApproximation(
       initialCandidates,
       initialCandidates,
       (p0, q0) => ts.silentReachable(q0)(p0)
    )

    println("cs game size: " + csGame.discovered.size)
    val attackerWin = csGame.computeWinningRegion()

    (csGame, attackerWin)
  }

  override def compute() = {
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