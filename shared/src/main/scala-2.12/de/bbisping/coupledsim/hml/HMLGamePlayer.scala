package de.bbisping.coupledsim.algo.cs

import de.bbisping.coupledsim.util.Relation
import de.bbisping.coupledsim.ts.WeakTransitionSystem
import de.bbisping.coupledsim.util.FixedPoint
import scala.collection.mutable.Queue
import de.bbisping.coupledsim.game.WinningRegionComputation
import de.bbisping.coupledsim.game.GameDiscovery
import de.bbisping.coupledsim.game.SimpleGame
import de.bbisping.coupledsim.hml.HennessyMilnerLogic
import de.bbisping.coupledsim.game.AttackTreeBuilder


class HMLGamePlayer[S, A, L] (
    val ts: WeakTransitionSystem[S, A, L],
    val nodes: List[S]) {
  
  case class AttackerObservation(p: S, qq: Set[S]) extends SimpleGame.AttackerNode
  case class DefenderConjunction(p: S, qq: Set[S]) extends SimpleGame.DefenderNode

  class HMLGame
    extends SimpleGame with GameDiscovery with WinningRegionComputation {

    override def initialNodes: Iterable[SimpleGame.GameNode] = Set(
      AttackerObservation(nodes(0), Set(nodes(1))),
      AttackerObservation(nodes(1), Set(nodes(0)))
    )

    def successors(gn: GameNode): Iterable[GameNode] = gn match {
      case AttackerObservation(p0, qq0) =>
        val dn = for {
          (a,pp1) <- ts.post(p0)
          p1 <- pp1
        } yield AttackerObservation(p1, qq0.flatMap(ts.post(_, a)))
        dn ++ List(DefenderConjunction(p0, qq0))
      case DefenderConjunction(p0, qq0) =>
        for {
          q0 <- qq0
        } yield AttackerObservation(p0, Set(q0))
    }
  }

  def buildHML(game: HMLGame, win: Set[SimpleGame.GameNode], node: SimpleGame.GameNode) = {

    def distances(n1: SimpleGame.GameNode, n2: SimpleGame.GameNode) = 1
    def pickMin(prices: Iterable[Int]) = prices.reduceLeft(Math.min _)

    val attackTreeBuilder = new AttackTreeBuilder(distances _)

    val attackTree = attackTreeBuilder.buildAttackTree(game, win, node)

    println(attackTree)

    val defenderDefeats = attackTree.rhs.collect {case n if !attackTree.rep.isDefinedAt(n) || attackTree.rep(n).isEmpty => n}

    println(defenderDefeats)

    val accumulatedPrices = attackTreeBuilder.accumulatePrices(attackTree, _ + _, pickMin _, 0, 9999, node, defenderDefeats.toSet)

    println(accumulatedPrices)

    val bestAttacks = attackTree.determinize { succs =>
      succs.reduceLeft[(Int, SimpleGame.GameNode)] { case (p1n1@(p1, n1), p2n2@(p2, n2)) =>
        if (accumulatedPrices(n1) <= accumulatedPrices(n2)) {
          p1n1
        } else {
          p2n2
        }
      }
    }

    println(bestAttacks)
    
    bestAttacks
  }

  def compute() = {

    val hmlGame = new HMLGame()

    println("hml game size: " + hmlGame.discovered.size)

    val attackerWin = hmlGame.computeWinningRegion()

    val a1 = AttackerObservation(nodes(0), Set(nodes(1)))

    println("lr:" + attackerWin.contains(a1))
    println("rl:" + attackerWin.contains(AttackerObservation(nodes(1), Set(nodes(0)))))
    
    println(buildHML(hmlGame, attackerWin, a1).peekPath(a1))

    true
  }
}