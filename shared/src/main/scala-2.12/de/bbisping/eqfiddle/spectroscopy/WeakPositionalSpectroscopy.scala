package de.bbisping.eqfiddle.spectroscopy

import de.bbisping.eqfiddle.util.Relation
import de.bbisping.eqfiddle.ts.WeakTransitionSystem
import de.bbisping.eqfiddle.util.FixedPoint
import de.bbisping.eqfiddle.game.WinningRegionComputation
import de.bbisping.eqfiddle.game.AttackGraphBuilder
import de.bbisping.eqfiddle.game.SimpleGame.GameNode
import de.bbisping.eqfiddle.algo.AlgorithmLogging
import de.bbisping.eqfiddle.util.LabeledRelation
import de.bbisping.eqfiddle.game.GameGraphVisualizer
import de.bbisping.eqfiddle.hml.HennessyMilnerLogic

class WeakPositionalSpectroscopy[S, A, L] (
    ts: WeakTransitionSystem[S, A, L],
    nodes: List[S])
  extends PositionalSpectroscopy[S, A, L](ts, nodes) {

  override def buildStrategyFormulas(game: AbstractSpectroscopyGame[S, A, L])(node: GameNode, possibleMoves: Iterable[Set[HennessyMilnerLogic.Formula[A]]]): Set[HennessyMilnerLogic.Formula[A]] = {
    node match {
      case game.DefenderConjunction(_, _, weak) =>
        val productMoves =
          possibleMoves.foldLeft(Seq(Seq[HennessyMilnerLogic.Formula[A]]()))(
            (b, a) => b.flatMap(i => a.map(j => i ++ Seq(j))))
        val moveSet = productMoves.map { mv =>
          val moves = mv.toSet
          if (weak)
            HennessyMilnerLogic.WeakAnd(moves).asInstanceOf[HennessyMilnerLogic.Formula[A]]
          else
            HennessyMilnerLogic.And(moves).asInstanceOf[HennessyMilnerLogic.Formula[A]]
        }.toSet
        pruneDominated(moveSet)
      case game.AttackerObservation(_, _, game.ConjunctMove) | game.AttackerObservation(_, _, game.WeakConjunctMove) =>
        possibleMoves.flatten.toSet
      case game.AttackerObservation(_, _, game.NegationMove) =>
        pruneDominated(possibleMoves.flatten.toSet.map(HennessyMilnerLogic.Negate[A](_)))
      case game.AttackerObservation(_, _, game.ObservationMove(a)) =>
        pruneDominated(possibleMoves.flatten.toSet.map(HennessyMilnerLogic.Observe[A](a, _)))
      case game.AttackerObservation(_, _, game.WeakObservationMove(a)) =>
        pruneDominated(possibleMoves.flatten.toSet.map(HennessyMilnerLogic.WeakObserve[A](a, _)))
    }
  }

  // copied from superclass but referring to new buildStrategyFormulas
  override def buildHML(game: AbstractSpectroscopyGame[S, A, L], win: Set[GameNode], nodes: Set[GameNode]) = {

    val attackGraphBuilder = new AttackGraphBuilder[Set[HennessyMilnerLogic.Formula[A]]]()

    val attackGraph = attackGraphBuilder.buildAttackGraph(game, win, nodes)

    val accumulatedPrices: Map[GameNode, Set[HennessyMilnerLogic.Formula[A]]] = attackGraphBuilder.accumulateNodePrices(
      graph = attackGraph,
      pricePick = buildStrategyFormulas(game) _,
      supPrice = Set(),
      nodes = nodes 
    )

    val bisimilarNodes = for {
      gn <- game.discovered
      if gn.isInstanceOf[game.AttackerObservation] && !win(gn)
      if nodeIsRelevantForResults(game, gn)
    } yield (gn, Set[HennessyMilnerLogic.Formula[A]]())

    val minPrices =
      bisimilarNodes.toMap ++
      (if (discardLanguageDominatedResults)
        accumulatedPrices.mapValues(HennessyMilnerLogic.getLeastDistinguishing(_))
      else
        accumulatedPrices)

    minPrices
  }

  override def compute() = {

    val hmlGame = new WeakSpectroscopyGame(ts, List((nodes(0), Set(nodes(1))), (nodes(1), Set(nodes(0)))))

    debugLog("HML spectroscopy game size: " + hmlGame.discovered.size)

    val attackerWin = hmlGame.computeWinningRegion()
    val aLR = hmlGame.AttackerObservation(nodes(0), Set(nodes(1)), hmlGame.ConjunctMove)
    val aRL = hmlGame.AttackerObservation(nodes(1), Set(nodes(0)), hmlGame.ConjunctMove)

    val minFormulas = buildHML(hmlGame, attackerWin, Set(aLR, aRL))

    if (attackerWin.contains(aLR)) {
      minFormulas(aLR).foreach { f =>
        debugLog("Distinguished under " + f.classifyFormula() + " preorder by " + f.toString())
        //checkDistinguishing(f, nodes(0), nodes(1))
      }
    }

    if (attackerWin.contains(aRL)) {
      minFormulas(aRL).foreach { f =>
        debugLog("Distinguished under " + f.classifyFormula() + " preorder by " + f.toString())
        //checkDistinguishing(f, nodes(1), nodes(0))
      }
    }
    debugLog(graphvizGameWithFormulas(hmlGame, attackerWin, minFormulas))

    collectSpectroscopyResult(hmlGame, minFormulas)
  }
}