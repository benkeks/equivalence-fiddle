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

class PositionalSpectroscopy[S, A, L] (
    ts: WeakTransitionSystem[S, A, L],
    nodes: List[S])
  extends AbstractSpectroscopy[S, A, L](ts, nodes) {

  def buildStrategyFormulas(game: AbstractSpectroscopyGame[S, A, L])(node: GameNode, possibleMoves: Iterable[Set[HennessyMilnerLogic.Formula[A]]]): Set[HennessyMilnerLogic.Formula[A]] = {
    node match {
      case game.DefenderConjunction(_, _) =>
        val productMoves =
          possibleMoves.foldLeft(Seq(Seq[HennessyMilnerLogic.Formula[A]]()))(
            (b, a) => b.flatMap(i => a.map(j => i ++ Seq(j))))
        val moveSet = productMoves.map { mv =>
          val moves = mv.toSet
          HennessyMilnerLogic.And(moves).asInstanceOf[HennessyMilnerLogic.Formula[A]]
        }.toSet
        pruneDominated(moveSet)
      case game.AttackerObservation(_, _, game.ConjunctMove) =>
        possibleMoves.flatten.toSet
      case game.AttackerObservation(_, _, game.NegationMove) =>
        pruneDominated(possibleMoves.flatten.map(HennessyMilnerLogic.Negate(_)).toSet)
      case game.AttackerObservation(_, _, game.ObservationMove(a)) =>
        pruneDominated(possibleMoves.flatten.toSet.map(HennessyMilnerLogic.Observe[A](a, _)))
    }
  }

  def nodeIsRelevantForResults(game: AbstractSpectroscopyGame[S, A, L], gn: GameNode): Boolean = gn match {
    case game.AttackerObservation(p, qq, kind) => (kind == game.ConjunctMove && qq.size == 1)
    case _ => false
  }

  override def pruneDominated(oldFormulas: Set[HennessyMilnerLogic.Formula[A]]) = {
    val formulaClasses = for {
      f <- oldFormulas
    } yield f.getRootClass()
    for {
      f <- oldFormulas
      cl = f.getRootClass()
      if !formulaClasses.exists(clOther => cl.strictlyAbove(clOther))
    } yield {
      f
    }
  }

  def buildHML(game: AbstractSpectroscopyGame[S, A, L], win: Set[GameNode], nodes: Set[GameNode]) = {

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

    if (AlgorithmLogging.loggingActive) {
      nodes.foreach { n => logAttacksAndResult(game, n, attackGraph, minPrices(n)) }
      nodes.foreach { n => logDefenseResult(game, n, minPrices)}
    }

    minPrices
  }

  def gameEdgeToLabel(game: AbstractSpectroscopyGame[S, A, L], gn1: GameNode, gn2: GameNode): String = ""

  def compute() = {

    val hmlGame = new SpectroscopyGame(ts, List((nodes(0), Set(nodes(1))), (nodes(1), Set(nodes(0)))))

    debugLog("HML spectroscopy game size: " + hmlGame.discovered.size)

    val attackerWin = hmlGame.computeWinningRegion()
    val aLR = hmlGame.AttackerObservation(nodes(0), Set(nodes(1)), hmlGame.ConjunctMove)
    val aRL = hmlGame.AttackerObservation(nodes(1), Set(nodes(0)), hmlGame.ConjunctMove)

    val minFormulas = buildHML(hmlGame, attackerWin, Set(aLR, aRL))

    if (attackerWin.contains(aLR)) {
      minFormulas(aLR).foreach { f =>
        debugLog("Distinguished under " + f.classifyFormula() + " preorder by " + f.toString())
        checkDistinguishing(f, nodes(0), nodes(1))
      }
    }

    if (attackerWin.contains(aRL)) {
      minFormulas(aRL).foreach { f =>
        debugLog("Distinguished under " + f.classifyFormula() + " preorder by " + f.toString())
        checkDistinguishing(f, nodes(1), nodes(0))
      }
    }
    debugLog(graphvizGameWithFormulas(hmlGame, attackerWin, minFormulas))

    collectSpectroscopyResult(hmlGame, minFormulas)
  }
}