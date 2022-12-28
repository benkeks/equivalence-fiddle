package io.equiv.eqfiddle.spectroscopy

import io.equiv.eqfiddle.util.Relation
import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.util.FixedPoint
import io.equiv.eqfiddle.game.WinningRegionComputation
import io.equiv.eqfiddle.game.AttackGraphBuilder
import io.equiv.eqfiddle.game.SimpleGame.GameNode
import io.equiv.eqfiddle.algo.AlgorithmLogging
import io.equiv.eqfiddle.util.LabeledRelation
import io.equiv.eqfiddle.game.GameGraphVisualizer
import io.equiv.eqfiddle.hml.HennessyMilnerLogic
import io.equiv.eqfiddle.hml.HennessyMilnerLogic.Formula
import io.equiv.eqfiddle.hml.ObservationClassWeak

class WeakPositionalSpectroscopy[S, A, L] (
    ts: WeakTransitionSystem[S, A, L],
    nodes: List[S])
  extends AbstractSpectroscopy[S, A, L, Formula[A]](ts) {

  override val spectrum = ObservationClassWeak.LTBTS

  override def buildStrategyFormulas(game: AbstractSpectroscopyGame[S, A, L])(node: GameNode, possibleMoves: Iterable[Set[Formula[A]]]): Set[Formula[A]] = {
    node match {
      case game.DefenderConjunction(_, _) =>
        val productMoves =
          possibleMoves.foldLeft(Seq(Seq[HennessyMilnerLogic.Formula[A]]()))(
            (b, a) => b.flatMap(i => a.map(j => i ++ Seq(j))))
        val moveSet = productMoves.map { mv =>
          val moves = mv.toSet
          HennessyMilnerLogic.And(moves).asInstanceOf[Formula[A]]
        }.toSet
        pruneDominated(moveSet)
      case game.AttackerObservation(_, _, game.ConjunctMove) =>
        possibleMoves.flatten.toSet
      case game.AttackerObservation(_, _, game.NegationMove) =>
        pruneDominated(possibleMoves.flatten.toSet[HennessyMilnerLogic.Formula[A]].map(f => HennessyMilnerLogic.Negate[A](f)))
      case game.AttackerObservation(_, _, game.ObservationMove(a)) =>
        pruneDominated(possibleMoves.flatten.toSet[HennessyMilnerLogic.Formula[A]].map(f => HennessyMilnerLogic.Observe[A](a, f)))
      case game.AttackerObservation(_, _, game.PassingMove) =>
        pruneDominated(possibleMoves.flatten.toSet[HennessyMilnerLogic.Formula[A]].map(f => HennessyMilnerLogic.Pass[A](f)))
    }
  }

  override def pruneDominated(oldFormulas: Set[Formula[A]]) = {
    val formulaClasses = for {
      f <- oldFormulas
    } yield spectrum.classifier(f)
    for {
      f <- oldFormulas
      cl = spectrum.classifier(f) //f.obsClass//getRootClass() // TODO: absolute or not?!
      if !formulaClasses.exists(clOther => cl > clOther)
    } yield {
      f
    }
  }

  def buildHML(game: AbstractSpectroscopyGame[S, A, L], win: Set[GameNode], nodes: Set[GameNode]) = {

    val attackGraphBuilder = new AttackGraphBuilder[Set[Formula[A]]]()

    val attackGraph = attackGraphBuilder.buildAttackGraph(game, win, nodes)

    val accumulatedPrices: Map[GameNode, Set[Formula[A]]] = attackGraphBuilder.accumulateNodePrices(
      graph = attackGraph,
      pricePick = buildStrategyFormulas(game) _,
      supPrice = Set(),
      nodes = nodes 
    )

    val bisimilarNodes = for {
      gn <- game.discovered
      if gn.isInstanceOf[game.AttackerObservation] && !win(gn)
      if nodeIsRelevantForResults(game, gn)
    } yield (gn, Set[Formula[A]]())

    val minPrices =
      bisimilarNodes.toMap ++
      (if (discardLanguageDominatedResults)
        accumulatedPrices.mapValues(spectrum.getLeastDistinguishing(_))
      else
        accumulatedPrices)

    minPrices
  }

  def nodeIsRelevantForResults(game: AbstractSpectroscopyGame[S, A, L], gn: GameNode): Boolean = gn match {
    case game.AttackerObservation(p, qq, kind) => (kind == game.ConjunctMove && qq.size == 1)
    case _ => false
  }

  def gameEdgeToLabel(game: AbstractSpectroscopyGame[S, A, L], gn1: GameNode, gn2: GameNode): String = ""

  def compute(
      comparedPairs: Iterable[(S,S)]
    ) = {
  
    val init = for {
      (p, q) <- comparedPairs
      start <- List((p, Set(q)), (q, Set(p)))
    } yield start

    val hmlGame = new WeakSpectroscopyGame(ts, init)

    debugLog("HML spectroscopy game size: " + hmlGame.discovered.size)

    val attackerWin = hmlGame.computeWinningRegion()
    val aLR = hmlGame.AttackerObservation(nodes(0), Set(nodes(1)), hmlGame.ConjunctMove)
    val aRL = hmlGame.AttackerObservation(nodes(1), Set(nodes(0)), hmlGame.ConjunctMove)

    val minFormulas = buildHML(hmlGame, attackerWin, Set(aLR, aRL))

    if (attackerWin.contains(aLR)) {
      minFormulas(aLR).foreach { f =>
        debugLog("Distinguished under " + spectrum.classifyFormula(f) + " preorder by " + f.toString())
        checkDistinguishing(f, nodes(0), nodes(1))
      }
    }

    if (attackerWin.contains(aRL)) {
      minFormulas(aRL).foreach { f =>
        debugLog("Distinguished under " + spectrum.classifyFormula(f) + " preorder by " + f.toString())
        checkDistinguishing(f, nodes(1), nodes(0))
      }
    }
    debugLog(graphvizGameWithFormulas(hmlGame, attackerWin, minFormulas))

    collectSpectroscopyResult(hmlGame, minFormulas)
  }
}