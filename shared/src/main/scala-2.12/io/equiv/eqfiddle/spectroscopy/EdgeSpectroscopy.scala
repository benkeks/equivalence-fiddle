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
import io.equiv.eqfiddle.hml.ObservationClassStrong

class EdgeSpectroscopy[S, A, L] (
    ts: WeakTransitionSystem[S, A, L]
  )
  extends AbstractSpectroscopy[S, A, L, HennessyMilnerLogic.Formula[A]](ts) {

  override val spectrum = ObservationClassStrong.LTBTS

  var gameSize = (0,0)

  def buildStrategyFormulas(game: AbstractSpectroscopyGame[S, A, L])(node: GameNode, possibleMoves: Iterable[Set[Formula[A]]]):
      Set[Formula[A]] = node match {
    case game.DefenderConjunction(_, _) if possibleMoves.size != 1 =>
      val productMoves =
        possibleMoves.foldLeft(Seq(Seq[HennessyMilnerLogic.Formula[A]]()))(
          (b, a) => b.flatMap(i => a.map(j => i ++ Seq(j))))
      productMoves.map { mv =>
        val moves = mv.toSet
        if (moves.size == 1) {
          moves.head
        } else {
          HennessyMilnerLogic.And(moves)
        }
      }.toSet
    case _ =>
      val possibleFormulas = possibleMoves.flatten.toSet
      if (possibleFormulas.size > 1) {
        pruneDominated(possibleFormulas)
      } else {
        possibleFormulas
      }
  }

  def nodeIsRelevantForResults(game: AbstractSpectroscopyGame[S, A, L], gn: GameNode): Boolean = gn match {
    case game.AttackerObservation(p, qq, kind) => (kind == game.DefenderMove && qq.size == 1)
    case _ => false
  }

  def moveToHML(game: SpectroscopyGameEdgeLabeled[S, A, L])(n1: GameNode, n2: GameNode, ff: Set[Formula[A]]): Set[Formula[A]] = {
    val kind = game.recordedMoveEdges(n1, n2)

    kind match {
      case game.ConjunctMove =>
        ff
      case game.NegationMove =>
        for {
          f <- ff
          if !f.isInstanceOf[HennessyMilnerLogic.Negate[_]]
        } yield HennessyMilnerLogic.Negate(f)
      case game.ObservationMove(a) =>
        ff.map(f => HennessyMilnerLogic.Observe(a, f))
      case game.DefenderMove =>
        ff
      case _ =>
        ff
    }
  }

  override def pruneDominated(oldFormulas: Set[Formula[A]]) = {
    val observationFormulaClasses = for {
      f <- oldFormulas
      if f.isInstanceOf[HennessyMilnerLogic.Observe[_]]
    } yield spectrum.classifier(f)

    val negationFormulaClasses = for {
      f <- oldFormulas
      if f.isInstanceOf[HennessyMilnerLogic.Negate[_]]
    } yield spectrum.classifier(f)

    val conjunctionFormulaClasses = for {
      f <- oldFormulas
      if f.isInstanceOf[HennessyMilnerLogic.And[_]]
    } yield spectrum.classifier(f)

    // if no old formula's class dominates this formula's class...
    for {
      f <- oldFormulas
      cl = spectrum.classifier(f)
      if f.isInstanceOf[HennessyMilnerLogic.Observe[_]] &&
          !observationFormulaClasses.exists(clOther => cl > clOther) ||
        f.isInstanceOf[HennessyMilnerLogic.Negate[_]] &&
          !negationFormulaClasses.exists(clOther => cl > clOther) ||
        f.isInstanceOf[HennessyMilnerLogic.And[_]] &&
          !observationFormulaClasses.exists(clOther => cl > clOther) &&
          !negationFormulaClasses.exists(clOther => cl > clOther) &&
          !conjunctionFormulaClasses.exists(clOther => cl > clOther)
    } yield {
      f
    }
  }

  def buildHML(game: SpectroscopyGameEdgeLabeled[S, A, L], win: Set[GameNode], nodes: Set[GameNode]) = {

    val attackGraphBuilder = new AttackGraphBuilder[Set[Formula[A]]]()

    val attackGraph = attackGraphBuilder.buildAttackGraph(game, win, nodes)

    val accumulatedPrices: Map[GameNode, Set[Formula[A]]] = attackGraphBuilder.accumulatePrices(
      graph = attackGraph,
      priceCons = moveToHML(game) _,
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
        accumulatedPrices.mapValues(ff => spectrum.getLeastDistinguishing(ff))
      else
        accumulatedPrices)

    minPrices
  }

  def gameEdgeToLabel(game: AbstractSpectroscopyGame[S, A, L], gn1: GameNode, gn2: GameNode): String = {
    game.asInstanceOf[SpectroscopyGameEdgeLabeled[S, A, L]].recordedMoveEdges(gn1, gn2).toString()
  }

  override def compute(comparedPairs: Iterable[(S,S)]) = compute(comparedPairs, false)

  def compute(
      comparedPairs: Iterable[(S,S)],
      /* the implementation will ignore this configuration as it NEEDs to compute formulas to work */
      computeFormulas: Boolean = true,
      saveGameSize: Boolean = false,
    ) = {
  
    val init = for {
      (p, q) <- comparedPairs
      start <- List((p, Set(q)), (q, Set(p)))
    } yield start

    val hmlGame = new SpectroscopyGameEdgeLabeled(ts, init)

    debugLog("HML spectroscopy game size: " + hmlGame.discovered.size)

    if (saveGameSize) {
      gameSize = (hmlGame.discovered.size, hmlGame.successorNum.values.sum)
    }

    val attackerWin = hmlGame.computeWinningRegion()
    
    val attackerStarts = for { (p,q) <- comparedPairs; (l,r) <- List((p,q), (q,p)) } yield hmlGame.AttackerObservation(l, Set(r), hmlGame.ConjunctMove)

    val minFormulas = buildHML(hmlGame, attackerWin, attackerStarts.toSet)

    for (aLR @ hmlGame.AttackerObservation(l, rr, hmlGame.ConjunctMove) <- attackerStarts) {
      if (attackerWin.contains(aLR)) {
        minFormulas(aLR).foreach { f =>
          debugLog("Distinguished under " + spectrum.classifyFormula(f) + " preorder by " + f.toString())
          checkDistinguishing(f, l, rr.head)
        }
      }
    }

    debugLog(graphvizGameWithFormulas(hmlGame, attackerWin, minFormulas))

    collectSpectroscopyResult(hmlGame, minFormulas)
  }
}