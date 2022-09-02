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
import de.bbisping.eqfiddle.hml.ObservationClassStrong
import de.bbisping.eqfiddle.hml.ObservationClassStrong.StronglyClassifiedFormula

class EdgeSpectroscopy[S, A, L] (
    ts: WeakTransitionSystem[S, A, L],
    nodes: List[S])
  extends AbstractSpectroscopy[S, A, L, StronglyClassifiedFormula[A]](ts, nodes) {

  override val spectrum = ObservationClassStrong.LTBTS

  def buildStrategyFormulas(game: AbstractSpectroscopyGame[S, A, L])(node: GameNode, possibleMoves: Iterable[Set[StronglyClassifiedFormula[A]]]):
      Set[StronglyClassifiedFormula[A]] = node match {
    case game.DefenderConjunction(_, _) if possibleMoves.size != 1 =>
      val productMoves =
        possibleMoves.foldLeft(Seq(Seq[HennessyMilnerLogic.Formula[A]]()))(
          (b, a) => b.flatMap(i => a.map(j => i ++ Seq(j))))
      productMoves.map { mv =>
        val moves = mv.toSet
        if (moves.size == 1) {
          StronglyClassifiedFormula(moves.head)
        } else {
          StronglyClassifiedFormula(HennessyMilnerLogic.And(moves))
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

  def moveToHML(game: SpectroscopyGameEdgeLabeled[S, A, L])(n1: GameNode, n2: GameNode, ff: Set[StronglyClassifiedFormula[A]]): Set[StronglyClassifiedFormula[A]] = {
    val kind = game.recordedMoveEdges(n1, n2)

    kind match {
      case game.ConjunctMove =>
        ff
      case game.NegationMove =>
        for {
          f <- ff
          if !f.isInstanceOf[HennessyMilnerLogic.Negate[_]]
        } yield StronglyClassifiedFormula(HennessyMilnerLogic.Negate(f))
      case game.ObservationMove(a) =>
        ff.map(f => StronglyClassifiedFormula(HennessyMilnerLogic.Observe(a, f)))
      case game.DefenderMove =>
        ff
    }
  }

  override def pruneDominated(oldFormulas: Set[StronglyClassifiedFormula[A]]) = {
    val observationFormulaClasses = for {
      f <- oldFormulas
      if f.isInstanceOf[HennessyMilnerLogic.Observe[_]]
    } yield f.getRootClass()

    val negationFormulaClasses = for {
      f <- oldFormulas
      if f.isInstanceOf[HennessyMilnerLogic.Negate[_]]
    } yield f.getRootClass()

    val conjunctionFormulaClasses = for {
      f <- oldFormulas
      if f.isInstanceOf[HennessyMilnerLogic.And[_]]
    } yield f.getRootClass()

    // if no old formula's class dominates this formula's class...
    for {
      f <- oldFormulas
      cl = f.getRootClass()
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

    val attackGraphBuilder = new AttackGraphBuilder[Set[StronglyClassifiedFormula[A]]]()

    val attackGraph = attackGraphBuilder.buildAttackGraph(game, win, nodes)

    val accumulatedPrices: Map[GameNode, Set[StronglyClassifiedFormula[A]]] = attackGraphBuilder.accumulatePrices(
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
    } yield (gn, Set[StronglyClassifiedFormula[A]]())

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

  def compute() = {

    val hmlGame = new SpectroscopyGameEdgeLabeled(ts, List((nodes(0), Set(nodes(1))), (nodes(1), Set(nodes(0)))))

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