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

class EdgeSpectroscopy[S, A, L] (
    ts: WeakTransitionSystem[S, A, L],
    nodes: List[S])
  extends AbstractSpectroscopy[S, A, L](ts, nodes) {

  def buildStrategyFormulas(game: AbstractSpectroscopyGame[S, A, L])(node: GameNode, possibleMoves: Iterable[Set[HennessyMilnerLogic.Formula[A]]]):
      Set[HennessyMilnerLogic.Formula[A]] = node match {
    case game.DefenderConjunction(_, _) if possibleMoves.size != 1 =>
      val productMoves =
        possibleMoves.foldLeft(Seq(Seq[HennessyMilnerLogic.Formula[A]]()))(
          (b, a) => b.flatMap(i => a.map(j => i ++ Seq(j))))
      productMoves.map { mv =>
        val moves = mv.toSet
        if (moves.size == 1) {
          moves.head
        } else {
          HennessyMilnerLogic.And(moves).asInstanceOf[HennessyMilnerLogic.Formula[A]]
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

  def moveToHML(game: SpectroscopyGameEdgeLabeled[S, A, L])(n1: GameNode, n2: GameNode, ff: Set[HennessyMilnerLogic.Formula[A]]): Set[HennessyMilnerLogic.Formula[A]] = {
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
        ff.map(HennessyMilnerLogic.Observe(a, _))
      case game.DefenderMove =>
        ff
    }
  }

  override def pruneDominated(oldFormulas: Set[HennessyMilnerLogic.Formula[A]]) = {
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
          !observationFormulaClasses.exists(clOther => cl.strictlyAbove(clOther)) ||
        f.isInstanceOf[HennessyMilnerLogic.Negate[_]] &&
          !negationFormulaClasses.exists(clOther => cl.strictlyAbove(clOther)) ||
        f.isInstanceOf[HennessyMilnerLogic.And[_]] &&
          !observationFormulaClasses.exists(clOther => cl.strictlyAbove(clOther)) &&
          !negationFormulaClasses.exists(clOther => cl.strictlyAbove(clOther)) &&
          !conjunctionFormulaClasses.exists(clOther => cl.strictlyAbove(clOther))
    } yield {
      f
    }
  }

  def buildHML(game: SpectroscopyGameEdgeLabeled[S, A, L], win: Set[GameNode], nodes: Set[GameNode]) = {

    val attackGraphBuilder = new AttackGraphBuilder[Set[HennessyMilnerLogic.Formula[A]]]()

    val attackGraph = attackGraphBuilder.buildAttackGraph(game, win, nodes)

    val accumulatedPrices: Map[GameNode, Set[HennessyMilnerLogic.Formula[A]]] = attackGraphBuilder.accumulatePrices(
      graph = attackGraph,
      priceCons = moveToHML(game) _,
      pricePick = buildStrategyFormulas(game) _,
      supPrice = Set(),
      nodes = nodes
    )

    val bisimilarNodes = for {
      gn <- game.discovered
      if gn.isInstanceOf[game.AttackerObservation] && !win(gn)
      game.AttackerObservation(p, qq, kind) = gn
      if qq.size == 1 && kind == game.ConjunctMove
    } yield (gn, Set[HennessyMilnerLogic.Formula[A]]())

    val minPrices =
      bisimilarNodes.toMap ++
      accumulatedPrices.mapValues(HennessyMilnerLogic.getLeastDistinguishing(_))

    if (AlgorithmLogging.loggingActive) {
      nodes.foreach { n => logAttacksAndResult(game, n, attackGraph, minPrices(n)) }
      nodes.foreach { n => logDefenseResult(game, n, minPrices)}
    }

    minPrices
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

    true
  }
}