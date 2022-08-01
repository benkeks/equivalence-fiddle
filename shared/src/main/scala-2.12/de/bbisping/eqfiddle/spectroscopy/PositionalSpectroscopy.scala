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
import de.bbisping.eqfiddle.hml.ObservationClassWeak
import de.bbisping.eqfiddle.hml.ObservationClassWeak.WeaklyClassifiedFormula

class PositionalSpectroscopy[S, A, L] (
    ts: WeakTransitionSystem[S, A, L],
    nodes: List[S])
  extends AbstractSpectroscopy[S, A, L](ts, nodes) {

  override val spectrum = ObservationClassWeak.LTBTS

  def buildStrategyFormulas(game: AbstractSpectroscopyGame[S, A, L])(node: GameNode, possibleMoves: Iterable[Set[WeaklyClassifiedFormula[A]]]): Set[WeaklyClassifiedFormula[A]] = {
    node match {
      case game.DefenderConjunction(_, _) =>
        val productMoves =
          possibleMoves.foldLeft(Seq(Seq[HennessyMilnerLogic.Formula[A]]()))(
            (b, a) => b.flatMap(i => a.map(j => i ++ Seq(j))))
        val moveSet = productMoves.map { mv =>
          val moves = mv.toSet
          WeaklyClassifiedFormula(HennessyMilnerLogic.And(moves))
        }.toSet
        pruneDominated(moveSet)
      case game.AttackerObservation(_, _, game.ConjunctMove) =>
        possibleMoves.flatten.toSet
      case game.AttackerObservation(_, _, game.NegationMove) =>
        pruneDominated(possibleMoves.flatten.map(f => WeaklyClassifiedFormula(HennessyMilnerLogic.Negate(f))).toSet)
      case game.AttackerObservation(_, _, game.ObservationMove(a)) =>
        pruneDominated(possibleMoves.flatten.toSet[WeaklyClassifiedFormula[A]].map(f => WeaklyClassifiedFormula(HennessyMilnerLogic.Observe[A](a, f))))
    }
  }

  def nodeIsRelevantForResults(game: AbstractSpectroscopyGame[S, A, L], gn: GameNode): Boolean = gn match {
    case game.AttackerObservation(p, qq, kind) => (kind == game.ConjunctMove && qq.size == 1)
    case _ => false
  }

  override def pruneDominated(oldFormulas: Set[WeaklyClassifiedFormula[A]]) = {
    val formulaClasses = for {
      f <- oldFormulas
    } yield f.getRootClass()
    for {
      f <- oldFormulas
      cl = f.getRootClass()
      if !formulaClasses.exists(clOther => cl > clOther)
    } yield {
      f
    }
  }

  def buildHML(game: AbstractSpectroscopyGame[S, A, L], win: Set[GameNode], nodes: Set[GameNode]) = {

    val attackGraphBuilder = new AttackGraphBuilder[Set[WeaklyClassifiedFormula[A]]]()

    val attackGraph = attackGraphBuilder.buildAttackGraph(game, win, nodes)

    val accumulatedPrices: Map[GameNode, Set[WeaklyClassifiedFormula[A]]] = attackGraphBuilder.accumulateNodePrices(
      graph = attackGraph,
      pricePick = buildStrategyFormulas(game) _,
      supPrice = Set(),
      nodes = nodes 
    )

    val bisimilarNodes = for {
      gn <- game.discovered
      if gn.isInstanceOf[game.AttackerObservation] && !win(gn)
      if nodeIsRelevantForResults(game, gn)
    } yield (gn, Set[WeaklyClassifiedFormula[A]]())

    val minPrices =
      bisimilarNodes.toMap ++
      (if (discardLanguageDominatedResults)
        accumulatedPrices.mapValues(spectrum.getLeastDistinguishing(_))
      else
        accumulatedPrices)

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