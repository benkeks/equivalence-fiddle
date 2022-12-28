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
import io.equiv.eqfiddle.hml.ObservationClass
import io.equiv.eqfiddle.hml.Spectrum
import io.equiv.eqfiddle.hml.HMLInterpreter
import io.equiv.eqfiddle.util.Coloring

abstract class AbstractSpectroscopy[S, A, L, CF <: HennessyMilnerLogic.Formula[A]] (
    val ts: WeakTransitionSystem[S, A, L])
  extends SpectroscopyInterface[S, A, L, CF] with AlgorithmLogging[S] {

  import SpectroscopyInterface._

  def buildStrategyFormulas(game: AbstractSpectroscopyGame[S, A, L])(node: GameNode, possibleMoves: Iterable[Set[CF]]): Set[CF]

  def pruneDominated(oldFormulas: Set[CF]): Set[CF]

  def compute(comparedPairs: Iterable[(S,S)]): SpectroscopyResult[S, A, ObservationClass, CF]

  /* Discards distinguishing formulas that do not contribute “extreme” distinguishing notions of equivalence */
  val discardLanguageDominatedResults: Boolean = false

  def nodeIsRelevantForResults(game: AbstractSpectroscopyGame[S, A, L], gn: GameNode): Boolean

  def collectSpectroscopyResult(
    game: AbstractSpectroscopyGame[S, A, L],
    nodeFormulas: Map[GameNode, Iterable[CF]])
  : SpectroscopyResult[S, A, ObservationClass, CF] = {
    
    val bestPreorders: Map[GameNode,List[Spectrum.EquivalenceNotion[ObservationClass]]] = nodeFormulas.mapValues { ffs =>
      val classes = ffs.flatMap(spectrum.classifyFormula(_)._2)
      spectrum.getStrongestPreorderClass(classes)
    }

    val spectroResults = for {
      gn <- game.discovered
      if gn.isInstanceOf[game.AttackerObservation]
      game.AttackerObservation(p, qq, kind) = gn
      if nodeIsRelevantForResults(game, gn)
      q <- qq
      preorders <- bestPreorders.get(gn)
      distinctionFormulas = nodeFormulas(gn)
      distinctions = for {
        f <- distinctionFormulas.toList
        (price, eqs) = spectrum.classifyFormula[CF](f)
      } yield (f, price, eqs)
    } yield SpectroscopyResultItem[S, A, ObservationClass, CF](p, q, distinctions, preorders)

    SpectroscopyResult(spectroResults.toList, spectrum)
  }

  def checkDistinguishing(formula: CF, p: S, q: S) = {
    val hmlInterpreter = new HMLInterpreter(ts)
    val check = hmlInterpreter.isTrueAt(formula, List(p, q))
    if (!check(p) || check(q)) {
      System.err.println("Formula " + formula.toString() + " is no sound distinguishing formula! " + check)
    }
  }

  def gameEdgeToLabel(game: AbstractSpectroscopyGame[S, A, L], gn1: GameNode, gn2: GameNode): String

  def graphvizGameWithFormulas(game: AbstractSpectroscopyGame[S, A, L], win: Set[GameNode], formulas: Map[GameNode, Iterable[CF]]) = {
    val visualizer = new GameGraphVisualizer(game) {

      def nodeToID(gn: GameNode): String = gn.hashCode().toString()

      def nodeToString(gn: GameNode): String = {
        val formulaString = formulas.getOrElse(gn,Set()).mkString("\\n").replaceAllLiterally("⟩⊤","⟩")
        (gn match {
          case game.AttackerObservation(p, qq: Set[_], kind) =>
            val qqString = qq.mkString("{",",","}")
            s"$p, $qqString, $kind"
          case game.DefenderConjunction(p, qqPart: List[Set[_]]) =>
            val qqString = qqPart.map(_.mkString("{",",","}")).mkString("/")
            s"$p, $qqString"
          case _ => ""
        }).replaceAllLiterally(".0", "") + (if (formulaString != "") s"\\n------\\n$formulaString" else "")
      }

      def edgeToLabel(gn1: GameNode, gn2: GameNode) = gameEdgeToLabel(game, gn1, gn2)
    }

    visualizer.outputDot(win)
  }

}
