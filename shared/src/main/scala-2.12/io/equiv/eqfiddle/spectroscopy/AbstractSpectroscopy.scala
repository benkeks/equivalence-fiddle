package io.equiv.eqfiddle.spectroscopy

import io.equiv.eqfiddle.util.Relation
import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.util.FixedPoint
import io.equiv.eqfiddle.game.WinningRegionComputation
import io.equiv.eqfiddle.game.AttackGraphBuilder
import io.equiv.eqfiddle.game.SimpleGame.GamePosition
import io.equiv.eqfiddle.algo.AlgorithmLogging
import io.equiv.eqfiddle.util.LabeledRelation
import io.equiv.eqfiddle.game.GameGraphVisualizer
import io.equiv.eqfiddle.hml.HennessyMilnerLogic
import io.equiv.eqfiddle.hml.ObservationNotion
import io.equiv.eqfiddle.hml.Spectrum
import io.equiv.eqfiddle.hml.HMLInterpreter
import io.equiv.eqfiddle.util.Coloring

abstract class AbstractSpectroscopy[S, A, L, CF <: HennessyMilnerLogic.Formula[A]] (
    val ts: WeakTransitionSystem[S, A, L])
  extends SpectroscopyInterface[S, A, L, CF] with AlgorithmLogging[S] {

  import SpectroscopyInterface._

  def buildStrategyFormulas(game: AbstractSpectroscopyGame[S, A, L])(node: GamePosition, possibleMoves: Iterable[Set[CF]]): Set[CF]

  def pruneDominated(oldFormulas: Set[CF]): Set[CF]

  def compute(
    comparedPairs: Iterable[(S,S)],
    config: SpectroscopyInterface.SpectroscopyConfig = SpectroscopyInterface.SpectroscopyConfig()
  ): SpectroscopyResult[S, A, ObservationNotion, CF]

  /* Discards distinguishing formulas that do not contribute “extreme” distinguishing notions of equivalence */
  val discardLanguageDominatedResults: Boolean = false

  def nodeIsRelevantForResults(game: AbstractSpectroscopyGame[S, A, L], gn: GamePosition): Boolean

  def collectSpectroscopyResult(
    game: AbstractSpectroscopyGame[S, A, L],
    nodeFormulas: Map[GamePosition, Iterable[CF]])
  : SpectroscopyResult[S, A, ObservationNotion, CF] = {
    
    val bestPreorders: Map[GamePosition,List[Spectrum.EquivalenceNotion[ObservationNotion]]] = nodeFormulas.mapValues { ffs =>
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
    } yield SpectroscopyResultItem[S, A, ObservationNotion, CF](p, q, distinctions, preorders)

    SpectroscopyResult(spectroResults.toList, spectrum)
  }

  def checkDistinguishing(formula: CF, p: S, q: S) = {
    val hmlInterpreter = new HMLInterpreter(ts)
    val check = hmlInterpreter.isTrueAt(formula, List(p, q))
    if (!check(p) || check(q)) {
      AlgorithmLogging.debugLog("Formula " + formula.toString() + " is no sound distinguishing formula! " + check, logLevel = 4)
    }
  }

  def gameMoveToLabel(game: AbstractSpectroscopyGame[S, A, L], gn1: GamePosition, gn2: GamePosition): String

  def graphvizGameWithFormulas(game: AbstractSpectroscopyGame[S, A, L], win: Set[GamePosition], formulas: Map[GamePosition, Iterable[CF]]) = {
    val visualizer = new GameGraphVisualizer(game) {

      def positionToID(gn: GamePosition): String = gn.hashCode().toString()

      def positionToString(gn: GamePosition): String = {
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

      def moveToLabel(gn1: GamePosition, gn2: GamePosition) = gameMoveToLabel(game, gn1, gn2)
    }

    visualizer.outputDot(win)
  }

}
