package io.equiv.eqfiddle.spectroscopy

import io.equiv.eqfiddle.algo.AlgorithmLogging

import io.equiv.eqfiddle.hml.HML
import io.equiv.eqfiddle.hml.Interpreter
import io.equiv.eqfiddle.hml.Spectrum

import io.equiv.eqfiddle.game.SimpleGame
import io.equiv.eqfiddle.game.GameGraphVisualizer
import io.equiv.eqfiddle.game.EnergyGame
import io.equiv.eqfiddle.game.EnergyGame.Energy

import io.equiv.eqfiddle.ts.WeakTransitionSystem

/** The trait abstractly implements the spectroscopy decision procedure for the `compute` function on the spectroscopy trait
  * The handling of specific spectra, spectroscopy games and their logics must be supplied by implementing methods. */
trait SpectroscopyFramework[S, A, L, CF <: HML.Formula[A]]
    extends AlgorithmLogging[S] {
  self: Spectroscopy[S, A, L, CF] =>

  /** Types of spectroscopy game and its game positions. */
  type SpectroscopyGame <: EnergyGame[GamePosition]
  type GamePosition <: SimpleGame.GamePosition

  /** Construct a spectroscopy game object */
  def openSpectroscopyGame(configuration: Spectroscopy.Config = Spectroscopy.Config()): SpectroscopyGame
  
  /** Convert between relation items on the transiton system and positions in the spectroscopy game. */
  def relationItemToGamePosition(p: S, q: S): GamePosition
  def gamePositionToRelationItem(gp: GamePosition): Option[(S, S)]

  /** Convert between notions and energy vectors. */
  def notionToEnergy(obsNotion: Notion): Energy
  def energyToNotion(e: Energy): Notion

  /** Build witness formulas for the given game position and price. (Aka “strategy formulas.”) Should populate `distigushingFormulas` mapping. */
  def buildHMLWitness(
    game: SpectroscopyGame,
    node: GamePosition,
    price: Energy
  ): Iterable[CF]

  /** String representation of game positions for graphviz game */
  def gamePositionToString(gn: GamePosition): String
  /** String IDs of game positions for graphviz game */
  def gamePositionToID(gn: GamePosition): String

  /** Place to memoize distinguishing formulas per position. (This side-effect member should be used to make the formulas available in game and spectrum output.) */
  val distinguishingFormulas =
    collection.mutable.Map[(GamePosition, Energy), Iterable[CF]]()

  def decideAll(
    comparedPairs: Iterable[(S,S)],
    config: Spectroscopy.Config = Spectroscopy.Config()
  ) : Spectroscopy.Result[S, A, Notion, CF] = {

    debugLog(s"Start spectroscopy on ${ts.nodes.size} node transition system with ${comparedPairs.size} compared pairs.")

    val spectroscopyGame = openSpectroscopyGame(config)

    val init = for {
      (p, q) <- comparedPairs
      start <- List(relationItemToGamePosition(p, q), relationItemToGamePosition(q, p))
    } yield start

    debugLog("HML spectroscopy game construction ...")

    spectroscopyGame.populateGame(init)

    debugLog("HML spectroscopy game size: " + spectroscopyGame.discovered.size)

    // if enabled, build distinguishing formulas for the compared states. (assumes side-effect: that `distinguishingFormulas` is populated by this construction.)
    // if there are problems, print errors / warnings. (these problems mean that the algorithm might be buggy! they should never appear in practice, if the implementation is correct.)
    if (config.computeFormulas) {
      for {
        gn <- init
        (p, q) <- gamePositionToRelationItem(gn)
        bestPrice <- spectroscopyGame.attackerWinningBudgets(gn)
        energyClass = energyToNotion(bestPrice)
      } {
        val witness = buildHMLWitness(spectroscopyGame, gn, bestPrice).headOption
        if (witness.isEmpty) {
          AlgorithmLogging.debugLog(s"ERROR: No witness formula for $gn with price ${bestPrice} found!", logLevel = 4)
        }
        for (witnessFormula <- witness) {
          val (formulaPrice, correspondingNotions) = spectrum.classifyFormula(witnessFormula)
          if (! (formulaPrice <= energyClass) ) {
            AlgorithmLogging.debugLog(s"ERROR: Formula $witnessFormula ${formulaPrice.toTuple} too expensive; not below ${energyClass.toTuple}.", logLevel = 4)
          } else {
            if (formulaPrice < energyClass) {
              AlgorithmLogging.debugLog(s"WARNING: Witness formula $witnessFormula ${formulaPrice.toTuple} is strictly cheaper than determined minimal distinction class ${energyClass.toTuple}!", logLevel = 5)
            }
            debugLog(s"Distinguished at ${energyClass.toTuple}, ${spectrum.classifyNotion(energyClass)} preorder by ${witnessFormula}; Price: $formulaPrice, $correspondingNotions")
            checkDistinguishing(witnessFormula, p, q)
          }
        }
      }
    }

    // pick price-minimal formulas for each game position
    val distinguishingPositionFormulas = (if (config.computeFormulas) {
       for {
        (node, pricedFormulas) <- distinguishingFormulas
          .toSet[((GamePosition, Energy), Iterable[CF])]
          .groupBy(kv => kv._1._1)
        formulas = for {
          (_, formulasForPrice) <- pricedFormulas
          f <- spectrum.selectCheapest(formulasForPrice)
        } yield f
      } yield (node, formulas)
    } else Map[GamePosition, Set[CF]]()).withDefaultValue(Set[CF]())

    // select the best preorders to relate the states
    val bestPreorders: Map[GamePosition,(Set[Notion],List[Spectrum.EquivalenceNotion[Notion]])] =
      spectroscopyGame.attackerWinningBudgets.toMap.mapValues { energies =>
      val fcs = energies.toSet[Energy].map(energyToNotion(_))
      (fcs, spectrum.getStrongestPreorderClassFromClass(fcs))
    }

    // assemble output in spectroscopy result object.
    val spectroResults = for {
      gn <- spectroscopyGame.discovered
      (p, q) <- gamePositionToRelationItem(gn)
      (prices, preorders) <- bestPreorders.get(gn)
      distinctions = if (!config.computeFormulas) {
        for {
          price <- prices
          formula = HML.True[A].asInstanceOf[CF] // if no formulas have been computed, take True as dummy
        } yield (formula, price, spectrum.classifyNotion(price))
      } else {
        val distinctionFormulas = distinguishingPositionFormulas(gn)
        for {
          f <- distinctionFormulas.toList
          (fPrice, eqs) = spectrum.classifyFormula(f)
        } yield (f, fPrice, eqs)
      }
    } yield Spectroscopy.ResultItem[S, A, Notion, CF](p, q, distinctions.toList, preorders)

    // collect some diagnostic information (unless disabled)
    val (gamePositionNum, gameMoveNum) = if (config.saveGameSize) spectroscopyGame.gameSize() else (0, 0)

    val gameString = debugLog(
      graphvizGameWithFormulas(spectroscopyGame, spectroscopyGame.attackerWinningBudgets.toMap, distinguishingPositionFormulas),
      asLink = "https://edotor.net/?engine=dot#"
    )

    Spectroscopy.Result[S, A, Notion, CF](
      spectroResults.toList, spectrum,
      meta = Map(
        "game" -> gameString,
        "game-positions" -> gamePositionNum.toString,
        "game-moves" -> gameMoveNum.toString)
    )
  }

  /** Check if the formula is a distinguishing formula for the given positions. Print an error to debug log if not. */
  def checkDistinguishing(formula: HML.Formula[A], p: S, q: S) = {
    val Interpreter = new Interpreter(ts)
    val check = Interpreter.isTrueAt(formula, List(p, q))
    if (!check(p) || check(q)) {
      AlgorithmLogging.debugLog("ERROR: Formula " + formula.toString() + " is no sound distinguishing formula! " + check, logLevel = 4)
      false
    } else {
      true
    }
  }

  def graphvizGameWithFormulas(
    spectroscopyGame: SpectroscopyGame,
    attackerWinningBudgets: Map[GamePosition, Iterable[Energy]],
    formulas: Map[GamePosition, Set[CF]]
  ) = {
    val visualizer = new GameGraphVisualizer(spectroscopyGame) {

      def positionToID(gn: GamePosition): String =
        gamePositionToID(gn)

      def positionToString(gn: GamePosition): String = {
        val budgetString = attackerWinningBudgets.getOrElse(gn,Set()).map(_.vector.mkString("(",",",")")).mkString(" / ")
        val formulaString = formulas.getOrElse(gn,Set()).mkString("\\n").replaceAllLiterally("⟩⊤","⟩")
        gamePositionToString(gn) +
         (if (budgetString != "") s"\\n------\\n$budgetString" else "") +
         (if (formulaString != "") s"\\n------\\n$formulaString" else "")
      }

      def moveToLabel(gn1: GamePosition, gn2: GamePosition) = spectroscopyGame.weight(gn1, gn2).toString()
    }

    val attackerWin = attackerWinningBudgets.filter(_._2.nonEmpty).keySet.toSet

    visualizer.outputDot(attackerWin)
  }
}