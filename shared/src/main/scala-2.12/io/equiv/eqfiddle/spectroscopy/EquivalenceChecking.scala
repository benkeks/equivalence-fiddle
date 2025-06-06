package io.equiv.eqfiddle.spectroscopy

import io.equiv.eqfiddle.hml.HML

import io.equiv.eqfiddle.game.SimpleGame
import io.equiv.eqfiddle.game.GameGraphVisualizer
import io.equiv.eqfiddle.game.EnergyGame.Energy
import io.equiv.eqfiddle.game.MaterializedEnergyGame
import io.equiv.eqfiddle.game.MaterializedEnergyGame._

/** This trait adds checking for individual equivalences to the spectroscopy approach */
trait EquivalenceChecking[S, A, L, CF <: HML.Formula[A]]
  extends SpectroscopyFramework[S, A, L, CF] {
  self: Spectroscopy[S, A, L, CF] =>

  /* whether to consider the baseSuccessor as a relevant move for the attacker in derived equivalence games */
  def preferredPositions(config: Spectroscopy.Config)(currentBaseNode: GamePosition, currentEnergy: Energy, baseSuccessor: GamePosition): Boolean

  /** Position type for derived equivalence games. */
  type MaterializedPosition = MaterializedGamePosition[GamePosition, Energy]

  def checkIndividualPreorder(
      comparedPairs: Iterable[(S,S)],
      notion: String,
      config: Spectroscopy.Config = Spectroscopy.Config()
  ) : Spectroscopy.IndividualNotionResult[S] = {
    val spectroscopyGame = openSpectroscopyGame(config)
    val init = for {
      (p, q) <- comparedPairs
      start <- List(relationItemToGamePosition(p, q), relationItemToGamePosition(q, p))
    } yield start

    val notionEnergy = notionToEnergy(spectrum.getSpectrumClass(notion).obsNotion)

    def energyUpdate(gn1: GamePosition, gn2: GamePosition, energy: Energy): Option[Energy] = {
      val update = spectroscopyGame.weight(gn1, gn2)
      val newEnergy = update.applyEnergyUpdateInfinity(energy)
      if (gn1.isInstanceOf[SimpleGame.DefenderPosition] || newEnergy.isNonNegative())
        Some(newEnergy)
      else
        None
    }

    val reachabilityGame: MaterializedEnergyGame[GamePosition, Energy] = new MaterializedEnergyGame[GamePosition, Energy](
      spectroscopyGame, init, notionEnergy, energyUpdate, if (config.useCleverInstanceBranching) preferredPositions(config) else ((_ ,_ ,_ ) => true))

    val attackerWins = reachabilityGame.computeWinningRegion()

    val (gamePositionNum, gameMoveNum) = if (config.saveGameSize) reachabilityGame.gameSize() else (0, 0)

    val gameString = debugLog(
      graphvizMaterializedGame(reachabilityGame, attackerWins),
      asLink = "https://edotor.net/?engine=dot#"
    )

    val relation: Set[(S, String, S)] = for {
      gn <- reachabilityGame.discovered.toSet
      if !attackerWins(gn)
      (p, eString, q) <- gn match {
        case MaterializedAttackerPosition(gn, energy) if energy == notionEnergy =>
          gamePositionToRelationItem(gn).map { case (p, q) => (p, "", q) }
        case _ =>
          None
      }
    } yield (p, eString,  q)

    val items = for {
      (p, q) <- comparedPairs
    } yield {
      Spectroscopy.IndividualNotionResultItem(p, q, relation.contains((p, "", q)))
    }
    Spectroscopy.IndividualNotionResult(
      items,
      relation, 
      meta = Map(
        "game" -> gameString,
        "game-positions" -> gamePositionNum.toString,
        "game-moves" -> gameMoveNum.toString
      )
    )
  }

  def materializedToBaseGamePosition(gn: MaterializedPosition) = gn match {
    case MaterializedAttackerPosition(bgn, e) =>
      bgn
    case MaterializedDefenderPosition(bgn, e) =>
      bgn
  }

  def graphvizMaterializedGame(
      game: MaterializedEnergyGame[GamePosition, Energy],
      attackerWin: Set[MaterializedPosition]
  ) = {
    val baseGame = game.baseGame.asInstanceOf[SpectroscopyGame]
    val maxIntString = Int.MaxValue.toString()
    val visualizer = new GameGraphVisualizer(game) {

      def positionToID(gn: MaterializedPosition): String = gn.hashCode().toString()

      def positionToString(gn: MaterializedPosition): String = gn match {
        case MaterializedAttackerPosition(bgn, e) =>
          gamePositionToString(bgn) + "\\n" + e.toString().replaceAllLiterally(maxIntString, "∞")
        case MaterializedDefenderPosition(bgn, e) =>
          gamePositionToString(bgn) + "\\n" + e.toString().replaceAllLiterally(maxIntString, "∞")
      }

      def moveToLabel(gn1: MaterializedPosition, gn2: MaterializedPosition) = {
        baseGame.weight(materializedToBaseGamePosition(gn1), materializedToBaseGamePosition(gn2)).toString()
      }

    }

    visualizer.outputDot(attackerWin)
  }
}
