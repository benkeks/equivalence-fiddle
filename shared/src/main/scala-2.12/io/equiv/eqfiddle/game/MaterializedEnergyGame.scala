package io.equiv.eqfiddle.game

import io.equiv.eqfiddle.game.SimpleGame.{GamePosition, AttackerPosition, DefenderPosition}

object MaterializedEnergyGame {
  trait MaterializedGamePosition[G <: SimpleGame.GamePosition, P] extends SimpleGame.GamePosition

  case class MaterializedAttackerPosition[G <: SimpleGame.GamePosition, P](baseNode: G, energy: P) extends SimpleGame.AttackerPosition with MaterializedGamePosition[G, P]
  case class MaterializedDefenderPosition[G <: SimpleGame.GamePosition, P](baseNode: G, energy: P) extends SimpleGame.DefenderPosition with MaterializedGamePosition[G, P]
}

class MaterializedEnergyGame[G <: SimpleGame.GamePosition, P](
    val baseGame: GameLazyDecision[G, P],
    initialBaseNodes: Iterable[G],
    initialEnergy: P,
    energyUpdate: (G, G, P) => Option[P],
    preferredNodes: (G, P, G) => Boolean)
  extends SimpleGame[MaterializedEnergyGame.MaterializedGamePosition[G, P]]
  with GameDiscovery[MaterializedEnergyGame.MaterializedGamePosition[G, P]]
  with WinningRegionComputation[MaterializedEnergyGame.MaterializedGamePosition[G, P]] {

  import MaterializedEnergyGame._

  type GamePosition = MaterializedGamePosition[G, P]

  def materialize(baseNode: G, energy: P) = {
    if (baseNode.isInstanceOf[AttackerPosition]) {
      MaterializedAttackerPosition(baseNode, energy)
    } else {
      MaterializedDefenderPosition(baseNode, energy)
    }
  }

  override def successors(gn: GamePosition): Iterable[GamePosition] = {
    val (baseNode, energy) = gn match {
      case MaterializedAttackerPosition(b, e) => (b, e)
      case MaterializedDefenderPosition(b, e) => (b, e)
    }
    for {
      s <- baseGame.computeSuccessors(baseNode)
      updatedEnergy <- energyUpdate(baseNode, s, energy)
      if preferredNodes(baseNode, energy, s)
    } yield materialize(s, updatedEnergy)
  }

  populate(initialBaseNodes.map(materialize(_, initialEnergy)))
}