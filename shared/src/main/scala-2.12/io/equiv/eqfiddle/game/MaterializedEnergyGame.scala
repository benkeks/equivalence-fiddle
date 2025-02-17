package io.equiv.eqfiddle.game

import io.equiv.eqfiddle.game.SimpleGame.{GamePosition, AttackerPosition, DefenderPosition}

class MaterializedEnergyGame[P](
    val baseGame: GameLazyDecision[P],
    initialBaseNodes: Iterable[GamePosition],
    initialEnergy: P,
    energyUpdate: (GamePosition, GamePosition, P) => Option[P],
    preferredNodes: (GamePosition, P, GamePosition) => Boolean)
  extends SimpleGame with GameDiscovery with WinningRegionComputation {
  
  case class MaterializedAttackerPosition(baseNode: GamePosition, energy: P) extends AttackerPosition
  case class MaterializedDefenderPosition(baseNode: GamePosition, energy: P) extends DefenderPosition

  def materialize(baseNode: GamePosition, energy: P) = {
    if (baseNode.isInstanceOf[AttackerPosition]) {
      MaterializedAttackerPosition(baseNode, energy)
    } else {
      MaterializedDefenderPosition(baseNode, energy)
    }
  }

  override def initialPositions: Iterable[GamePosition] = {
    initialBaseNodes.map(materialize(_, initialEnergy))
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
}