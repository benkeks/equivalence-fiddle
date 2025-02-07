package io.equiv.eqfiddle.game

import io.equiv.eqfiddle.game.SimpleGame.{GameNode, AttackerNode, DefenderNode}

class MaterializedEnergyGame[P](
    baseGame: SimpleGame,
    initialBaseNodes: Iterable[GameNode],
    initialEnergy: P,
    energyUpdate: (GameNode, GameNode, P) => P)
  extends SimpleGame with GameDiscovery with WinningRegionComputation {
  
  case class MaterializedAttackerNode(baseNode: GameNode, energy: P) extends AttackerNode
  case class MaterializedDefenderNode(baseNode: GameNode, energy: P) extends DefenderNode

  def materialize(baseNode: GameNode, energy: P) = {
    if (baseNode.isInstanceOf[AttackerNode]) {
      MaterializedAttackerNode(baseNode, energy)
    } else {
      MaterializedDefenderNode(baseNode, energy)
    }
  }

  override def initialNodes: Iterable[GameNode] = {
    initialBaseNodes.map(materialize(_, initialEnergy))
  }

  override def successors(gn: GameNode): Iterable[GameNode] = {
    val (baseNode, energy) = gn match {
      case MaterializedAttackerNode(baseNode, energy) => (baseNode, energy)
      case MaterializedDefenderNode(baseNode, energy) => (baseNode, energy)
    }
    for {
      s <- baseGame.successors(baseNode)
      updatedEnergy = energyUpdate(baseNode, s, energy)
    } yield materialize(baseNode, updatedEnergy)
  }
}