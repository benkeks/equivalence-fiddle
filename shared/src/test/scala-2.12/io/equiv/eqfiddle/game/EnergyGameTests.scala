package io.equiv.eqfiddle.game

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should
import io.equiv.eqfiddle.algo.AlgorithmLogging

class EnergyGameTests extends AnyFunSpec with should.Matchers  {
  
  class TestEnergyGame() extends EnergyGame[SimpleGame.GamePosition] {

    private def add = EnergyGame.EnergyUpdate.add(_, 2)

    case object G1 extends SimpleGame.AttackerPosition
    case object G2 extends SimpleGame.AttackerPosition
    case object G3 extends SimpleGame.AttackerPosition
    case object G4 extends SimpleGame.DefenderPosition
    case object G5 extends SimpleGame.AttackerPosition
    case object G6 extends SimpleGame.DefenderPosition

    type GamePosition = SimpleGame.GamePosition

    val graph: Map[(SimpleGame.GamePosition, SimpleGame.GamePosition), EnergyGame.EnergyUpdate] = Set(
      (G1, G2) -> EnergyGame.EnergyUpdate(Array(0, 0)),
      (G1, G3) -> EnergyGame.EnergyUpdate(Array(add(-1), 0)),
      (G1, G4) -> EnergyGame.EnergyUpdate(Array(add(-2), add(-1))),
      (G2, G4) -> EnergyGame.EnergyUpdate(Array(add(2), add(-1))),
      (G3, G4) -> EnergyGame.EnergyUpdate(Array(0, 0)),
      (G4, G5) -> EnergyGame.EnergyUpdate(Array(0, 0)),
      (G4, G6) -> EnergyGame.EnergyUpdate(Array(add(-1), 0)),
      (G5, G6) -> EnergyGame.EnergyUpdate(Array(0, add(-1)))
    ).toMap

    override def weight(gn1: GamePosition, gn2: GamePosition): EnergyGame.EnergyUpdate = graph((gn1, gn2))

    def computeSuccessors(gn: GamePosition): Iterable[GamePosition] = {
      for {
        (gn1, gn2) <- graph.keys
        if gn1 == gn
      } yield gn2
    }
  }

  val game = new TestEnergyGame()

  def instantAttackerWin(gn: SimpleGame.GamePosition) = gn match {
    case game.G6 => Set(EnergyGame.Energy.zeroEnergy(2))
    case _ => Set.empty
  }
  game.populateGame(
    List(game.G1),
    instantAttackerWin(_))

  describe("The Example Energy Game") {
    it("should be winnable for the attacker at G2") {
      game.attackerWinningBudgets(game.G2) should not be empty
    }

    val pareto = Set(
      EnergyGame.Energy(Array(0,2)),
      EnergyGame.Energy(Array(2,1))
    )

    it("should have (0,2), (2,1) as optimal winning budgets at G1") {
      game.attackerWinningBudgets(game.G1).toSet should equal(pareto)
    }

  }
}
