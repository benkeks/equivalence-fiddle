package io.equiv.eqfiddle.game

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should
import io.equiv.eqfiddle.algo.AlgorithmLogging

class GaloisEnergyGameTests extends AnyFunSpec with should.Matchers  {
  
  class EspressoEnergyGame() extends EnergyGame {

    // cups, time, shots, energization
    private def add = EnergyGame.EnergyUpdate.add(_, 4)
    private def minWith = EnergyGame.EnergyUpdate.minWith(_)

    private val NoEnergyUpdate        = new EnergyGame.EnergyUpdate(Array( 0, 0, 0, 0))
    private val ConsumeShot           = new EnergyGame.EnergyUpdate(Array( 0, 0, add(-1), add(1)))
    private val BrewCoffee1           = new EnergyGame.EnergyUpdate(Array( 0, 0, add(1), 0))
    private val BrewCoffee2           = new EnergyGame.EnergyUpdate(Array( 0, 0, minWith(1), 0))
    private val ReturnFromCoffeeMaker = new EnergyGame.EnergyUpdate(Array( 0, add(-2), 0, 0))
    private val ChatWithHead          = new EnergyGame.EnergyUpdate(Array( 0, add(-1), 0, 0))
    private val ShareCoffeeWithHead   = new EnergyGame.EnergyUpdate(Array( add(-1), 0, add(-1), 0))
    private val FinishEnergization    = new EnergyGame.EnergyUpdate(Array( 0, 0, 0, add(-10)))

    case object Office extends SimpleGame.AttackerPosition
    case object CoffeeMaker extends SimpleGame.AttackerPosition
    case object CoffeeProgress extends SimpleGame.AttackerPosition
    case object DepartmentHead extends SimpleGame.DefenderPosition
    case object DepartmentHeadChat extends SimpleGame.DefenderPosition
    case object Energized extends SimpleGame.DefenderPosition

    override def weight(gn1: GamePosition, gn2: GamePosition): EnergyGame.EnergyUpdate = (gn1, gn2) match {
      case (Office, Office) =>
        ConsumeShot
      case (Office, Energized) =>
        FinishEnergization
      case (CoffeeMaker, CoffeeProgress) =>
        BrewCoffee1
      case (CoffeeProgress, CoffeeMaker) =>
        BrewCoffee2
      case (CoffeeMaker, Office) =>
        ReturnFromCoffeeMaker
      case (DepartmentHead, DepartmentHeadChat) =>
        ChatWithHead
      case (DepartmentHead, Office) =>
        ShareCoffeeWithHead
      case _ =>
        NoEnergyUpdate
    }

    def computeSuccessors(gn: GamePosition): Iterable[GamePosition] = gn match {
      case Office =>
        List(Office, CoffeeMaker, Energized)
      case CoffeeMaker =>
        List(CoffeeProgress, Office, DepartmentHead)
      case CoffeeProgress =>
        List(CoffeeMaker)
      case DepartmentHead =>
        List(DepartmentHeadChat, Office)
      case DepartmentHeadChat =>
        List(Office)
      case Energized =>
        List()
    }
  }

  val game = new EspressoEnergyGame()

  def instantAttackerWin(gn: SimpleGame.GamePosition) = gn match {
    case game.Energized => Set(EnergyGame.Energy.zeroEnergy(4))
    case _ => Set.empty
  }
  game.populateGame(
    List(game.Office),
    instantAttackerWin(_))

  describe("The Espresso Energy Game") {
    it("should be winnable for the attacker at the office") {
      game.attackerWinningBudgets(game.Office) should not be empty
    }

    val officePareto = Set(
      EnergyGame.Energy(Array(10,1,0,0)),
      EnergyGame.Energy(Array(5,2,0,0)),
      EnergyGame.Energy(Array(4,4,0,0)),
      EnergyGame.Energy(Array(3,6,0,0)),
      EnergyGame.Energy(Array(2,10,0,0)),
      EnergyGame.Energy(Array(1,20,0,0))
    )

    it("should have (10,1), (5,2), (4,4), (3,6), (2,10), (1,20) as optimal time/cup values for shots and energy 0 at Office") {
      game.attackerWinningBudgets(game.Office).filter(e => e(2) == 0 && e(3) == 0).toSet should equal(officePareto)
    }

  }
}
