package io.equiv.eqfiddle.spectroscopy

import io.equiv.eqfiddle.game.SimpleGame
import io.equiv.eqfiddle.game.WinningRegionComputation
import io.equiv.eqfiddle.game.GameDiscovery
import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.util.Partition

abstract class AbstractSpectroscopyGame[S, A, L](val ts: WeakTransitionSystem[S, A, L], init: Iterable[(S, Set[S])])
  extends SimpleGame with GameDiscovery with WinningRegionComputation {
  
  abstract sealed class MoveKind
  case class ObservationMove(a: A) extends MoveKind {
    override def toString() = "⟨" + a + "⟩"
  }
  case object ConjunctMove extends MoveKind {
    override def toString() = "⋀"
  }
  case object PassingMove extends MoveKind {
    override def toString() = "ϵ"
  }
  case object NegationMove extends MoveKind {
    override def toString() = "¬"
  }
  case object DefenderMove extends MoveKind {
    override def toString() = "*"
  }

  /* This will abort the game construction in nodes where the attacker cannot win because p is contained in qq. */
  val optimizeSymmetryDefWins: Boolean = false

  case class AttackerObservation(p: S, qq: Set[S], arrivingMove: MoveKind) extends SimpleGame.AttackerPosition
  case class DefenderConjunction(p: S, qqPart: List[Set[S]]) extends SimpleGame.DefenderPosition
}
