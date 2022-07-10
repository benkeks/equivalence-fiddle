package de.bbisping.eqfiddle.spectroscopy

import de.bbisping.eqfiddle.game.SimpleGame
import de.bbisping.eqfiddle.game.WinningRegionComputation
import de.bbisping.eqfiddle.game.GameDiscovery
import de.bbisping.eqfiddle.ts.WeakTransitionSystem
import de.bbisping.eqfiddle.util.Partition

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

  case class AttackerObservation(p: S, qq: Set[S], arrivingMove: MoveKind) extends SimpleGame.AttackerNode
  case class DefenderConjunction(p: S, qqPart: List[Set[S]]) extends SimpleGame.DefenderNode
}
