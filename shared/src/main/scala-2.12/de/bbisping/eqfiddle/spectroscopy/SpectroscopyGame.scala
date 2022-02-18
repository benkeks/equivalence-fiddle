package de.bbisping.eqfiddle.hml

import de.bbisping.eqfiddle.game.SimpleGame
import de.bbisping.eqfiddle.game.WinningRegionComputation
import de.bbisping.eqfiddle.game.GameDiscovery
import de.bbisping.eqfiddle.ts.WeakTransitionSystem
import de.bbisping.eqfiddle.util.Partition

class SpectroscopyGame[S, A, L](val ts: WeakTransitionSystem[S, A, L], init: Iterable[(S, Set[S])])
  extends SimpleGame with GameDiscovery with WinningRegionComputation {
  
  abstract sealed class MoveKind
  case class ObservationMove(a: A) extends MoveKind {
    override def toString() = "⟨" + a + "⟩"
  }
  case object ConjunctMove extends MoveKind {
    override def toString() = "⋀"
  }
  case object NegationMove extends MoveKind {
    override def toString() = "¬"
  }
  case object DefenderMove extends MoveKind {
    override def toString() = "*"
  }

  case class AttackerObservation(p: S, qq: Set[S], arrivingMove: MoveKind) extends SimpleGame.AttackerNode
  case class DefenderConjunction(p: S, qqPart: List[Set[S]]) extends SimpleGame.DefenderNode

  override def initialNodes: Iterable[GameNode] = {
    init map { case (p0, qq0) => AttackerObservation(p0, qq0, ConjunctMove) }
  }

  def successors(gn: GameNode): Iterable[GameNode] = gn match {
    case AttackerObservation(p0, qq0, moveKind) =>
      if (qq0 contains p0) {
        List()
      } else {
        val dn = for {
          (a,pp1) <- ts.post(p0)
          p1 <- pp1
        } yield {
          AttackerObservation(p1,
            qq0.flatMap(ts.post(_, a)),
            ObservationMove(a)
          )
        }
        if (qq0.size == 1 && moveKind == ConjunctMove) {
          // wlog only have negation moves when the defender is focused (which can be forced by the attacker using preceding conjunctions)
          val neg = AttackerObservation(qq0.head, Set(p0), NegationMove)
          dn ++ List(neg)
        } else if (moveKind.isInstanceOf[ObservationMove]) {
          val conjMoves = for {
            parts <- Partition.partitioningListsOfSet(qq0)
            //if parts.length == qq0.size // this is equivalent to the original algorithm's game
            //if parts.length != 1 // drop the trivial partitioning
          } yield {
            DefenderConjunction(p0, parts)
          }
          dn ++ conjMoves
        } else {
          dn
        }
      }
    case DefenderConjunction(p0, qqPart0) =>
      for {
        qq0 <- qqPart0
      } yield {
        AttackerObservation(p0, qq0, ConjunctMove)
      }
  }
}
