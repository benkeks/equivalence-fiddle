package de.bbisping.coupledsim.hml

import de.bbisping.coupledsim.game.SimpleGame
import de.bbisping.coupledsim.game.GameDiscovery
import de.bbisping.coupledsim.game.WinningRegionComputation
import de.bbisping.coupledsim.ts.WeakTransitionSystem

object HMLSpectroscopyGame {

  abstract sealed class MoveKind[A]
  case class ObservationMove[A](a: A) extends MoveKind[A] {
    override def toString() = "⟨" + a + "⟩"
  }

  // case class PassingMove() extends MoveKind {
  //   override def toString() = "⟨ϵ⟩"
  // }
  case class ImmediacyMove[A]() extends MoveKind[A] {
    override def toString() = "!"
  }
  
  case class ConjunctMove[A]() extends MoveKind[A] {
    override def toString() = "⋀"
  }
  case class NegationMove[A]() extends MoveKind[A] {
    override def toString() = "¬"
  }
  case class DefenderMove[A]() extends MoveKind[A] {
    override def toString() = "*"
  }

}

class HMLSpectroscopyGame[S,A,L](initialStates: Iterable[(S, Set[S])], ts: WeakTransitionSystem[S,A,L])
  extends SimpleGame with GameDiscovery with WinningRegionComputation {

  import HMLSpectroscopyGame._

  lazy val recordedMoveEdges = collection.mutable.Map[(GameNode, GameNode), MoveKind[A]]()

  override def initialNodes: Iterable[SimpleGame.GameNode] = 
    initialStates.toSet[(S, Set[S])].map { case (p, qq) => AttackerObservation(p, qq) }

  case class AttackerObservation(p: S, qq: Set[S], immediacy: Boolean = false) extends SimpleGame.AttackerNode 
  case class DefenderConjunction(p: S, qq: Set[S]) extends SimpleGame.DefenderNode

  def successors(gn: GameNode): Iterable[GameNode] = gn match {
    case AttackerObservation(p0, qq0, immediacy) =>
      val dn = for {
        (a,pp1) <- ts.post(p0)
        if !(immediacy && ts.silentActions(a)) // <- allow attacker to move down tau steps only in non immediate mode, for now
        p1 <- pp1
        next = if (immediacy) {
          AttackerObservation(p1, qq0.flatMap(ts.post(_, a)), immediacy = false)
        } else {
          AttackerObservation(p1, qq0.flatMap(ts.weakPostDelay(_, a)), immediacy = false)
        }
        
      } yield {
        recordedMoveEdges((gn, next)) = ObservationMove(a)
        next
      }

      //TODO only if no immediacy!
      val immediate = AttackerObservation(p0, qq0, immediacy = true)
      recordedMoveEdges((gn, immediate)) = ImmediacyMove()
      
      val conj = if (immediacy) {
        DefenderConjunction(p0, qq0)
      } else {
        DefenderConjunction(p0, qq0.flatMap(ts.silentReachable(_)))
      }
      recordedMoveEdges((gn, conj)) = ConjunctMove()

      (if (qq0.size == 1) {
        // wlog only have negation moves when the defender is focused (which can be forced by the attacker using a preceding conjunction)
        val neg = AttackerObservation(qq0.head, Set(p0))
        recordedMoveEdges((gn, neg)) = NegationMove()
        List(neg)
      } else {
        List()
        // immediate conjunct moves only make sense if the defender is spread
      }) ++ dn ++ List(immediate, conj)
    case DefenderConjunction(p0, qq0) =>
      for {
        q0 <- qq0
        obs = AttackerObservation(p0, Set(q0))
      } yield {
        recordedMoveEdges((gn, obs)) = DefenderMove()
        obs
      }
  }
}