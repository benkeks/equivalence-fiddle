package io.equiv.eqfiddle.spectroscopy

import io.equiv.eqfiddle.game.SimpleGame
import io.equiv.eqfiddle.game.EnergyGame
import io.equiv.eqfiddle.hml.ObservationNotionStrong
import io.equiv.eqfiddle.ts.WeakTransitionSystem

class WeakSpectroscopyGame[S, A, L](ts: WeakTransitionSystem[S, A, L], energyCap: Int = Int.MaxValue)
  extends SimpleGame with EnergyGame {

  // obs, branchingConj, unstableConj, stableConj, immediateConj, revivals, positiveHeight, negativeHeight, negations
  protected val NoEnergyUpdate              = new EnergyGame.EnergyUpdate(Array( 0, 0, 0, 0, 0, 0, 0, 0, 0), energyCap = energyCap)
  protected val ObsEnergyUpdate             = new EnergyGame.EnergyUpdate(Array(-1, 0, 0, 0, 0, 0, 0, 0, 0), energyCap = energyCap)
  protected val InstableConjEnergyUpdate    = new EnergyGame.EnergyUpdate(Array( 0, 0,-1, 0, 0, 0, 0, 0, 0), energyCap = energyCap)
  protected val StableConjEnergyUpdate      = new EnergyGame.EnergyUpdate(Array( 0, 0, 0,-1, 0, 0, 6, 0, 0), energyCap = energyCap)
  protected val StableRevivalEnergyUpdate   = new EnergyGame.EnergyUpdate(Array( 6, 0, 0,-1, 0, 0, 0, 0, 0), energyCap = energyCap)
  protected val StabilityCheckEnergyUpdate  = new EnergyGame.EnergyUpdate(Array( 0, 0, 0,-1, 0, 0, 0, 0,-1), energyCap = energyCap)
  protected val ImmediateConjEnergyUpdate   = new EnergyGame.EnergyUpdate(Array( 0, 0, 0, 0,-1, 0, 0, 0, 0), energyCap = energyCap)
  protected val BranchingObsEnergyUpdate    = new EnergyGame.EnergyUpdate(Array( 6,-1,-1, 0, 0, 0, 0, 0, 0), energyCap = energyCap)
  protected val BranchingConjEnergyUpdate   = new EnergyGame.EnergyUpdate(Array( 0,-1,-1, 0, 0, 0, 0, 0, 0), energyCap = energyCap)
  protected val NegClauseEnergyUpdate       = new EnergyGame.EnergyUpdate(Array( 8, 0, 0, 0, 0, 0, 0, 0,-1), energyCap = energyCap)
  protected val PosClauseEnergyUpdate       = new EnergyGame.EnergyUpdate(Array( 6, 0, 0, 0, 0, 0, 0, 0, 0), energyCap = energyCap)
  protected val PosClauseStableEnergyUpdate = new EnergyGame.EnergyUpdate(Array( 7, 0, 0, 0, 0, 0, 0, 0, 0), energyCap = energyCap)

  /* This will abort the game construction in nodes where the attacker cannot win because p is contained in qq. */
  val optimizeSymmetryDefWins: Boolean = true
  val optimizeAttackerWins: Boolean = true

  case class AttackerObservation(p: S, qq: Set[S]) extends SimpleGame.AttackerNode
  case class AttackerDelayedObservation(p: S, qq: Set[S]) extends SimpleGame.AttackerNode
  case class AttackerClause(p: S, q: S) extends SimpleGame.AttackerNode
  case class AttackerClauseStable(p: S, q: S) extends SimpleGame.AttackerNode
  case class AttackerBranchingObservation(p: S, qq: Set[S]) extends SimpleGame.AttackerNode
  case class DefenderConjunction(p: S, qq: Set[S]) extends SimpleGame.DefenderNode
  case class DefenderStableConjunction(p: S, qq: Set[S], qqRevival: Set[S]) extends SimpleGame.DefenderNode
  case class DefenderBranchingConjunction(p1: S, a: A, p2: S, qq: Set[S], qqA: Set[S]) extends SimpleGame.DefenderNode

  override def weight(gn1: GameNode, gn2: GameNode): EnergyGame.EnergyUpdate = gn1 match {
    case AttackerObservation(p0, qq0) =>
      gn2 match {
        case DefenderConjunction(p1, qq1) if qq0.nonEmpty =>
          ImmediateConjEnergyUpdate
        case _ =>
          NoEnergyUpdate
      }
    case AttackerDelayedObservation(p0, qq0) =>
      gn2 match {
        case AttackerObservation(p1, qq1) =>
          ObsEnergyUpdate
        case _ =>
          NoEnergyUpdate
      }
    case AttackerClause(p0, q0) =>
      gn2 match {
        case AttackerDelayedObservation(p1, qq1) if p1 == p0 =>
          PosClauseEnergyUpdate
        case AttackerDelayedObservation(p1, qq1) if qq1 contains p0 =>
          NegClauseEnergyUpdate
      }
    case AttackerClauseStable(p0, q0) =>
      gn2 match {
        case AttackerDelayedObservation(p1, qq1) if p1 == p0 =>
          PosClauseStableEnergyUpdate
        case AttackerDelayedObservation(p1, qq1) if qq1 contains p0 =>
          NegClauseEnergyUpdate
      }
    case AttackerBranchingObservation(p0, qq0) =>
      ObsEnergyUpdate
    case DefenderConjunction(_, _) =>
      InstableConjEnergyUpdate
    case DefenderStableConjunction(_, _, _) =>
      gn2 match {
        case AttackerClauseStable(_, _) =>
          StableConjEnergyUpdate
        case DefenderConjunction(_, _) =>
          StabilityCheckEnergyUpdate
        case _ =>
          StableRevivalEnergyUpdate
      }
    case DefenderBranchingConjunction(p0, a, o01, qq0, qq0a) =>
      gn2 match {
        case AttackerBranchingObservation(p1, qq1) =>
          BranchingObsEnergyUpdate
        case _ =>
          BranchingConjEnergyUpdate
      }
    case _ =>
      NoEnergyUpdate
  }

  def computeSuccessors(gn: GameNode): Iterable[GameNode] = gn match {
    case AttackerObservation(p0, qq0) =>
      if (optimizeSymmetryDefWins && (qq0 contains p0)) {
        List()
      } else {
        val conjMoves = List(DefenderConjunction(p0, qq0))
          val qq1 = for {
            q0 <- qq0
            q1 <- ts.silentReachable(q0)
          } yield q1
          AttackerDelayedObservation(p0, qq1) :: conjMoves
        // }
      }
    case AttackerDelayedObservation(p0, qq0) =>
      if (optimizeSymmetryDefWins && (qq0 contains p0)) {
        List()
      } else {
        val unstableConjMove = DefenderConjunction(p0, qq0)
        if (optimizeAttackerWins && qq0.isEmpty) {
          // prioritize instant wins because of stuck defender
          List(unstableConjMove)
        } else {
          val stableConjMove = if (ts.isStable(p0)) {
            val qq0stable = qq0.filter(ts.isStable(_))
            (
              for {
                qqRevivals <- qq0stable.subsets()
              } yield DefenderStableConjunction(p0, qq0stable -- qqRevivals, qqRevivals)
            ).toList
          } else List()
          val dn = for {
            (a,pp1) <- ts.post(p0)
            p1 <- pp1
            if p0 != p1 || !ts.silentActions(a)
          } yield if (ts.silentActions(a)) {
            // stuttering
            AttackerDelayedObservation(p1, qq0)
          } else {
            // (delayed) observation
            AttackerObservation(p1, qq0.flatMap(ts.post(_, a)))
          }
          val branchingConjs = for {
            (a,pp1) <- ts.post(p0)
            if qq0.size > 1
            p1 <- pp1
            qq0a <- qq0.subsets()
            if qq0a.nonEmpty
          } yield {
            DefenderBranchingConjunction(p0, a, p1, qq0 -- qq0a, qq0a)
          }
          dn ++ List(unstableConjMove) ++ branchingConjs ++ stableConjMove
        }
      }
    case AttackerBranchingObservation(p0, qq0) =>
      List(AttackerObservation(p0, qq0))
    case AttackerClause(p0, q0) =>
      val neg = AttackerDelayedObservation(q0, ts.silentReachable(p0))
      val pos = AttackerDelayedObservation(p0, ts.silentReachable(q0))
      List(pos, neg)
    case AttackerClauseStable(p0, q0) =>
      // identical to AttackerClause
      val neg = AttackerDelayedObservation(q0, ts.silentReachable(p0))
      val pos = AttackerDelayedObservation(p0, ts.silentReachable(q0))
      List(pos, neg)
    case DefenderConjunction(p0, qq0) =>
      for {
        q1 <- qq0
      } yield {
        AttackerClause(p0, q1)
      }
    case DefenderStableConjunction(p0, qq0, qq0revivals) =>
      (for {
        q1 <- qq0
      } yield {
        AttackerClauseStable(p0, q1).asInstanceOf[GameNode]
      }) + DefenderConjunction(p0, Set()) ++ (if (qq0revivals.nonEmpty) List(AttackerObservation(p0, qq0revivals)) else List())
    case DefenderBranchingConjunction(p0, a, p1, qq0, qq0a) =>
      val qq1 = if (ts.silentActions(a)) {
        ts.post(qq0a, a) ++ qq0a
      } else {
        ts.post(qq0a, a)
      }
      (for {
        q0 <- qq0
      } yield {
        AttackerClause(p0, q0).asInstanceOf[GameNode]
      }) + AttackerBranchingObservation(p1, qq1)
  }
}
