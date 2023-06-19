package io.equiv.eqfiddle.spectroscopy

import io.equiv.eqfiddle.game.SimpleGame
import io.equiv.eqfiddle.game.EnergyGame
import io.equiv.eqfiddle.hml.ObservationClassFast
import io.equiv.eqfiddle.ts.WeakTransitionSystem

class EnergyWeakSpectroscopyGame[S, A, L](ts: WeakTransitionSystem[S, A, L], energyCap: Int = Int.MaxValue)
  extends SimpleGame with EnergyGame {

  // obs, branchingConj, instableConj, stableConj, immediateConj, revivals, positiveHeight, negativeHeight, negations
  private val NoEnergyUpdate             = new EnergyGame.EnergyUpdate(Array( 0, 0, 0, 0, 0, 0, 0, 0, 0), energyCap = energyCap)
  private val ObsEnergyUpdate            = new EnergyGame.EnergyUpdate(Array(-1, 0, 0, 0, 0, 0, 0, 0, 0), energyCap = energyCap)
  private val InstableConjEnergyUpdate   = new EnergyGame.EnergyUpdate(Array( 0, 0,-1, 0, 0, 0, 0, 0, 0), energyCap = energyCap)
  private val StableConjEnergyUpdate     = new EnergyGame.EnergyUpdate(Array( 0, 0, 0,-1, 0, 0, 0, 0, 0), energyCap = energyCap)
  private val ImmediateConjEnergyUpdate  = new EnergyGame.EnergyUpdate(Array( 0, 0,-1, 0,-1, 0, 0, 0, 0), energyCap = energyCap)
  private val BranchingConjEnergyUpdate  = new EnergyGame.EnergyUpdate(Array( 0,-1,-1, 0, 0, 0, 0, 0, 0), energyCap = energyCap)
  private val NegClauseEnergyUpdate      = new EnergyGame.EnergyUpdate(Array( 8, 0, 0, 0, 0, 0, 0, 0,-1), energyCap = energyCap)
  private val PosClauseEnergyUpdate      = new EnergyGame.EnergyUpdate(Array( 7, 0, 0, 0, 0, 0, 0, 0, 0), energyCap = energyCap)
  private val FailureTestEnergyUpdate    = new EnergyGame.EnergyUpdate(Array(-1, 0, 0, 0, 0, 0, 0,-1,-1), energyCap = energyCap)

  /* This will abort the game construction in nodes where the attacker cannot win because p is contained in qq. */
  val optimizeSymmetryDefWins: Boolean = true
  val optimizeAttackerWins: Boolean = true

  case class AttackerObservation(p: S, qq: Set[S]) extends SimpleGame.AttackerNode
  case class AttackerDelayedObservation(p: S, qq: Set[S]) extends SimpleGame.AttackerNode
  case class AttackerClause(p: S, q: S) extends SimpleGame.AttackerNode
  case class AttackerBranchingObservation(p: S, qq: Set[S]) extends SimpleGame.AttackerNode
  //case class AttackerBranchingClause(p: S, a: A, p2: S, q: S) extends SimpleGame.AttackerNode
  case class DefenderConjunction(p: S, qq: Set[S]) extends SimpleGame.DefenderNode
  case class DefenderStableConjunction(p: S, qq: Set[S]) extends SimpleGame.DefenderNode
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
        case AttackerDelayedObservation(p1, qq1) =>
          NoEnergyUpdate
        case DefenderConjunction(p1, qq1) if qq0.nonEmpty =>
          InstableConjEnergyUpdate
        case DefenderStableConjunction(p1, qq1) if qq0.nonEmpty =>
          StableConjEnergyUpdate
        case DefenderBranchingConjunction(p1, a, p2, qq1, qq1a) =>
          BranchingConjEnergyUpdate
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
    case AttackerBranchingObservation(p0, qq0) =>
      ObsEnergyUpdate
    // case AttackerBranchingClause(p0, a, p01, q0) =>
    //   gn2 match {
    //     case AttackerObservation(p1, qq1) =>
    //       ObsEnergyUpdate
    //     case _ =>
    //       NoEnergyUpdate
    //   }
    case DefenderBranchingConjunction(p0, a, o01, qq0, qq0a) =>
      gn2 match {
        case AttackerBranchingObservation(p1, qq1) =>
          PosClauseEnergyUpdate
        case _ =>
          NoEnergyUpdate
      }
    // case DefenderStableConjunction(p0, qq0) =>
    //   gn2 match {
    //     case DefenderConjunction(p1, qq1) =>
    //       FailureTestEnergyUpdate
    //     case _ =>
    //       NoEnergyUpdate
    //   }
    case _ =>
      NoEnergyUpdate
  }

  def computeSuccessors(gn: GameNode): Iterable[GameNode] = gn match {
    case AttackerObservation(p0, qq0) =>
      if (optimizeSymmetryDefWins && (qq0 contains p0)) {
        List()
      } else {
        val conjMoves = List(DefenderConjunction(p0, qq0))
        // if (optimizeAttackerWins && qq0.isEmpty) {
        //   // prioritize instant wins because of stuck defender
        //   List(conjMove)
        // } else {
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
        val instableConjMove = DefenderConjunction(p0, qq0)
        if (optimizeAttackerWins && qq0.isEmpty) {
          // prioritize instant wins because of stuck defender
          List(instableConjMove)
        } else {
          val stableConjMove = if (ts.isStable(p0)) {
            List(DefenderStableConjunction(p0, qq0.filter(ts.isStable(_))))
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
          dn ++ branchingConjs ++ List(instableConjMove) ++ stableConjMove
        }
      }
    case AttackerBranchingObservation(p0, qq0) =>
      List(AttackerObservation(p0, qq0))
    case AttackerClause(p0, q0) =>
      val neg = AttackerDelayedObservation(q0, ts.silentReachable(p0))
      val pos = AttackerDelayedObservation(p0, ts.silentReachable(q0))
      List(pos, neg)
    // case AttackerBranchingClause(p0, a, p1, q0) =>
    //   val qq1 = if (ts.silentActions(a)) {
    //     ts.post(q0, a) + q0
    //   } else {
    //     ts.post(q0, a)
    //   }
    //   if (qq1.nonEmpty) {
    //     List(
    //       AttackerClause(p0, q0),
    //       AttackerObservation(p1, qq1)
    //     )
    //   } else {
    //     List(
    //       AttackerObservation(p1, qq1)
    //     )
    //   }
    case DefenderConjunction(p0, qq0) =>
      for {
        q1 <- qq0
      } yield {
        AttackerClause(p0, q1)
      }
    case DefenderStableConjunction(p0, qq0) =>
      (for {
        q1 <- qq0
      } yield {
        AttackerClause(p0, q1).asInstanceOf[GameNode]
      })// + DefenderConjunction(p0, Set.empty)
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
