package io.equiv.eqfiddle.spectroscopy

import io.equiv.eqfiddle.game.SimpleGame
import io.equiv.eqfiddle.game.EnergyGame
import io.equiv.eqfiddle.hml.ObservationClassFast
import io.equiv.eqfiddle.ts.WeakTransitionSystem

class EnergyWeakSpectroscopyGame[S, A, L](ts: WeakTransitionSystem[S, A, L], energyCap: Int = Int.MaxValue)
  extends SimpleGame with EnergyGame {

  private val NoEnergyUpdate            = new EnergyGame.EnergyUpdate(Array( 0, 0, 0, 0, 0, 0, 0, 0), energyCap = energyCap)
  private val ObsEnergyUpdate           = new EnergyGame.EnergyUpdate(Array(-1, 0, 0, 0, 0, 0, 0, 0), energyCap = energyCap)
  private val BranchingObsEnergyUpdate  = new EnergyGame.EnergyUpdate(Array(-1,-1, 0, 0, 0, 0, 0, 0), energyCap = energyCap)
  private val ConjEnergyUpdate          = new EnergyGame.EnergyUpdate(Array( 0, 0,-1, 0, 0, 0, 0, 0), energyCap = energyCap)
  private val ImmediateConjEnergyUpdate = new EnergyGame.EnergyUpdate(Array( 0, 0,-1,-1, 0, 0, 0, 0), energyCap = energyCap)
  private val BranchingConjEnergyUpdate = new EnergyGame.EnergyUpdate(Array( 6, 0,-1, 0, 0, 0, 0, 0), energyCap = energyCap)
  private val NegClauseEnergyUpdate     = new EnergyGame.EnergyUpdate(Array( 7, 0, 0, 0, 0, 0, 0,-1), energyCap = energyCap)
  private val PosClauseEnergyUpdate     = new EnergyGame.EnergyUpdate(Array( 6, 0, 0, 0, 0, 0, 0, 0), energyCap = energyCap)

  /* This will abort the game construction in nodes where the attacker cannot win because p is contained in qq. */
  val optimizeSymmetryDefWins: Boolean = true
  val optimizeAttackerWins: Boolean = true

  case class AttackerObservation(p: S, qq: Set[S]) extends SimpleGame.AttackerNode
  case class AttackerDelayedObservation(p: S, qq: Set[S]) extends SimpleGame.AttackerNode
  case class AttackerClause(p: S, q: S) extends SimpleGame.AttackerNode
  case class AttackerBranchingClause(p: S, a: A, p2: S, q: S) extends SimpleGame.AttackerNode
  case class DefenderConjunction(p: S, qq: Set[S]) extends SimpleGame.DefenderNode
  case class DefenderBranchingConjunction(p1: S, a: A, p2: S, qq: Set[S]) extends SimpleGame.DefenderNode

  override def weight(gn1: GameNode, gn2: GameNode): EnergyGame.EnergyUpdate = gn1 match {
    case AttackerObservation(p0, qq0) =>
      gn2 match {
        case DefenderConjunction(p1, qq1) =>
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
        case DefenderConjunction(p1, qq1) =>
          ConjEnergyUpdate
        case DefenderBranchingConjunction(p1, a, p2, qq1) =>
          BranchingConjEnergyUpdate
      }
    case AttackerClause(p0, q0) =>
      gn2 match {
        case AttackerObservation(p1, qq1) if p1 == p0 =>
          PosClauseEnergyUpdate
        case AttackerObservation(p1, qq1) if qq1 contains p0 =>
          NegClauseEnergyUpdate
      }
    case DefenderBranchingConjunction(p0, a, p1, qq0) =>
      gn2 match {
        case DefenderConjunction(p1, qq1) =>
          BranchingObsEnergyUpdate
        case _ =>
          NoEnergyUpdate
      }
    case _ =>
      NoEnergyUpdate
  }

  def computeSuccessors(gn: GameNode): Iterable[GameNode] = gn match {
    case AttackerObservation(p0, qq0) =>
      if (optimizeSymmetryDefWins && (qq0 contains p0)) {
        List()
      } else {
        val conjMove = DefenderConjunction(p0, qq0)
        if (optimizeAttackerWins && qq0.isEmpty) {
          // prioritize instant wins because of stuck defender
          List(conjMove)
        } else {
          val qq1 = for {
            q0 <- qq0
            q1 <- ts.silentReachable(q0)
          } yield q1
          List(conjMove, AttackerDelayedObservation(p0, qq1))
        }
      }
    case AttackerDelayedObservation(p0, qq0) =>
      if (optimizeSymmetryDefWins && (qq0 contains p0)) {
        List()
      } else {
        val conjMove = DefenderConjunction(p0, qq0)
        if (optimizeAttackerWins && qq0.isEmpty) {
          // prioritize instant wins because of stuck defender
          List(conjMove)
        } else {
          val dn = for {
            (a,pp1) <- ts.post(p0)
            p1 <- pp1
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
            if !ts.silentActions(a)
          } yield {
            DefenderBranchingConjunction(p0, a, p1, qq0)
          }
          dn ++ branchingConjs ++ List(conjMove)
        }
      }
    case AttackerClause(p0, q0) =>
      val neg = AttackerObservation(q0, Set(p0))
      val pos = AttackerObservation(p0, Set(q0))
      List(pos, neg)
    case AttackerBranchingClause(p0, a, p1, q0) =>
      val qq1 = ts.post(q0, a)
      if (qq1.nonEmpty) {
        List(
          AttackerClause(p0, q0),
          AttackerObservation(p1, qq1)
        )
      } else {
        List(
          AttackerObservation(p1, qq1)
        )
      }
    case DefenderConjunction(p0, qq0) =>
      for {
        q1 <- qq0
      } yield {
        AttackerClause(p0, q1)
      }
    case DefenderBranchingConjunction(p0, a, p1, qq0) =>
      (for {
        q0 <- qq0
      } yield {
        AttackerBranchingClause(p0, a, p1, q0).asInstanceOf[GameNode]
      }) + DefenderConjunction(p0, Set.empty)
  }
}
