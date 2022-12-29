package io.equiv.eqfiddle.spectroscopy

import io.equiv.eqfiddle.game.SimpleGame
import io.equiv.eqfiddle.game.EnergyGame
import io.equiv.eqfiddle.hml.ObservationClassFast
import io.equiv.eqfiddle.ts.WeakTransitionSystem

class EnergySpectroscopyGame[S, A, L](ts: WeakTransitionSystem[S, A, L], energyCap: Int = Int.MaxValue)
  extends SimpleGame with EnergyGame {

  private val NoEnergyUpdate        = new EnergyGame.EnergyUpdate(Array( 0,0,0,0,0,0), energyCap = energyCap)
  private val ObsEnergyUpdate       = new EnergyGame.EnergyUpdate(Array(-1,0,0,0,0,0), energyCap = energyCap)
  private val ConjEnergyUpdate      = new EnergyGame.EnergyUpdate(Array(0,-1,0,0,0,0), energyCap = energyCap)
  private val LocalObsEnergyUpdate  = new EnergyGame.EnergyUpdate(Array(-1,-1,0,0,0,0), energyCap = energyCap)
  private val TestFailureEnergyUpdate = new EnergyGame.EnergyUpdate(Array(0,0,0,0,-1,-1), energyCap = energyCap)
  private val TestReadinessEnergyUpdate = new EnergyGame.EnergyUpdate(Array(0,0,0,-1,0,0), energyCap = energyCap)
  private val RevivalEnergyUpdate   = new EnergyGame.EnergyUpdate(Array(3,0,0,0,0,0), energyCap = energyCap)
  private val NegClauseEnergyUpdate = new EnergyGame.EnergyUpdate(Array(5,0,0,0,0,-1), energyCap = energyCap)
  private val PosClauseEnergyUpdate = new EnergyGame.EnergyUpdate(Array(4,0,0,0,0,0), energyCap = energyCap)

  /* This will abort the game construction in nodes where the attacker cannot win because p is contained in qq. */
  val optimizeSymmetryDefWins: Boolean = true
  val optimizeAttackerWins: Boolean = true

  case class AttackerObservation(p: S, qq: Set[S]) extends SimpleGame.AttackerNode
  case class AttackerClause(p: S, q: S) extends SimpleGame.AttackerNode
  case class AttackerLocalObservation(p: S, qq: Set[S]) extends SimpleGame.AttackerNode
  case class DefenderConjunction(p: S, qq: Set[S]) extends SimpleGame.DefenderNode
  case class DefenderTestFailureObservability(p: S, qq: Set[S]) extends SimpleGame.DefenderNode
  case class DefenderTestReadinessObservability(p: S, qq: Set[S]) extends SimpleGame.DefenderNode

  override def weight(gn1: GameNode, gn2: GameNode): EnergyGame.EnergyUpdate = gn1 match {
    case AttackerObservation(p0, qq0) =>
      gn2 match {
        case AttackerObservation(p1, qq1) =>
          ObsEnergyUpdate
        case DefenderTestFailureObservability(p1, qq1) =>
          LocalObsEnergyUpdate
        case DefenderTestReadinessObservability(p1, qq1) =>
          LocalObsEnergyUpdate
        case DefenderConjunction(p1, qq1) =>
          ConjEnergyUpdate
      }
    case AttackerClause(p0, q0) =>
      gn2 match {
        case AttackerObservation(p1, qq1) if p1 == p0 =>
          PosClauseEnergyUpdate
        case AttackerObservation(p1, qq1) if qq1 contains p0 =>
          NegClauseEnergyUpdate
      }
    case DefenderTestFailureObservability(p0, qq0) =>
      gn2 match {
        case AttackerLocalObservation(p1, qq1) =>
          RevivalEnergyUpdate
        case DefenderConjunction(p1, qq1) =>
          TestFailureEnergyUpdate
      }
    case DefenderTestReadinessObservability(p0, qq0) =>
      gn2 match {
        case AttackerLocalObservation(p1, qq1) =>
          RevivalEnergyUpdate
        case DefenderConjunction(p1, qq1) =>
          TestReadinessEnergyUpdate
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
          val failChallengedQ = qq0.filter(q0 => ts.enabled(q0) subsetOf ts.enabled(p0))
          val failMove = if (failChallengedQ.size < qq0.size) List(DefenderTestFailureObservability(p0, failChallengedQ)) else List()
          val readyChallengedQ = qq0.filter(q0 => ts.enabled(p0) subsetOf ts.enabled(q0))
          val readyMove = if (readyChallengedQ.size < qq0.size) List(DefenderTestReadinessObservability(p0, readyChallengedQ)) else List()
          val dn = for {
            (a,pp1) <- ts.post(p0)
            p1 <- pp1
            qq1 = qq0.flatMap(ts.post(_, a))
          } yield {
            AttackerObservation(p1, qq1)
          }
          dn ++ failMove ++ readyMove ++ List(conjMove)
        }
      }
    case AttackerLocalObservation(p0, qq0) =>
      if (optimizeSymmetryDefWins && (qq0 contains p0)) {
        List()
      } else {
        val conjMove = DefenderConjunction(p0, qq0)
        if (qq0.isEmpty) {
          List(conjMove)
        } else {
          val readyChallengedQ = qq0.filter(q0 => ts.enabled(p0) subsetOf ts.enabled(q0))
          val readyMove = if (readyChallengedQ.size < qq0.size) List(DefenderTestReadinessObservability(p0, readyChallengedQ)) else List()
          val dn = for {
            (a,pp1) <- ts.post(p0)
            p1 <- pp1
            qq1 = qq0.flatMap(ts.post(_, a))
          } yield {
            AttackerObservation(p1, qq1)
          }
          dn ++ readyMove
        }
      }
    case AttackerClause(p0, q0) =>
      val neg = AttackerObservation(q0, Set(p0))
      val pos = AttackerObservation(p0, Set(q0))
      List(pos, neg)
    case DefenderConjunction(p0, qq0) =>
      for {
        q1 <- qq0
      } yield {
        AttackerClause(p0, q1)
      }
    case DefenderTestFailureObservability(p0, qq0) =>
      List(
        DefenderConjunction(p0, Set.empty),
        AttackerLocalObservation(p0, qq0),
      )
    case DefenderTestReadinessObservability(p0, qq0) =>
      List(
        DefenderConjunction(p0, Set.empty),
        AttackerLocalObservation(p0, qq0),
      )
  }
}
