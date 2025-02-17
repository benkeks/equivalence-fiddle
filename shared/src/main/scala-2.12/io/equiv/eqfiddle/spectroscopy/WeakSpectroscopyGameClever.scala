package io.equiv.eqfiddle.spectroscopy

import io.equiv.eqfiddle.game.SimpleGame
import io.equiv.eqfiddle.game.EnergyGame
import io.equiv.eqfiddle.hml.ObservationNotionStrong
import io.equiv.eqfiddle.ts.WeakTransitionSystem

class WeakSpectroscopyGameClever[S, A, L](ts: WeakTransitionSystem[S, A, L], energyCap: Int = Int.MaxValue)
  extends WeakSpectroscopyGame(ts, energyCap) {

  // obs, branchingConj, unstableConj, stableConj, immediateConj, revivals, positiveHeight, negativeHeight, negations
  private val BranchingObsSubEnergyUpdate     = new EnergyGame.EnergyUpdate(Array(  6, 0, 0, 0, 0, 0, 0, 0, 0), energyCap = energyCap)
  private val BranchingConjEarlyEnergyUpdate  = new EnergyGame.EnergyUpdate(Array( -1, 0, 0, 0,-1, 0, 0, 0, 0), energyCap = energyCap)
  private val BranchingConjLateEnergyUpdate   = new EnergyGame.EnergyUpdate(Array( -1, 0,-1, 0, 0, 0, 0, 0, 0), energyCap = energyCap)

  case class AttackerBranchingConjunction(p0: S, a: A, p1: S, q0: S) extends SimpleGame.AttackerPosition

  override def weight(gn1: GamePosition, gn2: GamePosition): EnergyGame.EnergyUpdate = gn1 match {
    case DefenderBranchingConjunction(p10, a, p11, qq0, qq0a) =>
      BranchingConjEnergyUpdate
    case AttackerBranchingConjunction(_, _, _, _) =>
      gn2 match {
        case AttackerBranchingObservation(_, _) =>
          BranchingObsSubEnergyUpdate
        case _ =>
          NoEnergyUpdate
      }
    case AttackerBranchingObservation(_, qq0) =>
      gn2 match {
        case DefenderConjunction(_, _) =>
          if (qq0.isEmpty) {
            ObsEnergyUpdate
          } else {
            BranchingConjEarlyEnergyUpdate
          }
        case _ =>
          BranchingConjLateEnergyUpdate
      }
    case _ =>
      super.weight(gn1, gn2)
  }

  override def computeSuccessors(gn: GamePosition): Iterable[GamePosition] = gn match {

    case AttackerDelayedObservation(p0, qq0) =>
      // mostly identical to WeakSpectroscopyGame
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
          // this is where we deviate from WeakSpectroscopyGame in not building subsets of qq0
          val branchingConjs = for {
            (a,pp1) <- ts.post(p0)
            if qq0.size > 1
            p1 <- pp1
          } yield {
            DefenderBranchingConjunction(p0, a, p1, qq0, Set())
          }
          dn ++ List(unstableConjMove) ++ branchingConjs ++ stableConjMove
        }
      }
    case DefenderBranchingConjunction(p0, a, p1, qq0, qq0a) =>
      // note: in the clever game, qq0a is always empty
      for {
        q0 <- qq0
      } yield {
        AttackerBranchingConjunction(p0, a, p1, q0)
      }
    case AttackerBranchingConjunction(p0, a, p1, q0) =>
      val qq1 = if (ts.silentActions(a)) {
        ts.post(q0, a) + q0
      } else {
        ts.post(q0, a)
      }
      List(AttackerBranchingObservation(p1, qq1), AttackerClause(p0, q0))
    case AttackerBranchingObservation(p0, qq0) =>
      List(DefenderConjunction(p0, qq0), AttackerObservation(p0, qq0))
    case _ =>
      super.computeSuccessors(gn)
  }
}
