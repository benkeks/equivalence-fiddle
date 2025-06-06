package io.equiv.eqfiddle.spectroscopy

import io.equiv.eqfiddle.game.SimpleGame
import io.equiv.eqfiddle.game.EnergyGame
import io.equiv.eqfiddle.hml.StrongObservationNotion
import io.equiv.eqfiddle.ts.WeakTransitionSystem

object StrongSpectroscopyGame {
  trait StrongSpectroscopyGamePosition[S] extends SimpleGame.GamePosition
  case class AttackerObservation[S](p: S, qq: Set[S])
    extends SimpleGame.AttackerPosition with StrongSpectroscopyGamePosition[S]
  case class AttackerConjunct[S](p: S, q: S)
    extends SimpleGame.AttackerPosition with StrongSpectroscopyGamePosition[S]
  case class DefenderConjunction[S](p: S, qqSingles: Set[S], qqRevival: Set[S])
    extends SimpleGame.DefenderPosition with StrongSpectroscopyGamePosition[S]
}

class StrongSpectroscopyGame[S, A, L](
    ts: WeakTransitionSystem[S, A, L],
    config: Spectroscopy.Config = Spectroscopy.Config() )
  extends SimpleGame[StrongSpectroscopyGame.StrongSpectroscopyGamePosition[S]]
  with EnergyGame[StrongSpectroscopyGame.StrongSpectroscopyGamePosition[S]] {

  import StrongSpectroscopyGame._
  type GamePosition = StrongSpectroscopyGamePosition[S]

  override def dimensionality: Int = 6

  private val NoEnergyUpdate        = new EnergyGame.EnergyUpdate(Array( 0,0,0,0,0,0), energyCap = config.energyCap)
  private val ObsEnergyUpdate       = new EnergyGame.EnergyUpdate(Array(-1,0,0,0,0,0), energyCap = config.energyCap)
  private val RevivalEnergyUpdate   = new EnergyGame.EnergyUpdate(Array(3,-1,0,0,0,0), energyCap = config.energyCap)
  private val NoRevivalEnergyUpdate = new EnergyGame.EnergyUpdate(Array(0,-1,0,3,0,0), energyCap = config.energyCap)
  private val NegClauseEnergyUpdate = new EnergyGame.EnergyUpdate(Array(5,0,0,0,0,-1), energyCap = config.energyCap)
  private val PosClauseEnergyUpdate = new EnergyGame.EnergyUpdate(Array(4,0,0,0,0,0), energyCap = config.energyCap)

  val optimizeAttackerWins: Boolean = true

  override def weight(gn1: GamePosition, gn2: GamePosition): EnergyGame.EnergyUpdate = gn1 match {
    case AttackerObservation(p0, qq0) =>
      gn2 match {
        case AttackerObservation(p1, qq1) =>
          ObsEnergyUpdate
        case _ => 
          NoEnergyUpdate
      }
    case AttackerConjunct(p0, q0) =>
      gn2 match {
        case AttackerObservation(p1, qq1) if p1 == p0 =>
          PosClauseEnergyUpdate
        case AttackerObservation(p1, qq1) if qq1 contains p0 =>
          NegClauseEnergyUpdate
      }
    case DefenderConjunction(p0, qqS, qqR) =>
      gn2 match {
        case AttackerObservation(p1, qq1) =>
          RevivalEnergyUpdate
        case _ =>
          NoRevivalEnergyUpdate
      }
    case _ =>
      NoEnergyUpdate
  }

  def computeSuccessors(gn: GamePosition): Iterable[GamePosition] = gn match {
    case AttackerObservation(p0, qq0) =>
      if (config.useSymmetryPruning && (qq0 contains p0)) {
        List()
      } else {
        if (optimizeAttackerWins && qq0.isEmpty) {
          // prioritize instant wins because of stuck defender
          List(DefenderConjunction(p0, qq0, Set.empty))
        } else {
          val conjMoves = if (config.useCleverSpectroscopyGame) {
            val failChallengedQ = qq0.filter(q0 => ts.enabled(q0) subsetOf ts.enabled(p0))
            val offerChallengedQ = qq0.filter(q0 => ts.enabled(p0) subsetOf ts.enabled(q0))
            val readyChallengedQ = failChallengedQ intersect offerChallengedQ
            Set(
              DefenderConjunction(p0, qq0, Set.empty)
            ) ++ {
              if (failChallengedQ.size < qq0.size) Set(DefenderConjunction(p0, qq0 -- failChallengedQ, failChallengedQ)) else Set()
            } ++ {
              if (offerChallengedQ.size < qq0.size) Set(DefenderConjunction(p0, qq0 -- offerChallengedQ, offerChallengedQ)) else Set()
            } ++ {
              if (readyChallengedQ.size < qq0.size) Set(DefenderConjunction(p0, qq0 -- readyChallengedQ, readyChallengedQ)) else Set()
            }
          } else {
            for {
              qqS <- qq0.subsets()
            } yield DefenderConjunction(p0, qqS, qq0 -- qqS)
          }
          val obsMoves = for {
            (a,pp1) <- ts.post(p0)
            p1 <- pp1
            qq1 = qq0.flatMap(ts.post(_, a))
          } yield {
            AttackerObservation(p1, qq1)
          }
          obsMoves ++ conjMoves
        }
      }
    case AttackerConjunct(p0, q0) =>
      val neg = AttackerObservation(q0, Set(p0))
      val pos = AttackerObservation(p0, Set(q0))
      List(pos, neg)
    case DefenderConjunction(p0, qqS, qqR) =>
      val revival = if (qqR.isEmpty) List() else List(AttackerObservation(p0, qqR))
      (
        for {
          q1 <- qqS
        } yield {
          AttackerConjunct(p0, q1)
        }
       ) ++ revival
  }
}
