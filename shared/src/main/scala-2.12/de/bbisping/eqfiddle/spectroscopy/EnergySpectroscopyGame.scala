package de.bbisping.eqfiddle.spectroscopy

import de.bbisping.eqfiddle.game.SimpleGame
import de.bbisping.eqfiddle.game.EnergyGame
import de.bbisping.eqfiddle.hml.ObservationClassFast
import de.bbisping.eqfiddle.ts.WeakTransitionSystem

class EnergySpectroscopyGame[S, A, L](ts: WeakTransitionSystem[S, A, L])
  extends SimpleGame with EnergyGame {

  private val NoEnergyUpdate = EnergyGame.EnergyUpdate(List(0,0,0,0))
  private val ObsEnergyUpdate = EnergyGame.EnergyUpdate(List(-1,0,0,0))
  private val ConjEnergyUpdate = EnergyGame.EnergyUpdate(List(0,-1,0,0))
  private val PosClauseEnergyUpdate = EnergyGame.EnergyUpdate(List(0,0,1,0))
  private val NegClauseEnergyUpdate = EnergyGame.EnergyUpdate(List(0,0,0,1))
  
  override def weight(gn1: GameNode, gn2: GameNode): EnergyGame.EnergyUpdate = gn1 match {
    case AttackerObservation(p0, qq0) =>
      gn2 match {
          case AttackerObservation(p1, qq1) =>
            ObsEnergyUpdate
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
    case _ =>
      NoEnergyUpdate
  }

  /* This will abort the game construction in nodes where the attacker cannot win because p is contained in qq. */
  val optimizeSymmetryDefWins: Boolean = false

  case class AttackerObservation(p: S, qq: Set[S]) extends SimpleGame.AttackerNode
  case class AttackerClause(p: S, q: S) extends SimpleGame.AttackerNode
  case class DefenderConjunction(p: S, qq: Set[S]) extends SimpleGame.DefenderNode

  def computeSuccessors(gn: GameNode): Iterable[GameNode] = gn match {
    case AttackerObservation(p0, qq0) =>
      if (optimizeSymmetryDefWins && (qq0 contains p0)) {
        List()
      } else {
        val conjMoves = List(DefenderConjunction(p0, qq0))
        val dn = for {
          (a,pp1) <- ts.post(p0)
          p1 <- pp1
        } yield {
          AttackerObservation(p1,
            qq0.flatMap(ts.post(_, a))
          )
        }
        dn ++ conjMoves
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
  }
}
