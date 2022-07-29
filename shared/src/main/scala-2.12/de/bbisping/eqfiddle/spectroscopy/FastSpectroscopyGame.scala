package de.bbisping.eqfiddle.spectroscopy

import de.bbisping.eqfiddle.game.SimpleGame
import de.bbisping.eqfiddle.game.GameLazyDecision
import de.bbisping.eqfiddle.hml.ObservationClassFast
import de.bbisping.eqfiddle.ts.WeakTransitionSystem
import de.bbisping.eqfiddle.util.Partition

class FastSpectroscopyGame[S, A, L](ts: WeakTransitionSystem[S, A, L])
  extends SimpleGame with GameLazyDecision[ObservationClassFast] {

  /* price p1 is strictly better than p2 for an attacker win */
  override def priceIsBetter(p1: ObservationClassFast, p2: ObservationClassFast): Boolean = p1 strictlyBelow p2

  override def computeCurrentPrice(node: GameNode): Iterable[ObservationClassFast] = node match {
    case AttackerObservation(p0, qq0, postConj0) =>
      val succPrices = 
        for {
          s <- successors(node)
          sP <- attackerVictoryPrices(s)
        } yield s match {
          case AttackerObservation(p1, qq1, postConj1) if !postConj0 =>
            List(ObservationClassFast(observationHeight = sP.observationHeight + 1) lub sP)
          case AttackerObservation(p1, qq1, postConj1) if postConj0 =>
            {
              if (Set(p1) == qq0 && Set(p0) == qq1) { // side swap
                List(ObservationClassFast(maxNegativeConjunctHeight = sP.observationHeight) lub sP)
              } else {
                List()
              }
            } ++ {
              if (p0 == p1 && qq0 == qq1) { // no side swap (= positive branch)
                List(ObservationClassFast(maxPositiveConjunctHeight = sP.observationHeight) lub sP)
              } else {
                List()
              }
            }
          case DefenderConjunction(p1, qq1) =>
            List(ObservationClassFast(conjunctionLevels = sP.conjunctionLevels + 1) lub sP)
        }
      succPrices.flatten
    case defNode: DefenderConjunction =>
      val possibleMoves = for {
        s <- successors(node)
      } yield attackerVictoryPrices(s)
      val productMoves =
        possibleMoves.reduceLeft(
          (b, a) => b.flatMap(i => a.map(j => i lub j)))
      println(defNode + "(" + attackerVictoryPrices(defNode) + ") -> " + possibleMoves + " => " + productMoves)
      productMoves.toSet
  }

  /* This will abort the game construction in nodes where the attacker cannot win because p is contained in qq. */
  val optimizeSymmetryDefWins: Boolean = false

  case class AttackerObservation(p: S, qq: Set[S], postConj: Boolean = false) extends SimpleGame.AttackerNode
  case class DefenderConjunction(p: S, qq: Set[S]) extends SimpleGame.DefenderNode

  def computeSuccessors(gn: GameNode): Iterable[GameNode] = gn match {
    case AttackerObservation(p0, qq0, postConj) =>
      if (optimizeSymmetryDefWins && (qq0 contains p0)) {
        List()
      } else {
        if (postConj) {
          val neg = AttackerObservation(qq0.head, Set(p0))
          val pos = AttackerObservation(p0, qq0)
          List(pos, neg)
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
      }
    case DefenderConjunction(p0, qq0) =>
      for {
        q1 <- qq0
      } yield {
        AttackerObservation(p0, Set(q1), postConj = true)
      }
  }
}
