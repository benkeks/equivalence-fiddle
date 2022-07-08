package de.bbisping.eqfiddle.spectroscopy

import de.bbisping.eqfiddle.game.SimpleGame
import de.bbisping.eqfiddle.game.WinningRegionComputation
import de.bbisping.eqfiddle.game.GameDiscovery
import de.bbisping.eqfiddle.ts.WeakTransitionSystem
import de.bbisping.eqfiddle.util.Partition

class SpectroscopyGame[S, A, L](ts: WeakTransitionSystem[S, A, L], init: Iterable[(S, Set[S])])
  extends AbstractSpectroscopyGame(ts, init) {

  override def initialNodes: Iterable[GameNode] = {
    init map { case (p0, qq0) => AttackerObservation(p0, qq0, ConjunctMove) }
  }

  def successors(gn: GameNode): Iterable[GameNode] = gn match {
    case AttackerObservation(p0, qq0, moveKind) =>
      if (optimizeSymmetryDefWins && (qq0 contains p0)) {
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
          val neg = AttackerObservation(qq0.head, Set(p0), NegationMove)
          dn ++ List(neg)
        } else if (moveKind.isInstanceOf[ObservationMove]) {
          val conjMoves = for {
            parts <- Partition.partitioningListsOfSet(qq0)
          } yield {
            DefenderConjunction(p0, parts)
          }
          dn ++ conjMoves
        } else {
          dn
        }
      }
    case DefenderConjunction(p0, qqPart0, _) =>
      for {
        qq0 <- qqPart0
      } yield {
        AttackerObservation(p0, qq0, ConjunctMove)
      }
  }
}
