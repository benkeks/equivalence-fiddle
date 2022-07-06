package de.bbisping.eqfiddle.spectroscopy

import de.bbisping.eqfiddle.game.SimpleGame
import de.bbisping.eqfiddle.game.WinningRegionComputation
import de.bbisping.eqfiddle.game.GameDiscovery
import de.bbisping.eqfiddle.ts.WeakTransitionSystem
import de.bbisping.eqfiddle.util.Partition

class WeakSpectroscopyGame[S, A, L](ts: WeakTransitionSystem[S, A, L], init: Iterable[(S, Set[S])])
  extends SpectroscopyGame(ts, init) {

  override def successors(gn: GameNode): Iterable[GameNode] = gn match {
    case AttackerObservation(p0, qq0, moveKind) =>
      if (optimizeSymmetryDefWins && (qq0 contains p0)) {
        List()
      } else {
        val dn = for {
          (a,pp1) <- ts.post(p0)
          p1 <- pp1
          if !(moveKind == ImmediacyMove && ts.silentActions(a)) // prohibit immediate tau moves
        } yield {
          AttackerObservation(p1,
            if (moveKind == ImmediacyMove)
              qq0.flatMap(ts.post(_, a))
            else
              // if no immediacy is required, the defender may move silently before reacting
              qq0.flatMap(ts.weakPostDelay(_, a)),
            ObservationMove(a)
          )
        }
        val in = if (moveKind == ConjunctMove) {
          List(AttackerObservation(p0, qq0, ImmediacyMove))
         } else {
          List()
        }
        if (qq0.size == 1 && moveKind == ConjunctMove) {
          val neg = AttackerObservation(qq0.head, Set(p0), NegationMove)
          dn ++ in ++ List(neg)
        } else if (moveKind.isInstanceOf[ObservationMove]) { // weak conjunction
          val qq0prime = qq0.flatMap(ts.silentReachable(_))
          val conjMoves = for {
            parts <- Partition.partitioningListsOfSet(qq0prime)
          } yield {
            DefenderConjunction(p0, parts)
          }
          dn ++ conjMoves ++ List(AttackerObservation(p0, qq0, ImmediacyMove))
        } else if (moveKind == ImmediacyMove) { // strong conjunction
          val conjMoves = for {
            parts <- Partition.partitioningListsOfSet(qq0)
          } yield {
            DefenderConjunction(p0, parts)
          }
          dn ++ conjMoves
        } else {
          dn ++ in
        }
      }
    case DefenderConjunction(p0, qqPart0) =>
      for {
        qq0 <- qqPart0
      } yield {
        AttackerObservation(p0, qq0, ConjunctMove)
      }
  }
}
