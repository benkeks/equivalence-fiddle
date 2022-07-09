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
        val obsMoves = (
          for {
            (a,pp1) <- ts.post(p0)
            if moveKind == WeakConjunctMove || !(ts.silentActions(a)) // prohibit strong tau observations
            if moveKind != NegationMove // prohibit strong negated observations
            p1 <- pp1
          } yield {
            AttackerObservation(p1,
              qq0.flatMap(ts.post(_, a)),
              ObservationMove(a)
            )
          }
        )
        val negMoves = if (qq0.size == 1 && (moveKind == ConjunctMove || moveKind == WeakConjunctMove)) {
          List(AttackerObservation(qq0.head, Set(p0), NegationMove))
        } else {
          List()
        }
        val p0prime = ts.silentReachable(p0)
        val qq0prime = qq0.flatMap(ts.silentReachable(_))
        val conjMoves = (
          for {
            partList <- selectPartitions(qq0prime).toList
            p00 <- p0prime
          } yield {
            DefenderConjunction(p00, partList, weak = true)
          }
        ) ++ (
          if (moveKind.isInstanceOf[ObservationMove]) {
            for {
              partList <- selectPartitions(qq0)
            } yield {
              DefenderConjunction(p0, partList)
            }
          } else {
            List()
          }
        )
        obsMoves ++ negMoves ++ conjMoves
      }
    case DefenderConjunction(p0, qqPart0, weak) =>
      for {
        qq0 <- qqPart0
        post = if (weak) WeakConjunctMove else ConjunctMove
      } yield {
        AttackerObservation(p0, qq0, post)
      }
  }

  private def selectPartitions(states: Set[S]): TraversableOnce[List[Set[S]]] = {
    if (states.isEmpty) {
      List(List())
    } else {
      // List(states.map(Set(_)).toList) // hack to only explore finest partition
      for {
        parts <- Partition.partitioningListsOfSet(states)
        // mainPart <- states.subsets()
        // if mainPart.nonEmpty
      } yield {
        parts
        //mainPart :: (states -- mainPart).map(Set(_)).toList
      }
    }
  }
}
