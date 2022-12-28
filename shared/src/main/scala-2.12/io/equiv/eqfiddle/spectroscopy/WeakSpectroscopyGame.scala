package io.equiv.eqfiddle.spectroscopy

import io.equiv.eqfiddle.game.SimpleGame
import io.equiv.eqfiddle.game.WinningRegionComputation
import io.equiv.eqfiddle.game.GameDiscovery
import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.util.Partition

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
            if moveKind != NegationMove // prohibit strong negated observations
            if !ts.silentActions(a) // prohibit explicit tau moves
            p1 <- pp1
          } yield {
            AttackerObservation(p1,
              qq0.flatMap(ts.post(_, a)),
              ObservationMove(a)
            )
          }
        )
        val etaLoop = if (qq0.nonEmpty && moveKind != PassingMove) {
          val qq1 = qq0.flatMap(ts.silentReachable(_))
          for {
            p1 <- ts.silentReachable(p0)
          } yield AttackerObservation(p1, qq1, PassingMove)
        } else {
          List()
        }
        val negMoves = if (qq0.size == 1 && (moveKind == ConjunctMove)) {
          List(AttackerObservation(qq0.head, Set(p0), NegationMove))
        } else {
          List()
        }
        val p0prime = ts.silentReachable(p0)
        val conjMoves = (
          if (moveKind.isInstanceOf[ObservationMove] || moveKind == PassingMove) {
            for {
              partList <- selectPartitions(qq0)
            } yield {
              DefenderConjunction(p0, partList)
            }
          } else {
            List()
          }
        )
        obsMoves ++ etaLoop ++ negMoves ++ conjMoves
      }
    case DefenderConjunction(p0, qqPart0) =>
      for {
        qq0 <- qqPart0
      } yield {
        AttackerObservation(p0, qq0, ConjunctMove)
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
