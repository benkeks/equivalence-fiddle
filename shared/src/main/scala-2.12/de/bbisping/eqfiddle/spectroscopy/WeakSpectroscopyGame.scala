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
        val dn = (
          for {
            (a,pp1) <- ts.post(p0)
            p1 <- pp1
          } yield {
            // if no immediacy is required, the defender may move silently before reacting
            AttackerObservation(p1,
              qq0.flatMap(ts.weakPostDelay(_, a)),
              WeakObservationMove(a)
            )
          }) ++ (
          for {
            (a,pp1) <- ts.post(p0)
            if !ts.silentActions(a) // prohibit immediate tau observations
            p1 <- pp1
          } yield {
            AttackerObservation(p1,
              qq0.flatMap(ts.post(_, a)),
              ObservationMove(a)
            )
          }
        )
        if (qq0.size == 1 && (moveKind == ConjunctMove || moveKind == WeakConjunctMove)) {
          val neg = AttackerObservation(qq0.head, Set(p0), NegationMove)
          dn ++ List(neg)
        } else if (moveKind.isInstanceOf[ObservationMove] || moveKind.isInstanceOf[WeakObservationMove] || moveKind == NegationMove) {
          val qq0prime = qq0.flatMap(ts.silentReachable(_))
          dn ++ (
            for {
              partList <- selectPartitions(qq0prime)
            } yield {
              DefenderConjunction(p0, partList, weak = true)
            }
          ) ++ (
            if (moveKind != NegationMove)
              for {
                partList <- selectPartitions(qq0)
              } yield {
                DefenderConjunction(p0, partList)
              }
            else
              List()
          )
        } else {
          dn
        }
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
        //parts <- Partition.partitioningListsOfSet(states)
        mainPart <- states.subsets()
        if mainPart.nonEmpty
      } yield {
        mainPart :: (states -- mainPart).map(Set(_)).toList
      }
    }
  }
}
