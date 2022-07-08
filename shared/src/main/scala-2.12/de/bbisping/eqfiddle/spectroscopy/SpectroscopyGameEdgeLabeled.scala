package de.bbisping.eqfiddle.spectroscopy

import de.bbisping.eqfiddle.game.SimpleGame
import de.bbisping.eqfiddle.game.WinningRegionComputation
import de.bbisping.eqfiddle.game.GameDiscovery
import de.bbisping.eqfiddle.ts.WeakTransitionSystem
import de.bbisping.eqfiddle.util.Partition

class SpectroscopyGameEdgeLabeled[S, A, L](ts: WeakTransitionSystem[S, A, L], init: Iterable[(S, Set[S])])
  extends AbstractSpectroscopyGame(ts, init) {

  lazy val recordedMoveEdges: collection.mutable.Map[(GameNode, GameNode), MoveKind] = collection.mutable.Map()

  override def initialNodes: Iterable[GameNode] = {
    init map { case (p0, qq0) => AttackerObservation(p0, qq0, ConjunctMove) }
  }

  def successors(gn: GameNode): Iterable[GameNode] = gn match {
    case AttackerObservation(p0, qq0, moveLabel) =>
      if (optimizeSymmetryDefWins && (qq0 contains p0)) {
        List()
      } else {
        val dn = for {
          (a,pp1) <- ts.post(p0)
          p1 <- pp1
          next = AttackerObservation(p1,
            qq0.flatMap(ts.post(_, a)),
            DefenderMove
          )
        } yield {
          recordedMoveEdges((gn, next)) = ObservationMove(a)
          next
        }
        if (qq0.size == 1) {
          // wlog only have negation moves when the defender is focused (which can be forced by the attacker using preceding conjunctions)
          val neg = AttackerObservation(qq0.head, Set(p0), DefenderMove)
          recordedMoveEdges((gn, neg)) = NegationMove
          dn ++ List(neg)
        } else if (moveLabel == ConjunctMove) {
          dn
        } else {
          // conjunct moves only make sense if the defender is spread
          val conjMoves = for {
            parts <- Partition.partitioningListsOfSet(qq0)
            //if parts.length == qq0.size // this is equivalent to the original algorithm's game
            if parts.length != 1 // drop the trivial partitioning
            conj = DefenderConjunction(p0, parts)
          } yield {
            recordedMoveEdges((gn, conj)) = ConjunctMove
            conj
          }
          dn ++ conjMoves
        }
      }
    case DefenderConjunction(p0, qqPart0, _) =>
      for {
        qq0 <- qqPart0
        // after-conj nodes with singleton qq0 are conflated with usual attacker nodes.
        obs = AttackerObservation(p0, qq0, if (qq0.size == 1) DefenderMove else ConjunctMove)
      } yield {
        recordedMoveEdges((gn, obs)) = DefenderMove
        obs
      }
  }
}
