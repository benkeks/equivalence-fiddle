package io.equiv.eqfiddle.algo.cs

import io.equiv.eqfiddle.util.Relation
import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.util.FixedPoint

/**
 * Straight forward implementation of coupled similarity derived from CS's gfp characterization.
 */

class FixedPointCoupledSimilarity[S, A, L] (
    val ts: WeakTransitionSystem[S, A, L]) {
  
  def compute() = {
    val initialCandidates = for {
      s1 <- ts.nodes
      s2 <- ts.nodes
      if ts.weakEnabled(s1) subsetOf ts.weakEnabled(s2)
    } yield (s1, s2)
    
    def csFun(sim: Relation[S]) = {
      sim.filter {
        (p, q) =>
          // enforce simulation
          (ts.post(p) forall {
            case (a, pp2) => pp2 forall { p2 =>
              val p2sim = sim.values(p2)
              ts.weakPost(q, a) exists p2sim
            }
          }) &&
          // enforce coupling
          (ts.silentReachable(q) exists { q1 => sim(q1, p) })
      }
    }
    
    FixedPoint[Relation[S]](csFun, _ == _) apply (new Relation[S](initialCandidates))
  }
}