package io.equiv.eqfiddle.algo.sigref

import io.equiv.eqfiddle.ts.TransitionSystem
import io.equiv.eqfiddle.util.Relation
import io.equiv.eqfiddle.util.Coloring
import io.equiv.eqfiddle.algo.AlgorithmLogging
import io.equiv.eqfiddle.ts.WeakTransitionSystem

/**
 * Signature refinement bisimilarity algorithm
 * 
 * */

class Bisimilarity[S, A, L] (
    override val ts: WeakTransitionSystem[S, A, L],
    onStates: Option[Set[S]] = None
) extends SignatureRefinement[S, A, L](ts, onStates) {
  
  override def signature(s: S): Set[(Coloring.Color, Coloring.Color)] = {
    for {
      (a, s2s) <- ts.post(s).toSet
      s2 <- s2s
    } yield (actionColors(a), partition(s2))
  }
 
}