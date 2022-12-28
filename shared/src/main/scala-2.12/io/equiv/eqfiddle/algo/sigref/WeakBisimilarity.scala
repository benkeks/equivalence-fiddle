package io.equiv.eqfiddle.algo.sigref

import io.equiv.eqfiddle.ts.TransitionSystem
import io.equiv.eqfiddle.util.Relation
import io.equiv.eqfiddle.util.Coloring
import io.equiv.eqfiddle.algo.AlgorithmLogging
import io.equiv.eqfiddle.ts.WeakTransitionSystem

/**
 * Signature refinement weak bisimilarity algorithm
 * 
 * (following [WHHSB2006])
 * 
 * */

class WeakBisimilarity[S, A, L] (
    override val ts: WeakTransitionSystem[S, A, L])
  extends SignatureRefinement[S, A, L](ts) {
  
  override def signature(s: S): Set[(Coloring.Color, Coloring.Color)] = {
    for {
      s1 <- ts.silentReachable(s)
      (a, s2s) <- ts.post(s1).toSet
      s2 <- s2s
      s3 <- ts.silentReachable(s2)
      if (!ts.silentActions(a) || partition(s) != partition(s3))
    } yield (actionColors(a), partition(s3))
  }
  
}