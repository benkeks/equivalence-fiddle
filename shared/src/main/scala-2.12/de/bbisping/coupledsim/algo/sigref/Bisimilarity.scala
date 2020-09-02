package de.bbisping.coupledsim.algo.sigref

import de.bbisping.coupledsim.ts.TransitionSystem
import de.bbisping.coupledsim.util.Relation
import de.bbisping.coupledsim.util.Coloring
import de.bbisping.coupledsim.algo.AlgorithmLogging
import de.bbisping.coupledsim.ts.WeakTransitionSystem

/**
 * Signature refinement bisimilarity algorithm
 * 
 * */

class Bisimilarity[S, A, L] (
    override val ts: WeakTransitionSystem[S, A, L])
  extends SignatureRefinement[S, A, L](ts) {
  
  override def signature(s: S): Set[(Coloring.Color, Coloring.Color)] = {
    for {
      (a, s2s) <- ts.post(s).toSet
      s2 <- s2s
    } yield (actionColors(a), partition(s2))
  }
 
}