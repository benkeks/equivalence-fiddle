package de.bbisping.eqfiddle.algo.sigref

import de.bbisping.eqfiddle.ts.TransitionSystem
import de.bbisping.eqfiddle.util.Relation
import de.bbisping.eqfiddle.util.Coloring
import de.bbisping.eqfiddle.ts.WeakTransitionSystem

/**
 * weak bisimilarity only regarding the maximal steps (where the target states cannot perform further tau-transitions.)
 * */

class BigStepEquivalence[S, A, L] (
    override val ts: WeakTransitionSystem[S, A, L])
  extends SignatureRefinement[S, A, L](ts) {
    
  val tau = ts.silentActions.headOption.toSet
  
  val tauMaximalNode = {
    for {
      p <- ts.nodes
      if ts.silentReachable(p) subsetOf Set(p)
    } yield p
  }
  
  override def signature(s: S): Set[(Coloring.Color, Coloring.Color)] = {
    
    val visibleSteps = for {
      s1 <- ts.silentReachable(s)
      (a, s2s) <- ts.post(s1).toSet
      if (!ts.silentActions(a))
      s2 <- s2s
      s3 <- ts.silentReachable(s2)
      if ( (tauMaximalNode contains s3))
    } yield (actionColors(a), partition(s3))
    
    val internalStep = for {
      t <- tau
      s1 <- ts.silentReachable(s)
      if ((partition(s) != partition(s1)) && (tauMaximalNode contains s1))
    } yield (actionColors(t), partition(s1))
    
    visibleSteps ++ internalStep
  }
  
}