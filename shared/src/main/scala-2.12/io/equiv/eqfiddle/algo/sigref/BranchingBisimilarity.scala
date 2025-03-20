package io.equiv.eqfiddle.algo.sigref

import io.equiv.eqfiddle.ts.TransitionSystem
import io.equiv.eqfiddle.util.Relation
import io.equiv.eqfiddle.util.Coloring
import io.equiv.eqfiddle.algo.AlgorithmLogging
import io.equiv.eqfiddle.ts.WeakTransitionSystem

/**
 * Signature refinement branching bisimilarity algorithm
 * 
 * (following [WHHSB2006])
 * 
 * */

class BranchingBisimilarity[S, A, L] (
    override val ts: WeakTransitionSystem[S, A, L],
    onStates: Option[Set[S]] = None,
    stabilityRespecting: Boolean = false
  ) extends SignatureRefinement[S, A, L](ts, onStates) {

  val stabilizationColor = actionColors.freshColor()

  override def signature(s: S): Set[(Coloring.Color, Coloring.Color)] = {
    val stepSigs: Set[(Coloring.Color, Coloring.Color)] = for {
      s1 <- ts.silentReachable(s)
      if partition(s) == partition(s1)
      (a, s2s) <- ts.post(s1).toSet
      s2 <- s2s
      if (!ts.silentActions(a) || partition(s) != partition(s2))
    } yield (actionColors(a), partition(s2))

    if (stabilityRespecting && ts.isStable(s)) {
      stepSigs + ((stabilizationColor, partition(s)))
    } else {
      stepSigs
    /*} else {
      stepSigs ++ (for {
        s1 <- ts.silentReachable(s)
        if partition(s) == partition(s1) && ts.isStable(s1)
      } yield (stabilizationColor, partition(s1))
      )*/
    }
  }
}