package io.equiv.eqfiddle.algo.transform

import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.util.Coloring
import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.util.LabeledRelation

class RemoveLittleBrothers[S, A, L] (
    ts: WeakTransitionSystem[S, A, L],
    isDominated: (S, S) => Boolean,
  ) {

  def build() = {
    val dominatedTransitions = for {
      (s1, aSs2) <- ts.step.rep.toSet
      (a, ss2) <- aSs2
      s2 <- ss2
      if ss2.exists(s2o => (s2o != s2 || s2o == s1) && isDominated(s2, s2o))
    } yield (s1, a, s2)

    val dominatedStates = for {
      s2 <- ts.nodes
      pre = ts.pre(s2)
      if pre.nonEmpty // retain states that started out with no predecessor
      if pre.forall { case (a, ss1) =>
        ss1.forall(s1 => dominatedTransitions.contains(s1, a, s2))
      }
    } yield s2

    val nodeLabeling = ts.nodeLabeling -- dominatedStates

    new WeakTransitionSystem(
        ts.step.filter((s1, a, s2) => !dominatedTransitions.contains((s1, a, s2))),
        nodeLabeling.toMap,
        ts.silentActions)
  }
  
}