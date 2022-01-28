package de.bbisping.eqfiddle.algo.transform

import scala.annotation.migration
import de.bbisping.eqfiddle.ts.TransitionSystem
import de.bbisping.eqfiddle.util.LabeledRelation
import de.bbisping.eqfiddle.ts.WeakTransitionSystem

class InternalizedNondeterminismSaturation[S, A, L] (
    ts: WeakTransitionSystem[S, A, L],
    newState: (S) => S
    ) {

  //private val newStates: collection.mutable.Map[(A,Set[S]), S] = collection.mutable.Map()

  def compute() = {

    val tau = ts.silentActions.head

    val nonDeterminisms = for {
      s1 <- ts.nodes
      (a, ss2) <- ts.post(s1).toSet
      if ss2.size > 1 && !ts.silentActions(a) // if we see non-determinism of external actions...
      s1ss2 = newState(s1) //newStates.getOrElseUpdate((a, ss2), newState(s1))
    } yield (s1, s1ss2, a, ss2)

    val newNondeterministicChoicePostponementNodeLabeling = for {
      (s1, s1ss2, a, ss2) <- nonDeterminisms
    } yield (s1ss2, ts.nodeLabeling(s1))

    val newNondeterministicChoicePostponementPreSteps = for {
      (s1, s1ss2, a, ss2) <- nonDeterminisms
    } yield (s1, a, s1ss2)

    val newNondeterministicChoicePostponementPostSteps = for {
      (s1, s1ss2, a, ss2) <- nonDeterminisms
      s2 <- ss2
    } yield (s1ss2, tau, s2)

    new WeakTransitionSystem(
      new LabeledRelation(
        ts.step.tupleSet ++
          newNondeterministicChoicePostponementPreSteps ++
          newNondeterministicChoicePostponementPostSteps
      ),
      ts.nodeLabeling ++
        newNondeterministicChoicePostponementNodeLabeling,
      ts.silentActions
    )
  }

}