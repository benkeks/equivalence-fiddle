package io.equiv.eqfiddle.algo.transform

import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.util.Coloring
import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.util.LabeledRelation

/** takes an equivalence class coloring and builds a quotient automaton system.
 *  
 *  it will take some node from every class as representative (no merging of nodelabels or whatsoever!).
 *  the transitions of classes are merged. (the algorithm makes no assumptions like that nodes within
 *  a class have bisimilar transitions.)
 *  
 *  */
class BuildQuotientSystem[S, A, L] (
    ts: WeakTransitionSystem[S, A, L],
    coloring: Coloring[S],
    protectedNodes: Set[S] = Set[S]()
  ) {
  
  def build() = {
    val partitions = coloring.partitions
    val reps = partitions.mapValues(partition => partition.find(protectedNodes).getOrElse(partition.head))
    
    val transitions = for {
      (color, partition) <- partitions.toList
      (a, post) <- ts.post(partition).toList
      tar <- post
      srcRep = reps(color)
      tarRep = reps(coloring(tar))
    } yield (srcRep, a, tarRep)
    
    val nodeLabeling = for {
      rep <- reps.values
    } yield (rep, ts.nodeLabeling(rep))
    
    new WeakTransitionSystem(
        new LabeledRelation(transitions.toSet),
        nodeLabeling.toMap,
        ts.silentActions)
  }
  
}