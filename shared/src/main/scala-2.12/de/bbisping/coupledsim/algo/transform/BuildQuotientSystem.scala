package de.bbisping.coupledsim.algo.transform

import de.bbisping.coupledsim.ts.WeakTransitionSystem
import de.bbisping.coupledsim.util.Coloring
import de.bbisping.coupledsim.ts.WeakTransitionSystem
import de.bbisping.coupledsim.util.LabeledRelation

/** takes an equivalence class coloring and builds a quotient automaton system.
 *  
 *  it will take some node from every class as representative (no merging of nodelabels or whatsoever!).
 *  the transitions of classes are merged. (the algorithm makes no assumptions like that nodes within
 *  a class have bisimilar transitions.)
 *  
 *  */
class BuildQuotientSystem[S, A, L] (
    ts: WeakTransitionSystem[S, A, L],
    coloring: Coloring[S]
  ) {
  
  def build() = {
    val partitions = coloring.partitions
    val reps = partitions.mapValues(_.head)
    
    val transitions = for {
      (color, partition) <- partitions.toList
      (a, post) <- ts.post(partition).toList
      tar <- post
    } yield (reps(color), a, reps(coloring(tar)))
    
    val nodeLabeling = for {
      rep <- reps.values
    } yield (rep, ts.nodeLabeling(rep))
    
    new WeakTransitionSystem(
        new LabeledRelation(transitions.toSet),
        nodeLabeling.toMap,
        ts.silentActions)
  }
  
}