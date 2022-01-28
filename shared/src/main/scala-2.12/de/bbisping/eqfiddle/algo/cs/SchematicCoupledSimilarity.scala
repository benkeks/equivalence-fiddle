package de.bbisping.eqfiddle.algo.cs

import de.bbisping.eqfiddle.util.Relation
import de.bbisping.eqfiddle.ts.WeakTransitionSystem

/**
 * Simplest coupled similarity algorithm based on schematic similarity from [RT2010]
 * */

class SchematicCoupledSimilarity[S, A, L] (
    val ts: WeakTransitionSystem[S, A, L]) {
  
  def compute() = {
    val initialCandidates = for {
      s1 <- ts.nodes
      s2 <- ts.nodes
      if ts.weakEnabled(s1) subsetOf ts.weakEnabled(s2)
    } yield (s1, s2)
    
    var sim = new Relation[S](initialCandidates)
    
    while ({
      val delCand = sim.rep collect {
        case (u, ww) => ww collect {
          case w if
            // enforce simulation
            (ts.post(u).exists {
            case (a, vv) => vv.exists { v =>
              val vsim = sim.values(v)
              ! ( ts.weakPost(w, a) exists (vsim contains _) )
            }
          }) || 
            // enforce coupling
            !ts.silentReachable(w).exists { w1 => sim(w1, u) }
          => (u, w)
        }
      } flatten
      
      if (delCand.isEmpty) {
        false
      } else {
        val dcs = delCand.toSet
        sim = sim.filter((s1, s2) => !dcs.contains(s1, s2))
        true
      }
    }) {}
    
    sim
  }
}