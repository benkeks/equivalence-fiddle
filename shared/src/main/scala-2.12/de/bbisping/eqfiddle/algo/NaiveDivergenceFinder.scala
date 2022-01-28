package de.bbisping.eqfiddle.algo

import de.bbisping.eqfiddle.ts.TransitionSystem
import de.bbisping.eqfiddle.util.FixedPoint
import de.bbisping.eqfiddle.util.Coloring
import de.bbisping.eqfiddle.ts.WeakTransitionSystem

class NaiveDivergenceFinder[S, A, L] (
    ts: WeakTransitionSystem[S, A, L]) {
  
  /** all tau-states are 0, the others are 1. */
  val labelColors = {
    Coloring(
      ts.nodes map { n =>
//        if (ts.silentNodes contains n) {
//          (n, 0)
//        } else {
//          (n, 1)
//        }
        (n, 1)
      } toMap 
    )
  }
  
  // coinductive: "divergent_state p' ⟹ tau t ∧ p ⟼t p'⟹ divergent_state p"
  
  def compute() = {

    FixedPoint[Coloring[S]] (
      { p => 
        p map { (s, cs) =>
          if (cs == 0 && ts.post(s).exists { case (l, ee2) => ts.silentActions(l) && ee2.exists(p(_) == 0) } ) {
            0
          } else {
            1
          }
        }
      },
      _ == _
    ) (labelColors)
    
  }
  
}