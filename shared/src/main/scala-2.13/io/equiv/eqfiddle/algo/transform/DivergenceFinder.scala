package io.equiv.eqfiddle.algo.transform

import io.equiv.eqfiddle.ts.TransitionSystem
import io.equiv.eqfiddle.util.FixedPoint
import io.equiv.eqfiddle.ts.WeakTransitionSystem

class DivergenceFinder[S, A, L] (
    ts: WeakTransitionSystem[S, A, L]) {

  // coinductive: "divergent_state p' ⟹ tau t ∧ p ⟼t p'⟹ divergent_state p"
  def compute(): Set[S] = {
    val initialDivergence = ts.nodes

    FixedPoint[Set[S]] (
      { divergences => 
        for {
          s <- divergences
          if ts.silentSteps.values(s).exists( divergences(_) )
        } yield s
      },
      _ == _
    ) (initialDivergence)
    
  }
  
}