package io.equiv.eqfiddle.algo

import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.util.LabeledRelation

class WeakTransitionSaturation[S, A, L] (
    ts: WeakTransitionSystem[S, A, L]
    ) {
  
  def compute() = {

    val weakTrans = for {
      s1 <- ts.nodes
    } yield (s1 -> {
      for { a <- ts.weakEnabled2(s1) ++ ts.silentActions }
        yield (a -> ts.weakPost(s1, a))
    }.toMap)

    new WeakTransitionSystem(new LabeledRelation(weakTrans.toMap), ts.nodeLabeling, ts.silentActions)
  }
  
}