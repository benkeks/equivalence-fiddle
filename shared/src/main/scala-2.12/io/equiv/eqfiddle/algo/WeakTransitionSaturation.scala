package io.equiv.eqfiddle.algo

import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.util.LabeledRelation

class WeakTransitionSaturation[S, A, L] (
    ts: WeakTransitionSystem[S, A, L],
    onStates: Option[Set[S]] = None
    ) {
  
  def compute() = {

    val weakTrans = onStates match {
      case None =>
        for {
          s1 <- ts.nodes
        } yield (s1 -> {
          for { a <- ts.weakEnabled2(s1) ++ ts.silentActions }
            yield (a -> ts.weakPost(s1, a))
        }.toMap)
      case Some(affectedStates) =>
        for {
          s1 <- ts.nodes
        } yield (s1 -> {
          if (affectedStates(s1)) {
            for { a <- ts.weakEnabled2(s1) ++ ts.silentActions }
              yield (a -> ts.weakPost(s1, a))
          } else {
            ts.post(s1)
          }
        }.toMap)
    }

    new WeakTransitionSystem(new LabeledRelation(weakTrans.toMap), ts.nodeLabeling, ts.silentActions)
  }

}