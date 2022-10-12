package io.equiv.eqfiddle.algo

import scala.annotation.migration
import io.equiv.eqfiddle.ts.TransitionSystem
import io.equiv.eqfiddle.util.FixedPoint
import io.equiv.eqfiddle.util.LabeledRelation

class NaiveTransitiveClosure[S, A, L] (
    ts: TransitionSystem[S, A, L],
    transitiveSteps: Set[A]) {
  
  def compute() = {
    FixedPoint[LabeledRelation[S, A]] (
      { p =>
        val newTrans = for {
          s <- ts.nodes
          a1 <- transitiveSteps
          ns <- p.values(s, a1)
          a2 <- transitiveSteps
          ns2 <- p.values(ns, a2)
        } yield (s, a1, ns2)
        new LabeledRelation(p.tupleSet ++ newTrans)
      },
      _.size == _.size
    ) (ts.step)
    
  }
  
}