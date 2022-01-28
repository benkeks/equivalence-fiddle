package de.bbisping.eqfiddle.algo

import scala.annotation.migration
import de.bbisping.eqfiddle.ts.TransitionSystem
import de.bbisping.eqfiddle.util.FixedPoint
import de.bbisping.eqfiddle.util.LabeledRelation

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