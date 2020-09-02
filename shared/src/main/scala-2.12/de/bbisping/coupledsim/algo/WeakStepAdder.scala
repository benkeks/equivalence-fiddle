package de.bbisping.coupledsim.algo

import scala.annotation.migration
import de.bbisping.coupledsim.ts.TransitionSystem
import de.bbisping.coupledsim.util.FixedPoint
import de.bbisping.coupledsim.util.Relation
import de.bbisping.coupledsim.util.LabeledRelation

class WeakStepAdder[S, A, L] (
    ts: TransitionSystem[S, A, L],
    transitiveSteps: Set[A]
    ) {
  
  def compute() = {
    val newTrans = for {
      s0 <- ts.nodes
      tau1 <- transitiveSteps
      s1 <- ts.post(s0, tau1) + s0
      (a, ss2) <- ts.post(s1)
      s2 <- ss2
      tau2 <- transitiveSteps
      s3 <- ts.post(s2, tau2) + s2
    } yield (s0, a, s3)
    
    new LabeledRelation(ts.step.tupleSet ++ newTrans)
  }
  
}