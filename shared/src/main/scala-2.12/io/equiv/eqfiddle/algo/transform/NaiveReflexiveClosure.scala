package io.equiv.eqfiddle.algo.transform

import scala.annotation.migration
import io.equiv.eqfiddle.ts.TransitionSystem
import io.equiv.eqfiddle.util.FixedPoint
import io.equiv.eqfiddle.util.Relation
import io.equiv.eqfiddle.util.LabeledRelation

class NaiveReflexiveClosure[S, A, L] (
    ts: TransitionSystem[S, A, L],
    reflexionActionLabel: A) {
  
  def compute() = {
    
    val newTrans = for {
      s <- ts.nodes
    } yield (s, reflexionActionLabel, s)
    val newSteps = ts.step merge new LabeledRelation(newTrans)
    
    TransitionSystem(newSteps, ts.nodeLabeling)
  }
  
}