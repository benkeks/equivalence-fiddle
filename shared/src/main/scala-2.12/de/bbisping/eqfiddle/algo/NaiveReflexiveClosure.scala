package de.bbisping.eqfiddle.algo

import scala.annotation.migration
import de.bbisping.eqfiddle.ts.TransitionSystem
import de.bbisping.eqfiddle.util.FixedPoint
import de.bbisping.eqfiddle.util.Relation
import de.bbisping.eqfiddle.util.LabeledRelation

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