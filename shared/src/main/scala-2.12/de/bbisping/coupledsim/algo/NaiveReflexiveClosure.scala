package de.bbisping.coupledsim.algo

import scala.annotation.migration
import de.bbisping.coupledsim.ts.TransitionSystem
import de.bbisping.coupledsim.util.FixedPoint
import de.bbisping.coupledsim.util.Relation
import de.bbisping.coupledsim.util.LabeledRelation

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