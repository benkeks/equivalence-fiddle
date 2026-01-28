package io.equiv.eqfiddle.algo.sigref

import io.equiv.eqfiddle.ts.TransitionSystem
import io.equiv.eqfiddle.util.Relation
import io.equiv.eqfiddle.util.Coloring
import io.equiv.eqfiddle.algo.AlgorithmLogging
import io.equiv.eqfiddle.ts.WeakTransitionSystem

/**
 * Abstract signature refinement (bi)similarity algorithm
 * that keeps track of signature subsets!
 * 
 * */

abstract class SignatureRefinement[S, A, L] (
    val ts: WeakTransitionSystem[S, A, L],
    onStates: Option[Set[S]] = None
  ) extends AlgorithmLogging[S] {
  
  val labelColors = onStates match {
    case None =>
      Coloring.fromPartition(Set(ts.nodes))
    case Some(affectedStates) =>
      Coloring.fromPartition(Set(affectedStates) ++ (ts.nodes -- affectedStates).map(Set(_)))
  }
  
  val actionColors =
    Coloring.fromPartition(ts.actions.map(Set(_)).toSet)
  
  var partition = labelColors
  
  def signature(s: S): Set[(Coloring.Color, Coloring.Color)]
  
  def computePartition() = {

    var size = 1
    while ({
      
      val signatures = for {
        s <- ts.nodes
        sig = signature(s)
      } yield (sig, s)
      
      val blocks = signatures.groupBy(_._1).mapValues(_.map(_._2))
      
      val colorMap = blocks.keys.zipWithIndex.toMap
      
      val newColoring = for {
        (sig, ss) <- blocks
        s <- ss
      } yield (s, colorMap(sig))
      
      partition = new Coloring(newColoring)
      
      val oldSize = size
      size = colorMap.size
      
      debugLog(s"sigref: $size")
      
      oldSize != size
    }) {}
    
    partition
  }
  
  def compute() = {
    Relation.fromColoring(computePartition())
  }
}