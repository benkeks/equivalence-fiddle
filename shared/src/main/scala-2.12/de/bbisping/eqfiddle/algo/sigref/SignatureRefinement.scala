package de.bbisping.eqfiddle.algo.sigref

import de.bbisping.eqfiddle.ts.TransitionSystem
import de.bbisping.eqfiddle.util.Relation
import de.bbisping.eqfiddle.util.Coloring
import de.bbisping.eqfiddle.algo.AlgorithmLogging
import de.bbisping.eqfiddle.ts.WeakTransitionSystem

/**
 * Abstract signature refinement (bi)similarity algorithm
 * that keeps track of signature subsets!
 * 
 * */

abstract class SignatureRefinement[S, A, L] (
    val ts: WeakTransitionSystem[S, A, L])
  extends AlgorithmLogging[S] {
  
  val labelColors = {
    val parts = for {
      (_, ss) <- ts.nodesByLabel
    } yield ss
    Coloring.fromPartition(parts.toSet)
  }
  
  val actionColors =
    Coloring.fromPartition(ts.actions.map(Set(_)).toSet)
  
  var partition = labelColors
  
  def signature(s: S): Set[(Coloring.Color, Coloring.Color)]
  
  def computePartition() = {

    var size = 1
    println("start sigref")
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
      //logRelation(Relation.fromColoring(partition), "refinement "+size)
      
      println(size)
      
      oldSize != size
    }) {}
    
    println("sigref complete")
    
    partition
  }
  
  def compute() = {
    Relation.fromColoring(computePartition())
  }
}