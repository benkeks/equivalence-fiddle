package io.equiv.eqfiddle.algo.transform

import io.equiv.eqfiddle.ts.TransitionSystem
import io.equiv.eqfiddle.util.LabeledRelation
import io.equiv.eqfiddle.ts.WeakTransitionSystem
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import io.equiv.eqfiddle.util.Coloring

/** tau-loop-compression
 *  
 *  nodes on a tau loop are considered equal by many weak similarities.
 *  we use Tarjans [Tar72] algorithm for strongly connected components to find
 *  tau-looped components.
 *  (along the lines of the algorithm from https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm)
 *  
 *  */
class TauLoopCompression[S, A, L] (
    ts: WeakTransitionSystem[S, A, L],
    protectedNodes: Set[S] = Set[S]()
  ) {
  
  def compute() = {
    val coloring = computeColoring()
    new BuildQuotientSystem(ts, coloring, protectedNodes = protectedNodes).build()
  }
  
  def computeColoring() = {
    
    var currentIndex = 0
    var stack = List[S]()
    val index = new HashMap[S, Int]()
    val rep = new HashMap[S, Int]()
    val onStack = new HashSet[S]()
    
    def colorComponents(s: S) {
      index(s) = currentIndex
      rep(s) = currentIndex
      stack = s::stack
      onStack += s
      currentIndex += 1
      
      for (t <- ts.silentSteps.values(s)) {
        if (!index.contains(t)) {
          colorComponents(t)
          rep(s) = Math.min(rep(s), rep(t))
        } else if (onStack(t)) {
          rep(s) = Math.min(rep(s), index(t))
        }
      }
      
      if (rep(s) == index(s)) {
        stack = stack.dropWhile { t =>
          onStack -= t
          t != s
        }
      }
    }
    
    for {
      s <- ts.nodes
      if !index.contains(s)
    } {
      colorComponents(s)
    }
    
    Coloring(rep.toMap)
  }
  
}