package io.equiv.eqfiddle.ts

import io.equiv.eqfiddle.util.LabeledRelation


class DirectTSImporter(
    tsFileContent: String
  ) {
  
  val silentActions = Set(Symbol("i"))
  
  
  def result(): WeakTransitionSystem[Int, Symbol, Symbol] = {
    
    val transitionMatcher = """(\d*)\s*,\s*(\d*)\s*,\s*\"?([^\"]*)\"?""".r
    
    val transitions = for {
      l <- tsFileContent.linesIterator
      transitionMatcher(srcS, tarS, actionS) = l
      src = srcS.toInt
      tar = tarS.toInt
      action = Symbol(actionS)
    } yield {
      (src, action, tar)
    }
    
    val trans = new LabeledRelation(transitions.toSet)

    val labels = (trans.lhs ++ trans.rhs) map { n => (n, Symbol("")) } toMap
    
    new WeakTransitionSystem[Int, Symbol, Symbol](trans, labels, silentActions)
  }
}
