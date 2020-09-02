package de.bbisping.coupledsim.ts

import de.bbisping.coupledsim.util.LabeledRelation


class DirectTSImporter(
    tsFileContent: String
  ) {
  
  val silentActions = Set(Symbol("i"))
  
  
  def result(): WeakTransitionSystem[Int, Symbol, Symbol] = {
    
    val transitionMatcher = """(\d*)\s*,\s*(\d*)\s*,\s*\"?([^\"]*)\"?""".r
    
    val transitions = for {
      l <- tsFileContent.linesIterator
      val transitionMatcher(srcS, tarS, actionS) = l
      val src = srcS.toInt
      val tar = tarS.toInt
      val action = Symbol(actionS)
    } yield {
      (src, action, tar)
    }
    
    val trans = new LabeledRelation(transitions.toSet)

    val labels = (trans.lhs ++ trans.rhs) map { n => (n, Symbol("")) } toMap
    
    new WeakTransitionSystem[Int, Symbol, Symbol](trans, labels, silentActions)
  }
}
