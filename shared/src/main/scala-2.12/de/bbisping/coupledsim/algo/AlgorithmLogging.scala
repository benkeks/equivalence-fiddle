package de.bbisping.coupledsim.algo

import de.bbisping.coupledsim.util.Relation
import scala.collection.mutable.ListBuffer
import de.bbisping.coupledsim.util.Coloring

trait AlgorithmLogging[S, A, L] {
  
  private val log = ListBuffer[() => AlgorithmLogging.LogEntry[S, A, L]]()
  
  def logRelation(rel: Relation[S], comment: String) = {
    if (AlgorithmLogging.loggingActive && log.size < AlgorithmLogging.maxLogLength) {
      log += (() => AlgorithmLogging.LogRelation[S, A, L](rel, comment))
    }
  }
  
  def getReplay() = log toList
}

object AlgorithmLogging {
  
  abstract class LogEntry[S, A, L]
  
  case class LogRelation[S, A, L](rel: Relation[S], comment: String) extends LogEntry[S, A, L]
  //case class LogColoring[S, A, L](colors: Coloring[S], comment: String) extends LogEntry[S, A, L]
  
  var loggingActive = true
  
  var maxLogLength = 100
  
}