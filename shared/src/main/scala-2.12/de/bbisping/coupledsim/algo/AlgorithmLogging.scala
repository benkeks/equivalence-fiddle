package de.bbisping.coupledsim.algo

import de.bbisping.coupledsim.util.Relation
import scala.collection.mutable.ListBuffer
import de.bbisping.coupledsim.util.Coloring

trait AlgorithmLogging[S, A, L] {
  
  private val log = ListBuffer[() => AlgorithmLogging.LogEntry[S, A, L]]()
  
  private def logAppend[O](f: (O, String) => AlgorithmLogging.LogEntry[S, A, L])(obj: O, comment: String) = {
    if (AlgorithmLogging.loggingActive && log.size < AlgorithmLogging.maxLogLength) {
      log += (() => f(obj, comment))
    }
  }

  def logRelation =
    logAppend(AlgorithmLogging.LogRelation[S, A, L]) _

  def logRichRelation =
    logAppend(AlgorithmLogging.LogRichRelation[S, A, L]) _
  
  def getReplay() = log toList
}

object AlgorithmLogging {
  
  abstract class LogEntry[S, A, L]
  
  case class LogRelation[S, A, L](rel: Relation[S], comment: String) extends LogEntry[S, A, L]
  //case class LogColoring[S, A, L](colors: Coloring[S], comment: String) extends LogEntry[S, A, L]

  case class LogRichRelation[S, A, L](rel: Iterable[(Iterable[S], Iterable[S])], comment: String) extends LogEntry[S, A, L]
  
  var loggingActive = true
  
  var maxLogLength = 100
  
}