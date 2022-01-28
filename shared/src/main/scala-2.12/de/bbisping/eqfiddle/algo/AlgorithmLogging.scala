package de.bbisping.eqfiddle.algo

import de.bbisping.eqfiddle.util.Relation
import scala.collection.mutable.ListBuffer
import de.bbisping.eqfiddle.util.Coloring
import de.bbisping.eqfiddle.util.LabeledRelation

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

  def debugLog(msg: => String) = {
    if (AlgorithmLogging.debugLogActive) {
      println(msg)
    }
  }
}

object AlgorithmLogging {
  
  abstract class LogEntry[S, A, L]
  
  case class LogRelation[S, A, L](rel: LabeledRelation[S, String], comment: String) extends LogEntry[S, A, L]

  case class LogRichRelation[S, A, L](rel: LabeledRelation[(Set[S], String, Set[S]), String], comment: String) extends LogEntry[S, A, L]
  
  var loggingActive = true
  
  var maxLogLength = 100
  
  var debugLogActive = true
}