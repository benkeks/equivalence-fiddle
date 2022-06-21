package de.bbisping.eqfiddle.algo

import de.bbisping.eqfiddle.util.Relation
import scala.collection.mutable.ListBuffer
import de.bbisping.eqfiddle.util.Coloring
import de.bbisping.eqfiddle.util.LabeledRelation

trait AlgorithmLogging[S] {
  
  private val log = ListBuffer[() => AlgorithmLogging.LogEntry[S]]()
  
  private def logAppend[O](f: (O, String) => AlgorithmLogging.LogEntry[S])(obj: O, comment: String) = {
    if (AlgorithmLogging.loggingActive && log.size < AlgorithmLogging.maxLogLength) {
      log += (() => f(obj, comment))
    }
  }

  def logRelation =
    logAppend(AlgorithmLogging.LogRelation[S]) _

  def logRichRelation =
    logAppend(AlgorithmLogging.LogRichRelation[S]) _
  
  def getReplay() = log toList

  def debugLog(msg: => String) = {
    if (AlgorithmLogging.debugLogActive) {
      println(msg)
    }
  }
}

object AlgorithmLogging {
  
  abstract class LogEntry[S]
  
  case class LogRelation[S](rel: LabeledRelation[S, String], comment: String) extends LogEntry[S]

  case class LogRichRelation[S](rel: LabeledRelation[(Set[S], String, Set[S]), String], comment: String) extends LogEntry[S]
  
  var loggingActive = true
  
  var maxLogLength = 100
  
  var debugLogActive = true
}