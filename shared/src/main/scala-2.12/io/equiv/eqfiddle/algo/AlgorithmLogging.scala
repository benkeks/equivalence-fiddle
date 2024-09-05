package io.equiv.eqfiddle.algo

import io.equiv.eqfiddle.util.Relation
import scala.collection.mutable.ListBuffer
import io.equiv.eqfiddle.util.Coloring
import io.equiv.eqfiddle.util.LabeledRelation
import io.equiv.eqfiddle.hml.ObservationClass
import io.equiv.eqfiddle.hml.Spectrum
trait AlgorithmLogging[S] {
  
  private val log = ListBuffer[() => AlgorithmLogging.LogEntry[S]]()
  
  private def logAppend[O](f: (O, String) => AlgorithmLogging.LogEntry[S])(obj: O, comment: String) = {
    if (AlgorithmLogging.loggingActive && log.size < AlgorithmLogging.maxLogLength) {
      log += (() => f(obj, comment))
    }
  }

  var uriEncoder = (s: String) => s

  def logRelation =
    logAppend(AlgorithmLogging.LogRelation[S]) _

  def logRichRelation =
    logAppend(AlgorithmLogging.LogRichRelation[S]) _
  
  def getReplay() = log toList

  def debugLog(msg: => String, asLink: String = "") = {
    if (AlgorithmLogging.debugLogActive) {
      if (asLink == "") {
        println(msg)
      } else {
        println(uriEncoder(asLink + msg))
      }
    }
  }
}

object AlgorithmLogging {
  
  abstract class LogEntry[S]
  
  case class LogRelation[S](rel: LabeledRelation[S, String], comment: String) extends LogEntry[S]

  case class LogRichRelation[S](rel: LabeledRelation[(Set[S], String, Set[S]), String], comment: String) extends LogEntry[S]

  case class LogSpectrum[S, OC <: ObservationClass](spectrum: Spectrum[OC], preords: List[String], equations: List[String], distCoordsLR: List[OC], distCoordsRL: List[OC], comment: String) extends LogEntry[S]
  
  var loggingActive = true
  
  var maxLogLength = 100
  
  var debugLogActive = true
}