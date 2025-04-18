package io.equiv.eqfiddle.algo

import io.equiv.eqfiddle.util.Relation
import scala.collection.mutable.ListBuffer
import io.equiv.eqfiddle.util.Coloring
import io.equiv.eqfiddle.util.LabeledRelation
import io.equiv.eqfiddle.hml.ObservationNotion
import io.equiv.eqfiddle.hml.Spectrum

trait AlgorithmLogging[S] {
  
  val individualDebugLogActive: Boolean = true

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

  def debugLog(msg: => String, asLink: String = "", logLevel: Int = 8): String =
    if (individualDebugLogActive)
      AlgorithmLogging.debugLog(msg, asLink, logLevel)
    else ""
}

object AlgorithmLogging {
  
  abstract class LogEntry[S]
  
  case class LogRelation[S](rel: LabeledRelation[S, String], comment: String) extends LogEntry[S]

  case class LogRichRelation[S](rel: LabeledRelation[(Set[S], String, Set[S]), String], comment: String) extends LogEntry[S]

  case class LogSpectrum[S, OC <: ObservationNotion](
    spectrum: Spectrum[OC],
    preords: List[String],
    postords: List[String],
    equations: List[String],
    distCoordsLR: List[(OC, String)],
    distCoordsRL: List[(OC, String)],
    comment: String)
  extends LogEntry[S]
  
  var loggingActive = true
  
  var maxLogLength = 100
  
  var debugLogActive: Boolean = true

  /* crash on debug messages with loglevel <= 4 */
  var strictMode: Boolean = true

  var uriEncoder = (s: String) => s

  var globalLogLevel = 7

  def debugLog(msg: => String, asLink: String = "", logLevel: Int = 8): String = {
    if (AlgorithmLogging.debugLogActive) {
      val outMsg = if (asLink == "") msg else uriEncoder(asLink + msg)
      if (logLevel <= globalLogLevel) {
        if (logLevel <= 4) {
          if (strictMode) {
            throw new Exception(outMsg)
          } else {
            Console.err.println(Console.RED + outMsg)
          }
        } else if (logLevel <= 6) {
          Console.out.println(Console.YELLOW + outMsg)
        } else {
          Console.out.println(outMsg)
        }
      }
      outMsg
    } else {
      ""
    }
  }
}