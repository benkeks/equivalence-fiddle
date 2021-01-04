package de.bbisping.coupledsim

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.Inspectors.forAll
import org.scalatest.matchers.BeMatcher
import org.scalatest.matchers.should

import de.bbisping.coupledsim.tool.model.NodeID
import de.bbisping.coupledsim.util.Relation
import de.bbisping.coupledsim.hml.ObservationClass
import de.bbisping.coupledsim.hml.HMLGamePlayer
import de.bbisping.coupledsim.algo.AlgorithmLogging
import de.bbisping.coupledsim.algo.AlgorithmLogging.{LogRelation, LogRichRelation}

class LTBTSTests extends AnyFunSpec with should.Matchers  {
  
  AlgorithmLogging.loggingActive = true
  
  val ltbtsSystem = TestSamples.samples.find(_._1 == "ltbts1").get._2

  private def toSpectrumClassSet(names: Iterable[String]) = (for {
    n <- names
    cl <- ObservationClass.getSpectrumClass(n)
  } yield cl).toSet

  val samples = List(
    ("L13", "R13", List("simulation"), List("failure")),
    ("R13", "L13", List("2-nested-simulation"), List("bisimulation")),
    ("L16", "R16", List("simulation"), List("failure")),
    ("R16", "L16", List("2-nested-simulation"), List("bisimulation"))
  ).map {
    case (n1, n2, preords, notPreords) =>
      (n1, n2, toSpectrumClassSet(preords), toSpectrumClassSet(notPreords))
  }
  
  describe("The Spectroscopy") {
    forAll(samples) { case (n1s, n2s, preords, notPreords) =>
      describe("for " + n1s + " <= " + n2s) {
        val n1 = NodeID(n1s)
        val n2 = NodeID(n2s)
        val preordsStr = preords.map(_._1)
        val notPreordsStr = notPreords.map(_._1)

        val algo = new HMLGamePlayer(ltbtsSystem, List(n1, n2))
        algo.compute()

        val log = for {
          r <- algo.getReplay()
        } yield r()
        
        log.foreach {
          case LogRichRelation(_, comment) if comment.startsWith(n1 + " distinguished") =>
            val commentParts = comment.split(Array(',', ' ')).toSet
            println(commentParts)
            val diff = notPreordsStr diff commentParts
            it ("should only be distinguished by " + notPreordsStr.mkString(",")) {
              diff should be (empty)
            }
          case LogRelation(_, comment) if comment.startsWith(n1 + " preordered") =>
            val commentParts = comment.split(Array(',', ' ')).toSet
            println(commentParts)
            val diff = preordsStr diff commentParts
            it ("should only be preordered by " + preordsStr.mkString(",")) {
              diff should be (empty)
            }
          case _ =>
        }
      }
    }
  }
}