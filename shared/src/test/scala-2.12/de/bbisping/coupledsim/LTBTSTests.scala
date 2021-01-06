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
  AlgorithmLogging.debugLogActive = false
  
  val ltbtsSystem = TestSamples.samples.find(_._1 == "ltbts1").get._2

  private def toSpectrumClassSet(names: Iterable[String]) = (for {
    n <- names
    cl <- ObservationClass.getSpectrumClass(n)
  } yield cl).toSet

  val samples = List(
    ("L13", "R13", List("simulation"), List("failure")),
    ("R13", "L13", List("2-nested-simulation"), List("bisimulation")),
    ("L16", "R16", List("simulation"), List("failure")),
    ("R16", "L16", List("2-nested-simulation"), List("bisimulation")),
    ("L21", "R21", List("readiness"), List("failure-trace", "impossible-future", "simulation")),
    ("R21", "L21", List("readiness"), List("failure-trace", "impossible-future", "simulation")),
    ("L24", "R24", List("possible-future", "ready-simulation"), List("2-nested-simulation")),
    ("R24", "L24", List("failure-trace", "impossible-future"), List("readiness", "simulation")),
    ("L27", "R27", List("possible-future"), List("failure-trace", "simulation")),
    ("R27", "L27", List("possible-future"), List("failure-trace", "simulation")),
    ("L31", "R31", List("ready-simulation"), List("impossible-future")),
    ("R31", "L31", List("impossible-future", "ready-trace"), List("possible-future", "simulation")),
    ("L34", "R31", List("ready-simulation"), List("impossible-future")),
    ("R31", "L34", List("2-nested-simulation"), List("bisimulation")),
    ("L38", "R24", List("simulation"), List("failure")),
    ("R24", "L38", List("failure-trace","impossible-future"), List("readiness", "simulation")),
    ("L42", "R42", List("2-nested-simulation"), List("bisimulation")),
    ("R42", "L42", List("2-nested-simulation"), List("bisimulation")),
    ("L50", "R50", List("ready-simulation"), List("impossible-future")),
    ("R50", "L50", List("ready-trace", "impossible-future"), List("possible-future", "simulation"))
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

        var foundPreorders = Set[String]()

        log.foreach {
          case LogRichRelation(_, comment) if comment.startsWith(n1 + " distinguished") =>
            val commentParts = comment.split(Array(',', ' ')).toSet intersect ObservationClass.LTBTSNotionNames
            val diff = notPreordsStr diff commentParts
            it ("should be distinguished by " + notPreordsStr.mkString(",")) {
              diff should be (empty)
            }
          case LogRelation(_, comment) if comment.startsWith(n1 + " preordered") =>
            val commentParts = comment.split(Array(',', ' ')).toSet intersect ObservationClass.LTBTSNotionNames
            foundPreorders ++= commentParts
          case _ =>
        }

        it ("should only be preordered by " + preordsStr.mkString(",")) {
          (foundPreorders diff preordsStr) should be (empty)
          (preordsStr diff foundPreorders) should be (empty)
        }
      }
    }
  }
}