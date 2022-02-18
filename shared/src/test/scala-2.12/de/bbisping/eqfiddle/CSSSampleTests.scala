package de.bbisping.eqfiddle

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.Inspectors.forAll
import org.scalatest.matchers.BeMatcher
import org.scalatest.matchers.should

import de.bbisping.eqfiddle.tool.model.NodeID
import de.bbisping.eqfiddle.util.Relation
import de.bbisping.eqfiddle.hml.ObservationClass
import de.bbisping.eqfiddle.spectroscopy.PositionalSpectroscopy
import de.bbisping.eqfiddle.ts.WeakTransitionSystem
import de.bbisping.eqfiddle.algo.AlgorithmLogging
import de.bbisping.eqfiddle.algo.AlgorithmLogging.{LogRelation, LogRichRelation}

trait CSSSampleTests extends AnyFunSpec with should.Matchers  {
  
  AlgorithmLogging.loggingActive = true
  AlgorithmLogging.debugLogActive = false
  
  private def toSpectrumClassSet(names: Iterable[String]) = (for {
    n <- names
    cl <- ObservationClass.getSpectrumClass(n)
  } yield cl).toSet

  def runTest(sampleSystem: WeakTransitionSystem[NodeID,String,String], sampleNames: List[(String, String, List[String], List[String])]) = {
    val samples = sampleNames.map {
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

          val algo = new PositionalSpectroscopy(sampleSystem, List(n1, n2))
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
}