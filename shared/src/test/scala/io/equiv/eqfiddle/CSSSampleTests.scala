package io.equiv.eqfiddle

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.Inspectors.forAll
import org.scalatest.matchers.BeMatcher
import org.scalatest.matchers.should

import io.equiv.eqfiddle.tool.model.NodeID
import io.equiv.eqfiddle.util.Relation
import io.equiv.eqfiddle.hml.ObservationNotion
import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.algo.AlgorithmLogging
import io.equiv.eqfiddle.hml.Spectrum
import io.equiv.eqfiddle.hml.HML
import io.equiv.eqfiddle.spectroscopy.Spectroscopy

trait CSSSampleTests[OC <: ObservationNotion, CF <: HML.Formula[String]]
    extends AnyFunSpec with should.Matchers  {
  /** The spectrum under test */
  def spectrum: Spectrum[OC]

  /** Type for a check function that validates a preorder relation for a notion.
    * Takes the notion name and the relation (set of (from, label, to) tuples) and returns
    * an optional error message. None means the check passed, Some(message) means it failed.
    */
  type NotionCheck = (WeakTransitionSystem[NodeID,String,String], Set[(NodeID, String, NodeID)]) => Option[String]

  /** Map of checks to run per notion. Override in subclasses to provide custom checks. */
  protected def notionChecks: Map[String, NotionCheck] = Map()

  private def toSpectrumClassSet(names: Iterable[String]) = (for {
    n <- names
    cl <- spectrum.getSpectrumClass.get(n)
  } yield cl).toSet

  /**
    * 
    *
    * @param sampleSystem 
    * @param sampleNames list of tuples of (p1, p2, exact list of maintained preorder, list of distinctions)
    * @param spectroscopyAlgo
    * @param title
    */
  def runTest(
      sampleSystem: WeakTransitionSystem[NodeID,String,String],
      sampleNames: List[(String, String, List[String], List[String])],
      spectroscopyAlgo: (WeakTransitionSystem[NodeID,String,String]) => Spectroscopy[NodeID,String,String,CF],
      title: String,
      config: Spectroscopy.Config = Spectroscopy.Config()) = {

    val samples = sampleNames.map {
      case (n1, n2, preords, notPreords) =>
        (n1, n2, toSpectrumClassSet(preords), toSpectrumClassSet(notPreords))
    }
    describe("The Spectroscopy " + title) {
      forAll(samples) { case (n1s, n2s, preords, notPreords) =>
        describe("for " + n1s + " <= " + n2s) {
          val n1 = NodeID(n1s)
          val n2 = NodeID(n2s)

          val algo = spectroscopyAlgo(sampleSystem)

          val preordsStr = preords.map(_.name)
          val notPreordsStr = notPreords.map(_.name).intersect(algo.spectrum.notionNames)

          val result = algo.decideAll(List((n1, n2)), config)

          def maintainsPreorder(notionName: String): Boolean = {
            val preorderResult = algo.checkIndividualPreorder(
              List((n1, n2)),
              notionName,
              config.copy(computeMaxRelation = true) // this is needed to get a full witness relation for simulation-like notions
                // (otherwise some tuples that do not matter for the game might be missing.)
            )
            val isMaintained = preorderResult.items.exists(item => item.left == n1 && item.right == n2 && item.isMaintained)
            if (isMaintained) {
              for {
                check <- notionChecks.get(notionName)
                errorMsg <- check(sampleSystem, preorderResult.relation)
              } {
                fail(s"Notion check failed for $notionName: $errorMsg")
              }
            }
            
            isMaintained
          }

          val foundDistinctions = result.foundDistinctions(n1, n2).map(
            d => d.name match { case "2bisimulation" => "bisimulation"; case "2trace" => "trace"; case n => n }
          ).toSet
          it ("should at least be distinguished by " + notPreordsStr.mkString(",")) {
            (notPreordsStr diff foundDistinctions) should be (empty)
          }

          val foundPreorders = result.foundPreorders(n1, n2).map(_.name).toSet
          it ("should exactly be preordered by " + preordsStr.mkString(",")) {
            if (!(preordsStr subsetOf algo.spectrum.notionNames)) {
              cancel(s"$preordsStr do not apply for $title spectrum")
            }
            (foundPreorders diff preordsStr) should be (empty)
            (preordsStr diff foundPreorders) should be (empty)
          }

          it ("checkIndividualPreorder should accept " + preordsStr.mkString(",")) {
            if (!(preordsStr subsetOf algo.spectrum.notionNames)) {
              cancel(s"$preordsStr do not apply for $title spectrum")
            }
            forAll(preordsStr) { notionName =>
              withClue(s"$notionName should be maintained: ") {
                maintainsPreorder(notionName) shouldBe true
              }
            }
          }

          it ("checkIndividualPreorder should reject " + notPreordsStr.mkString(",")) {
            forAll(notPreordsStr) { notionName =>
              withClue(s"$notionName should not be maintained: ") {
                maintainsPreorder(notionName) shouldBe false
              }
            }
          }
        }
      }
    }
  }
}