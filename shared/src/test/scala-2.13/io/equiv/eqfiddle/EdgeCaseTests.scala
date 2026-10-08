package io.equiv.eqfiddle

import io.equiv.eqfiddle.spectroscopy.{CCSSpectroscopy, StrongSpectroscopy}
import io.equiv.eqfiddle.tool.model.NodeID
import io.equiv.eqfiddle.hml.StrongObservationNotion
import io.equiv.eqfiddle.hml.HML

class EdgeCaseTests extends CSSSampleTests[StrongObservationNotion, HML.Formula[String]] {
  override val spectrum = StrongObservationNotion.LTBTS
  
  val reviewSystem = TestSamples.samples.find(_._1 == "review-counterexamples").get._2

  val sampleNames: List[(String, String, List[String], List[String])] = List(
    ("L1", "R1", List("readiness", "simulation"), List("failure-trace", "impossible-future")),
    ("L2", "R2", List("readiness", "simulation"), List("failure-trace", "impossible-future"))
  )

  runTest(reviewSystem, sampleNames, new StrongSpectroscopy(_), "(energy-labeled)")

  describe("The shared CCS spectroscopy service") {
    val request = CCSSpectroscopy.Request("P = a.b + a.c\nQ = a.(b + c)", "P", "Q")

    it("preserves directional preorders and distinguishing certificates") {
      val analysis = CCSSpectroscopy.run(request).toOption.get
      val result = analysis.result
      result.foundPreorders(NodeID("P"), NodeID("Q")).map(_.name) should contain ("simulation")
      result.foundPreorders(NodeID("Q"), NodeID("P")).map(_.name) should not contain "simulation"
      result.findEqs(NodeID("P"), NodeID("Q")).map(_.name) shouldBe List("trace")
      result.foundDistinctionsWithCertificate(NodeID("P"), NodeID("Q")) should not be empty
    }

    it("reports source locations and invalid process names without computing a result") {
      val error = CCSSpectroscopy.run(request.copy(source = "P = (")).swap.toOption.get
      error.code shouldBe "PARSE_ERROR"
      error.line should be > 0
      CCSSpectroscopy.run(request.copy(left = "Missing")).swap.toOption.get.code shouldBe "UNKNOWN_PROCESS"
      CCSSpectroscopy.run(request.copy(maxStates = 0)).swap.toOption.get.code shouldBe "INVALID_REQUEST"
    }

    it("checks a single notion in both directions, including weak notions") {
      val checked = CCSSpectroscopy.check(request, "simulation").toOption.get.result
      checked.items.find(item => item.left == NodeID("P") && item.right == NodeID("Q")).get.isMaintained shouldBe true
      checked.items.find(item => item.left == NodeID("Q") && item.right == NodeID("P")).get.isMaintained shouldBe false
      val weak = request.copy(source = "P = tau.a\nQ = a")
      CCSSpectroscopy.check(weak, "sr-branching-bisimulation").toOption.get.result.items.forall(_.isMaintained) shouldBe true
      CCSSpectroscopy.check(request, "unknown").swap.toOption.get.code shouldBe "UNKNOWN_NOTION"
    }
  }
}