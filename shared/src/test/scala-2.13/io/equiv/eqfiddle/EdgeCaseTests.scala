package io.equiv.eqfiddle

import io.equiv.eqfiddle.spectroscopy.{CCSSpectroscopy, StrongSpectroscopy}
import io.equiv.eqfiddle.tool.model.NodeID
import io.equiv.eqfiddle.hml.StrongObservationNotion
import io.equiv.eqfiddle.hml.HML
import io.equiv.eqfiddle.spectroscopy.{Spectroscopy, WeakSpectroscopy}
import io.equiv.eqfiddle.tool.benchmark.Comparator

class EdgeCaseTests extends CSSSampleTests[StrongObservationNotion, HML.Formula[String]] {
  override val spectrum = StrongObservationNotion.LTBTS
  
  val reviewSystem = TestSamples.samples.find(_._1 == "review-counterexamples").get._2

  val sampleNames: List[(String, String, List[String], List[String])] = List(
    ("L1", "R1", List("readiness", "simulation"), List("failure-trace", "impossible-future")),
    ("L2", "R2", List("readiness", "simulation"), List("failure-trace", "impossible-future"))
  )

  runTest(reviewSystem, sampleNames, new StrongSpectroscopy(_), "(energy-labeled)")

  describe("CSV comparison with divergence marking") {
    def compare(csv: String, left: String, right: String, divergences: Boolean): String = {
      val file = java.nio.file.Files.createTempFile("eqfiddle-compare-", ".csv")
      val output = new java.io.ByteArrayOutputStream()
      val stream = new java.io.PrintStream(output)
      try {
        java.nio.file.Files.writeString(file, csv)
        Console.withOut(stream) {
          new Comparator(new WeakSpectroscopy(_)).run(
            file.toString, left, right, Spectroscopy.Config(), divergences = divergences)
        }
        output.toString("UTF-8")
      } finally {
        stream.close()
        java.nio.file.Files.deleteIfExists(file)
      }
    }

    it("distinguishes a tau cycle from deadlock only when requested") {
      val csv = "0,Loop,\n1,LoopPart,\n2,Deadlock,\n0,1,tau\n1,0,tau\n0,2,tau\n"
      compare(csv, "Loop", "Deadlock", divergences = false) should include ("sr-branching-bisimulation")
      compare(csv, "Loop", "Deadlock", divergences = true) should not include "weak-enabledness"
      compare(csv, "1", "2", divergences = true) should not include "weak-enabledness"
    }

    it("compares selected states in the same compressed tau component") {
      val csv = "0,1,tau\n1,0,tau\n0,2,tau\n"
      compare(csv, "0", "1", divergences = true) should include ("sr-branching-bisimulation")
    }

    it("leaves a system without tau cycles unchanged") {
      val csv = "0,1,a\n2,1,a\n"
      compare(csv, "0", "2", divergences = true) shouldBe compare(csv, "0", "2", divergences = false)
    }

    it("preserves visible trace distinctions between divergent states") {
      val csv = "0,0,tau\n0,2,a\n1,1,tau\n1,2,b\n"
      compare(csv, "0", "1", divergences = true).trim shouldBe ""
      compare(csv, "1", "0", divergences = true).trim shouldBe ""
    }

    it("distinguishes divergence within a branching class despite equal divergence traces") {
      val csv = "0,0,tau\n0,3,a\n0,2,tau\n1,3,a\n1,2,tau\n2,2,tau\n"
      compare(csv, "0", "1", divergences = false) should include ("sr-branching-bisimulation")
      for ((left, right) <- Seq(("0", "1"), ("1", "0"))) {
        val result = compare(csv, left, right, divergences = true)
        result should include ("sr-delay-bisimulation")
        result should not include "branching-bisimulation"
      }
    }

    it("reports when no deadlock state is available for marking") {
      val error = intercept[IllegalArgumentException] {
        compare("0,0,tau\n", "0", "0", divergences = true)
      }
      error.getMessage should include ("deadlock state")
    }
  }

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