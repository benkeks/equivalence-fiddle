package io.equiv.eqfiddle

import io.equiv.eqfiddle.spectroscopy.StrongSpectroscopy
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
}