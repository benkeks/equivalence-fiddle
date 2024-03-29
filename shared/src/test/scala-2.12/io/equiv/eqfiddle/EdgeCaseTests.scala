package io.equiv.eqfiddle

import io.equiv.eqfiddle.spectroscopy.PositionalSpectroscopy
import io.equiv.eqfiddle.spectroscopy.EdgeSpectroscopy
import io.equiv.eqfiddle.spectroscopy.FastSpectroscopy
import io.equiv.eqfiddle.hml.ObservationClassStrong
import io.equiv.eqfiddle.hml.HennessyMilnerLogic

class EdgeCaseTests extends CSSSampleTests[ObservationClassStrong, HennessyMilnerLogic.Formula[String]] {
  override val spectrum = ObservationClassStrong.LTBTS
  
  val reviewSystem = TestSamples.samples.find(_._1 == "review-counterexamples").get._2

  val sampleNames: List[(String, String, List[String], List[String])] = List(
    ("L1", "R1", List("readiness", "simulation"), List("failure-trace", "impossible-future")),
    ("L2", "R2", List("readiness", "simulation"), List("failure-trace", "impossible-future"))
  )

  runTest(reviewSystem, sampleNames, new PositionalSpectroscopy(_), "(positional)")
  runTest(reviewSystem, sampleNames, new EdgeSpectroscopy(_), "(edge-labeled)")
  runTest(reviewSystem, sampleNames, new FastSpectroscopy(_), "(energy-labeled)")
}