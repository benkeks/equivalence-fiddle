package io.equiv.eqfiddle

import io.equiv.eqfiddle.spectroscopy.StrongSpectroscopy
import io.equiv.eqfiddle.hml.ObservationClassStrong
import io.equiv.eqfiddle.hml.HennessyMilnerLogic


class LTBTSTests extends CSSSampleTests[ObservationClassStrong, HennessyMilnerLogic.Formula[String]] {

  override val spectrum = ObservationClassStrong.LTBTS

  val ltbtsSystem = TestSamples.samples.find(_._1 == "ltbts1").get._2

  val sampleNames: List[(String, String, List[String], List[String])] = List(
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
  )

  runTest(ltbtsSystem, sampleNames, new StrongSpectroscopy(_), "(energy-labeled)")
}