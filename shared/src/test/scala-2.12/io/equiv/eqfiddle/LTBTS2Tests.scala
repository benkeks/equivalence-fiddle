package io.equiv.eqfiddle

import io.equiv.eqfiddle.spectroscopy.EnergyWeakSpectroscopy
import io.equiv.eqfiddle.hml.ObservationClassEnergyWeak
import io.equiv.eqfiddle.hml.HennessyMilnerLogic


class LTBTS2Tests extends CSSSampleTests[ObservationClassEnergyWeak, HennessyMilnerLogic.Formula[String]] {

  override val spectrum = ObservationClassEnergyWeak.LTBTS

  val ltbtsSystem = TestSamples.samples.find(_._1 == "ltbts2").get._2

  val sampleNames: List[(String, String, List[String], List[String])] = List(
    ("P12", "P11", List("sr-delay-bisimulation", "eta-bisimulation"), List("branching-bisimulation")),
    ("P13", "P14", List("stable-bisimulation", "eta-bisimulation"), List("delay-bisimulation")),
    ("P15", "P16", List("contrasimulation", "stable-bisimulation", "eta-simulation"), List("unstable-readiness")),
    ("P21", "P22", List("ready-simulation", "s-ready-simulation", "eta-simulation"), List("impossible-future", "s-impossible-future")),
    ("P24", "P25", List("possible-future", "ready-simulation", "contrasimulation", "stable-bisimulation", "eta-simulation"), List("delay-bisimulation", "2-nested-simulation")),
    ("P34", "P33", List("sr-delay-bisimulation"), List("eta-simulation")),
    ("P44", "P45", List("readiness", "eta-simulation", "ready-simulation"), List("s-ready-simulation", "impossible-future", "s-impossible-future")),
    ("P51", "P52", List("readiness", "eta-simulation"), List("s-ready-simulation", "unstable-failure", "s-impossible-future", "sr-delay-bisimulation")),
    ("P53", "P54", List("sr-branching-bisimulation"), List()),
    ("P54", "P55", List("branching-bisimulation"), List("failure")),
    ("P55", "P56", List("eta-simulation", "2-nested-simulation", "stable-bisimulation"), List("contrasimulation", "sr-delay-bisimulation")),
    ("P57", "P56", List("branching-bisimulation"), List("failure")),
    ("P58", "P57", List("s-impossible-future", "eta-simulation", "2-nested-simulation"), List("contrasimulation", "stable-bisimulation", "readiness")),
  )

  runTest(ltbtsSystem, sampleNames, new EnergyWeakSpectroscopy(_), "(weak)")
}