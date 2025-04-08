package io.equiv.eqfiddle

import io.equiv.eqfiddle.spectroscopy.WeakSpectroscopy
import io.equiv.eqfiddle.hml.WeakObservationNotion
import io.equiv.eqfiddle.hml.HML
import io.equiv.eqfiddle.spectroscopy.Spectroscopy

class LTBTS2Tests extends CSSSampleTests[WeakObservationNotion, HML.Formula[String]] {

  override val spectrum = WeakObservationNotion.LTBTS

  val ltbtsSystem = TestSamples.samples.find(_._1 == "ltbts2").get._2

  val configs = Seq(
    "unoptimized" -> Spectroscopy.Config(useSymmetryPruning = false, useBranchingSpectroscopyGame = false, useCleverSpectroscopyGame = false, energyCap = 3),
    "unoptimized-formulas" -> Spectroscopy.Config(useSymmetryPruning = false, useBranchingSpectroscopyGame = false, useCleverSpectroscopyGame = false, computeFormulas = true),
    "branching-formulas" -> Spectroscopy.Config(useSymmetryPruning = false, useBranchingSpectroscopyGame = true, useCleverSpectroscopyGame = false, computeFormulas = true),
    "symmetry-pruned" -> Spectroscopy.Config(useSymmetryPruning = true, useBranchingSpectroscopyGame = false, useCleverSpectroscopyGame = false, energyCap = 3),
    "clever-spectro" -> Spectroscopy.Config(useSymmetryPruning = false, useBranchingSpectroscopyGame = false, useCleverSpectroscopyGame = true, energyCap = 3),
    "symmetry-pruned-clever" -> Spectroscopy.Config(useSymmetryPruning = true, useBranchingSpectroscopyGame = false, useCleverSpectroscopyGame = true, energyCap = 3),
    "branching" -> Spectroscopy.Config(useSymmetryPruning = false, useCleverSpectroscopyGame = false, energyCap = 3),
    "symmetry-pruned-branching" -> Spectroscopy.Config(useSymmetryPruning = true, useCleverSpectroscopyGame = false, energyCap = 3),
    "clever-spectro-branching" -> Spectroscopy.Config(useSymmetryPruning = false, useCleverSpectroscopyGame = true, energyCap = 3),
    "symmetry-pruned-clever-branching" -> Spectroscopy.Config(useSymmetryPruning = true, useCleverSpectroscopyGame = true, energyCap = 3),
  )
  
  val sampleNames: List[(String, String, List[String], List[String])] = List(
    ("P12", "P11", List("sr-delay-bisimulation", "eta-bisimulation"), List("branching-bisimulation")),
    ("P13", "P14", List("stable-bisimulation", "eta-bisimulation"), List("delay-bisimulation")),
    ("P15", "P16", List("contrasimulation", "stable-bisimulation", "eta-simulation"), List("weak-readiness")),
    ("P21", "P22", List("weak-ready-simulation", "s-ready-simulation", "eta-simulation"), List("weak-impossible-future", "s-impossible-future")),
    ("P24", "P25", List("weak-possible-future", "weak-ready-simulation", "contrasimulation", "stable-bisimulation", "eta-simulation"), List("delay-bisimulation", "2-nested-weak-simulation")),
    ("P34", "P33", List("sr-delay-bisimulation"), List("eta-simulation")),
    ("P44", "P45", List("stable-readiness", "eta-simulation", "weak-ready-simulation"), List("stable-failure-trace", "weak-impossible-future", "s-impossible-future", "stable-simulation")),
    ("P51", "P52", List("eta-simulation", "s-ready-simulation"), List("weak-failure", "s-impossible-future", "sr-delay-bisimulation")),
    ("P53", "P54", List("sr-branching-bisimulation"), List()),
    ("P54", "P55", List("branching-bisimulation"), List("stable-failure", "stable-simulation")),
    ("P55", "P56", List("eta-simulation", "2-nested-weak-simulation", "stable-bisimulation"), List("contrasimulation", "sr-delay-bisimulation")),
    ("P57", "P56", List("branching-bisimulation"), List("stable-failure", "stable-simulation")),
    ("P58", "P57", List("s-impossible-future", "eta-simulation", "2-nested-weak-simulation"), List("contrasimulation", "stable-simulation", "stable-revival")),
  )
  for ((cfgName, cfg) <- configs) {
    runTest(ltbtsSystem, sampleNames, new WeakSpectroscopy(_), s"(weak, $cfgName)", cfg)
  }
}