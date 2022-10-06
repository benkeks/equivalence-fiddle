package de.bbisping.eqfiddle.tool

import de.bbisping.eqfiddle.algo.AlgorithmLogging
import de.bbisping.eqfiddle.algo.AlgorithmLogging.{LogRelation, LogRichRelation}
import de.bbisping.eqfiddle.tool.benchmark.LTBTSDistinctions
import de.bbisping.eqfiddle.tool.benchmark.VeryLargeTransitionSystems

object Benchmark extends App {

  AlgorithmLogging.loggingActive = true
  AlgorithmLogging.debugLogActive = false

  //new LTBTSDistinctions(0).run()
  new VeryLargeTransitionSystems(0).run()
}
