package io.equiv.eqfiddle.tool

import io.equiv.eqfiddle.algo.AlgorithmLogging
import io.equiv.eqfiddle.algo.AlgorithmLogging.{LogRelation, LogRichRelation}
import io.equiv.eqfiddle.tool.benchmark.LTBTSDistinctions
import io.equiv.eqfiddle.tool.benchmark.VeryLargeTransitionSystems

object Benchmark extends App {

  AlgorithmLogging.loggingActive = true
  AlgorithmLogging.debugLogActive = false

  //new LTBTSDistinctions(0).run()
  new VeryLargeTransitionSystems(0).run()
}
