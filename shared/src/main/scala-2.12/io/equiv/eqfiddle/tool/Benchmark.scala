package io.equiv.eqfiddle.tool

import io.equiv.eqfiddle.algo.AlgorithmLogging
import io.equiv.eqfiddle.algo.AlgorithmLogging.{LogRelation, LogRichRelation}
import io.equiv.eqfiddle.tool.benchmark.LTBTSDistinctions
import io.equiv.eqfiddle.tool.benchmark.VeryLargeTransitionSystems

object Benchmark extends App {

  AlgorithmLogging.loggingActive = true
  AlgorithmLogging.debugLogActive = false

  val spectroscopyMode = if (args.contains("--formula-spectroscopy")) 1 else 0

  if (args.headOption == Some("formulas")) {
    new LTBTSDistinctions(spectroscopyMode).run()
  } else {
    val includeHardExamples = args.contains("--include-hard")
    val shuffleExamples = args.contains("--shuffle")
    val reducedSizes = args.contains("--reduced-sizes")

    if (reducedSizes) {
      new VeryLargeTransitionSystems(spectroscopyMode).run(
        includeHardExamples = includeHardExamples,
        shuffleExamples = shuffleExamples
      )
    } else {
      new VeryLargeTransitionSystems(spectroscopyMode).run(
        includeHardExamples = includeHardExamples,
        shuffleExamples = shuffleExamples,
        outputMinimizationSizes = List()
      )
    }
  }

}
