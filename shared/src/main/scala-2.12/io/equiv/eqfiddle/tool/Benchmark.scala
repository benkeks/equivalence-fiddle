package io.equiv.eqfiddle.tool

import io.equiv.eqfiddle.algo.AlgorithmLogging
import io.equiv.eqfiddle.algo.AlgorithmLogging.{LogRelation, LogRichRelation}
import io.equiv.eqfiddle.tool.benchmark.LTBTSDistinctions
import io.equiv.eqfiddle.tool.benchmark.VeryLargeTransitionSystems

import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.spectroscopy.SpectroscopyInterface
import io.equiv.eqfiddle.spectroscopy.StrongSpectroscopy
import io.equiv.eqfiddle.hml.HennessyMilnerLogic

object Benchmark extends App {

  val usage =
    """Usage: [COMMAND] [OPTIONS]
      |
      |Performs various benchmarks on example systems
      |
      |Commands:
      |  help       Print this help
      |  formulas   Output example distinguishing formulas of the LTBT spectrum 1
      |  benchmark  Run benchmarks on VLTS
      |  sizemark   Compare sizes of spectroscopy games
      |
      |General options:
      | --unclever-spectroscopy  Use the exponentially-branching energy game (instead of the clever energy game)
      |
      |Benchmark options:
      | --shuffle        Perform benchmarks in random order
      | --include-hard   Also include hard cases (will take long / timeout on some examples)
      | --reduced-sizes  Prints sizes of transition systems reduced according to enabledness, traces, and sim (may take longer)
      | --timeout=T      Sets how many milliseconds each analysis may take the most (default: 500,000)
      |""".stripMargin

  val about =
    """The Linear-Time--Branching-Time Spectroscope is being developed by Benjamin Bisping at TU Berlin
      |More info: https://equiv.io/
      |Source:    https://github.com/benkeks/equivalence-fiddle
      |""".stripMargin

  val baseConfig = SpectroscopyInterface.SpectroscopyConfig(
    useCleverSpectroscopyGame = (args.contains("--unclever-spectroscopy")),
    useSymmetryPruning = true
  )

  AlgorithmLogging.loggingActive = true
  AlgorithmLogging.debugLogActive = false

  args.headOption match {
    case Some("formulas") =>
      new LTBTSDistinctions(new StrongSpectroscopy(_), baseConfig).run()
    case Some("help") =>
      println(usage)
    case Some("about") =>
      println(about)
    case Some("benchmark") =>
      val includeHardExamples = args.contains("--include-hard")
      val shuffleExamples = args.contains("--shuffle")
      val reducedSizes = args.contains("--reduced-sizes")
      val timeoutRegex = raw"--timeout=(\d+)".r
      val timeout = args.collectFirst { case timeoutRegex(timeout) => timeout.toInt }.getOrElse(500000)

      if (reducedSizes) {
        new VeryLargeTransitionSystems(new StrongSpectroscopy(_), baseConfig).run(
          includeHardExamples = includeHardExamples,
          shuffleExamples = shuffleExamples,
          timeoutTime = timeout
        )
      } else {
        new VeryLargeTransitionSystems(new StrongSpectroscopy(_), baseConfig).run(
          includeHardExamples = includeHardExamples,
          shuffleExamples = shuffleExamples,
          outputMinimizationSizes = List(),
          timeoutTime = timeout
        )
      }
    case Some("sizemark") =>
      
    case _ =>
      println("Usage: [COMMAND] [OPTIONS]\n  Run `help` for details.")
  }

}
