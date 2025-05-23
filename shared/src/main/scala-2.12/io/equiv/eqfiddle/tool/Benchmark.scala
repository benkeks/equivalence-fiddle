package io.equiv.eqfiddle.tool

import io.equiv.eqfiddle.algo.AlgorithmLogging
import io.equiv.eqfiddle.algo.AlgorithmLogging.{LogRelation, LogRichRelation}
import io.equiv.eqfiddle.tool.benchmark.LTBTSDistinctions
import io.equiv.eqfiddle.tool.benchmark.LTBTSEquivalenceChecks
import io.equiv.eqfiddle.tool.benchmark.VeryLargeTransitionSystems
import io.equiv.eqfiddle.tool.model.NodeID

import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.spectroscopy.Spectroscopy
import io.equiv.eqfiddle.spectroscopy.StrongSpectroscopy
import io.equiv.eqfiddle.spectroscopy.WeakSpectroscopy
import io.equiv.eqfiddle.hml.HML
import io.equiv.eqfiddle.tool.benchmark.Sizemark

object Benchmark extends App {

  val usage =
    """Usage: [COMMAND] [OPTIONS]
      |
      |Performs various benchmarks on example systems
      |
      |Commands:
      |  help       Print this help
      |  formulas   Output example distinguishing formulas of the LTBT spectrum 1
      |  eqchecks   Check each individual notion of the strong spectrum on the LTBTS examples
      |  benchmark  Run benchmarks on VLTS
      |  sizemark   Compare sizes of spectroscopy games
      |
      |General options:
      | --unclever-spectroscopy  Use the exponentially-branching energy game (instead of the clever energy game)
      | --strong-game  Use the strong spectroscopy game (instead of the weak variant)
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

  val baseConfig = Spectroscopy.Config(
    useCleverSpectroscopyGame = !(args.contains("--unclever-spectroscopy")),
    useBranchingSpectroscopyGame = !(args.contains("--exponential-branching-conj")),
    useSymmetryPruning = true
  )

  val algoVLTS: (WeakTransitionSystem[Int,Symbol,Unit]) => Spectroscopy[Int,Symbol,Unit,HML.Formula[Symbol]] =
    if (args.contains("--strong-game")) {
      new StrongSpectroscopy(_)
    } else {
      new WeakSpectroscopy(_)
  }
  val algoCCS: (WeakTransitionSystem[NodeID,String,String]) => Spectroscopy[NodeID,String,String,HML.Formula[String]] =
    if (args.contains("--strong-game")) {
      new StrongSpectroscopy(_)
    } else {
      new WeakSpectroscopy(_)
  }

  val timeoutRegex = raw"--timeout=(\d+)".r
  val timeout = args.collectFirst { case timeoutRegex(timeout) => timeout.toInt }.getOrElse(500000)

  AlgorithmLogging.loggingActive = true
  AlgorithmLogging.debugLogActive = false

  args.headOption match {
    case Some("help") =>
      println(usage)
    case Some("about") =>
      println(about)
    case Some("formulas") =>
      new LTBTSDistinctions(algoCCS, baseConfig).run()
    case Some("eqchecks") =>
      new LTBTSEquivalenceChecks(algoCCS, baseConfig).run()
    case Some("benchmark") =>
      val includeHardExamples = args.contains("--include-hard")
      val shuffleExamples = args.contains("--shuffle")
      val reducedSizes = args.contains("--reduced-sizes")
      val branchingBisim = !args.contains("--strong-game")

      if (reducedSizes) {
        val outputNotions = if (args.contains("--strong-game"))
          List(
            "enabledness",
            "trace",
            "simulation",
          )
        else
          List(
            "weak-enabledness",
            "weak-trace",
            "weak-simulation",
            "sr-branching-bisimulation",
          )
        new VeryLargeTransitionSystems(algoVLTS, baseConfig, branchingBisim = branchingBisim).run(
          includeHardExamples = includeHardExamples,
          shuffleExamples = shuffleExamples,
          timeoutTime = timeout,
          outputMinimizationSizes = outputNotions,
        )
      } else {
        new VeryLargeTransitionSystems(algoVLTS, baseConfig, branchingBisim = branchingBisim).run(
          includeHardExamples = includeHardExamples,
          shuffleExamples = shuffleExamples,
          outputMinimizationSizes = List(),
          timeoutTime = timeout,
        )
      }
    case Some("sizemark") =>
      new Sizemark(algoVLTS).run(
        timeoutTime = timeout
      )
    case _ =>
      println("Usage: [COMMAND] [OPTIONS]\n  Run `help` for details.")
  }

}
