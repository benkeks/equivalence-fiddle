package io.equiv.eqfiddle.tool.benchmark

import io.equiv.eqfiddle.ccs.Syntax
import io.equiv.eqfiddle.util.Interpreting
import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.ts.CSVTSLoader
import io.equiv.eqfiddle.spectroscopy.Spectroscopy
import io.equiv.eqfiddle.algo.transform.BuildQuotientSystem
import io.equiv.eqfiddle.algo.sigref.Bisimilarity

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import io.equiv.eqfiddle.hml.HML

class Sizemark(
  algorithm: (WeakTransitionSystem[Int,Symbol,Unit]) => Spectroscopy[Int,Symbol,Unit,HML.Formula[Symbol]]
) {

  val configs = Seq(
    Spectroscopy.Config(useBisimMinimization = false, useSymmetryPruning = false, useCleverSpectroscopyGame = false, energyCap = 1),
    Spectroscopy.Config(useBisimMinimization = false, useSymmetryPruning = true, useCleverSpectroscopyGame = false, energyCap = 1),
    Spectroscopy.Config(useBisimMinimization = true, useSymmetryPruning = false, useCleverSpectroscopyGame = false, energyCap = 1),
    Spectroscopy.Config(useBisimMinimization = true, useSymmetryPruning = true, useCleverSpectroscopyGame = false, energyCap = 1),
    Spectroscopy.Config(useBisimMinimization = false, useSymmetryPruning = false, useCleverSpectroscopyGame = true, energyCap = 1),
    Spectroscopy.Config(useBisimMinimization = false, useSymmetryPruning = true, useCleverSpectroscopyGame = true, energyCap = 1),
    Spectroscopy.Config(useBisimMinimization = true, useSymmetryPruning = false, useCleverSpectroscopyGame = true, energyCap = 1),
    Spectroscopy.Config(useBisimMinimization = true, useSymmetryPruning = true, useCleverSpectroscopyGame = true, energyCap = 1),
  )

  val samples = Seq(    
    //"shared/src/test/assets/vlts/vasy_0_1.csv",
    "shared/src/test/assets/other/peterson_mutex_weak.csv",
  )

  val tableOutput = true

  def arrowLabeling(o: Option[Syntax.Label]) = {
    Interpreting.fromOption(o.map(_.name) orElse(Some("")))
  }
  def nodeLabeling(o: Option[Syntax.NodeAnnotation]) = {
    Interpreting.fromOption(o.map(_.name) orElse(Some("")))
  }

  def listGameSizes(fileName: String, config: Spectroscopy.Config) = {

    print(fileName)
    val Some(loadedSystem) = new CSVTSLoader(fileName).result()

    output("States", loadedSystem.nodes.size.toString)
    output("Transitions", loadedSystem.step.size.toString)

    val system = if (config.useBisimMinimization) {
      val strongBisim = new Bisimilarity(loadedSystem).computePartition()
      val system = new BuildQuotientSystem(loadedSystem, strongBisim).build()
      output("Bisim minimized system", system.nodes.size.toString)
      system
    } else {
      output("Non-minimized system", loadedSystem.nodes.size.toString)
      loadedSystem
    }
    val states = system.nodes.toList
    val stateGroups = states.groupBy(system.enabled(_))

    val comparedPairs = {
      for {
        group <- stateGroups.values
        n1i <- 0 until group.length
        n2j <- (n1i) until group.length
      } yield (group(n1i), group(n2j))
    }
    output("Initial pairs", comparedPairs.size.toString())

    val algo = algorithm(system)

    val result = algo.decideAll(comparedPairs, config)

    output("Game positions", result.meta("game-positions"))
    output("Game moves", result.meta("game-moves"))

    println()
  }

  def output(msg: String, data: String, suffix: String = "") = {
    if (tableOutput)
      print(", " + data)
    else
      println(msg + ": " + data + suffix)
  }

  def run(
      timeoutTime: Long = 1000
    ): Unit = {
    if (tableOutput) {
      println("System, States, Transitions, Bisim pre-minimized size, Initial pairs, Game positions, Game moves")
    }
    for (cfg <- configs) {
      println(s"${if (cfg.useSymmetryPruning) "do" else "no"}-symm-pruning, ${if (cfg.useBisimMinimization) "do" else "no"}-bisim-min, ${if (cfg.useCleverSpectroscopyGame) "do" else "no"}-cleverness")
      for (e <- samples) {
        try {
          Await.result(
            Future(listGameSizes(e, cfg)),
            timeoutTime milliseconds
          )
        } catch {
          case e: TimeoutException => println(s" [TIMEOUT after $timeoutTime ms]")
        }
      }
    }
  }
}
