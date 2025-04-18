package io.equiv.eqfiddle.tool.benchmark

import io.equiv.eqfiddle.ccs.CCSSamples
import io.equiv.eqfiddle.ccs.Parser
import io.equiv.eqfiddle.ccs.Interpreter
import io.equiv.eqfiddle.ccs.Syntax
import io.equiv.eqfiddle.util.Interpreting
import io.equiv.eqfiddle.tool.model.NodeID
import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.ts.CSVTSLoader
import io.equiv.eqfiddle.spectroscopy.Spectroscopy
import io.equiv.eqfiddle.spectroscopy.StrongSpectroscopy
import io.equiv.eqfiddle.algo.transform.BuildQuotientSystem
import io.equiv.eqfiddle.algo.sigref.Bisimilarity
import io.equiv.eqfiddle.algo.sigref.BranchingBisimilarity
import io.equiv.eqfiddle.algo.transform.RemoveLittleBrothers

import scala.util.Random
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import io.equiv.eqfiddle.hml.HML

class VeryLargeTransitionSystems(
  algorithm: (WeakTransitionSystem[Int,Symbol,Unit]) => Spectroscopy[Int,Symbol,Unit,HML.Formula[Symbol]],
  config: Spectroscopy.Config,
  branchingBisim: Boolean = true
) {

  val vltsSamplesMedium = Seq(    
    "shared/src/test/assets/vlts/vasy_0_1.csv", //   289,   1224,  no tau,  2
    "shared/src/test/assets/vlts/vasy_1_4.csv", //  1183,   4464,    1213,  6
    "shared/src/test/assets/vlts/vasy_5_9.csv",
    "shared/src/test/assets/vlts/cwi_1_2.csv", //  1952,   2387,    2215, 26
    "shared/src/test/assets/vlts/cwi_3_14.csv",//  3996,  14552,   14551,  2
    "shared/src/test/assets/vlts/vasy_8_24.csv",
    "shared/src/test/assets/vlts/vasy_8_38.csv",
    "shared/src/test/assets/vlts/vasy_10_56.csv",
    "shared/src/test/assets/vlts/vasy_18_73.csv",
    "shared/src/test/assets/vlts/vasy_25_25.csv",
    // "shared/src/test/assets/other/peterson_mutex_weak.csv",
    "shared/src/test/assets/other/peterson_mutex.csv",
  )

  val easyExamples = List(10,0,1,2,4,5,6,9)
  val hardExamples = List(3,7) // excluded 8 = vasy_18_73, which never worked

  val tableOutput = true
  val littleBrotherElimination = false

  def arrowLabeling(o: Option[Syntax.Label]) = {
    Interpreting.fromOption(o.map(_.name) orElse(Some("")))
  }
  def nodeLabeling(o: Option[Syntax.NodeAnnotation]) = {
    Interpreting.fromOption(o.map(_.name) orElse(Some("")))
  }
  def actionIsOutput(a: String) = a.endsWith("!")
  def actionToInput(a: String): String = if (actionIsOutput(a)) actionToInput(a.dropRight(1)) else a

  def listMinimizations(fileName: String, outputMinimizationSizes: List[String]) = {

    print(fileName)

    val Some(loadedSystem) = new CSVTSLoader(fileName).result()

    output("States", loadedSystem.nodes.size.toString)
    output("Transitions", loadedSystem.step.size.toString)

    val minStartTime = System.nanoTime()
    val strongBisim = if (branchingBisim) {
      new BranchingBisimilarity(loadedSystem).computePartition()
    } else {
      new Bisimilarity(loadedSystem).computePartition()
    }
    val system = new BuildQuotientSystem(loadedSystem, strongBisim).build()
    printTiming(minStartTime, "Bisim minimization")

    output("Bisim minimized system", system.nodes.size.toString)
    val startTime = System.nanoTime()

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

    val result = algo.decideAll(comparedPairs, config.copy(computeFormulas = false, energyCap = 3, saveGameSize = true))
    printTiming(startTime, "Spectroscopy")

    output("Game positions", result.meta("game-positions"))
    output("Game moves", result.meta("game-moves"))

    val interestingNotions = result.spectrum.notions.filter(n => outputMinimizationSizes.contains(n.name))

    (interestingNotions zip
      result.toQuotients(interestingNotions, Math.min, comparedPairs)
    ).foreach { case (notion, quotient) =>
      val quotientSystem = new BuildQuotientSystem(system, quotient).build()
      if (littleBrotherElimination) {
        val minimizedSystem = new RemoveLittleBrothers(quotientSystem, { (p: Int, q: Int) =>
          result.foundPreorders(p, q).exists(eq => eq.obsNotion >= notion.obsNotion)
        }).build()
        output(notion.name, minimizedSystem.nodes.size.toString)
      } else {
        output(notion.name, quotientSystem.nodes.size.toString)
      }
    }
    if (tableOutput) {
      println()
    } else {
      println(
         (result.spectrum.notions.map(_.name) zip
           result.toQuotients(result.spectrum.notions, Math.min, comparedPairs).map(_.universeSize())).mkString("  "))
    }
  }

  def output(msg: String, data: String, suffix: String = "") = {
    if (tableOutput)
      print(", " + data)
    else
      println(msg + ": " + data + suffix)
  }

  def run(
      includeHardExamples: Boolean = false,
      shuffleExamples: Boolean = false,
      outputMinimizationSizes: List[String] = List("enabledness", "trace", "simulation"),
      timeoutTime: Long = 1000,
    ): Unit = {
    if (tableOutput) {
      println(("System, States, Transitions, Bisim pre-minimization time, Bisim pre-minimized size, Initial pairs, Spectroscopy time, Game positions, Game moves" +: outputMinimizationSizes).mkString(", "))
    }
    val exampleNumbers = if (includeHardExamples) easyExamples ++ hardExamples else easyExamples
    val orderedExamples = if (shuffleExamples) Random.shuffle(exampleNumbers) else exampleNumbers
    for (i <-orderedExamples) {
      val cancelPromise = Promise[Unit]
      val run = Future firstCompletedOf Seq(
        Future[Unit](listMinimizations(vltsSamplesMedium(i), outputMinimizationSizes)),
        cancelPromise.future
      )
      try {
        Await.result(
          run,
          timeoutTime milliseconds
        )
        run
      } catch {
        case e: TimeoutException =>
          println(s" [TIMEOUT after $timeoutTime ms]")
          cancelPromise.success(())
      }
    }
  }

  def printTiming(startTime: Long, text: String) = {
    output(s"$text took", ((System.nanoTime() - startTime) / 1000000).toString(), "ms")
  }
}
