package io.equiv.eqfiddle.tool.benchmark

import io.equiv.eqfiddle.ccs.CCSSamples
import io.equiv.eqfiddle.ccs.Parser
import io.equiv.eqfiddle.ccs.Interpreter
import io.equiv.eqfiddle.ccs.Syntax
import io.equiv.eqfiddle.util.Interpreting
import io.equiv.eqfiddle.tool.model.NodeID
import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.ts.CSVTSLoader
import io.equiv.eqfiddle.spectroscopy.EdgeSpectroscopy
import io.equiv.eqfiddle.spectroscopy.FastSpectroscopy
import io.equiv.eqfiddle.algo.transform.BuildQuotientSystem
import io.equiv.eqfiddle.algo.sigref.Bisimilarity
import io.equiv.eqfiddle.algo.WeakTransitionSaturation

class VeryLargeTransitionSystems(val useSpectro: Int = 0) {

  val vltsSamplesMedium = Seq(
    "shared/src/test/assets/other/peterson_mutex.csv",
    "shared/src/test/assets/vlts/vasy_0_1.csv", //   289,   1224,  no tau,  2
    "shared/src/test/assets/vlts/vasy_1_4.csv", //  1183,   4464,    1213,  6
    "shared/src/test/assets/vlts/vasy_5_9.csv",
    "shared/src/test/assets/vlts/cwi_1_2.csv", //  1952,   2387,    2215, 26
    "shared/src/test/assets/vlts/cwi_3_14.csv",//  3996,  14552,   14551,  2
    "shared/src/test/assets/vlts/vasy_8_24.csv",
    "shared/src/test/assets/vlts/vasy_8_38.csv",
    "shared/src/test/assets/vlts/vasy_10_56.csv",
    "shared/src/test/assets/vlts/vasy_18_73.csv",
    "shared/src/test/assets/vlts/vasy_25_25.csv"
  )

  val easyExamples = List(0,1,2,4,5,6,9)
  val hardExamples = List(3,7,8)

  val tableOutput = true

  def arrowLabeling(o: Option[Syntax.Label]) = {
    Interpreting.fromOption(o.map(_.name) orElse(Some("")))
  }
  def nodeLabeling(o: Option[Syntax.NodeDeclaration]) = {
    Interpreting.fromOption(o.map(_.name) orElse(Some("")))
  }
  def actionIsOutput(a: String) = a.endsWith("!")
  def actionToInput(a: String): String = if (actionIsOutput(a)) actionToInput(a.dropRight(1)) else a

  def listMinimizations(fileName: String) = {

    print(fileName)

    val Some(loadedSystem) = new CSVTSLoader(fileName).result()

    output("States", loadedSystem.nodes.size.toString)
    output("Transitions", loadedSystem.step.size.toString)
    // val parser: Parser = new Parser(CCSSamples.ltbts1)
    // val parser.ParseSuccess(esDef, _) = parser.parse
    // val interpreter = new Interpreter(esDef, NodeID(_).hash, arrowLabeling, nodeLabeling, actionToInput, actionIsOutput)
    // val Interpreting.Success(is) = interpreter.result(new WeakTransitionSystem(_, _, Set()))
    // val loadedSystem = is.asInstanceOf[WeakTransitionSystem[Int, String, String]]

    val minStartTime = System.nanoTime()
    val weakSystem = new WeakTransitionSaturation(loadedSystem).compute()
    val strongBisim = new Bisimilarity(weakSystem).computePartition()
    val system = weakSystem //new BuildQuotientSystem(weakSystem, strongBisim).build()
    printTiming(minStartTime, "Bisim minimization")

    output("Strong bisim minimized system", system.nodes.size.toString)
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

    val algo = new FastSpectroscopy(system)

    val result = algo.compute(comparedPairs, computeFormulas = false, saveGameSize = true)
    printTiming(startTime, "Spectroscopy")

    output("Game positions", algo.gameSize._1.toString)
    output("Game moves", algo.gameSize._2.toString)

    if (tableOutput) {
      (result.spectrum.notions.map(_.name) zip
        result.toQuotients(result.spectrum.notions, Math.min, comparedPairs).map(_.universeSize())).foreach {case (notion, quotient) => output(notion, quotient.toString)}
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


  def run(): Unit = {
    for (i <- List(0)) {//easyExamples ++ hardExamples) {
      listMinimizations(vltsSamplesMedium(i))
    }
  }

  def printTiming(startTime: Long, text: String) = {
    output(s"$text took", ((System.nanoTime() - startTime) / 1000000).toString(), "ms")
  }
}
