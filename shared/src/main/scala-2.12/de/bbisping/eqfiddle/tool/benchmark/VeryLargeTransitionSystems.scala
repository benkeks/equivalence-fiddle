package de.bbisping.eqfiddle.tool.benchmark

import de.bbisping.eqfiddle.ccs.CCSSamples
import de.bbisping.eqfiddle.ccs.Parser
import de.bbisping.eqfiddle.ccs.Interpreter
import de.bbisping.eqfiddle.ccs.Syntax
import de.bbisping.eqfiddle.util.Interpreting
import de.bbisping.eqfiddle.tool.model.NodeID
import de.bbisping.eqfiddle.ts.WeakTransitionSystem
import de.bbisping.eqfiddle.ts.CSVTSLoader
import de.bbisping.eqfiddle.spectroscopy.EdgeSpectroscopy
import de.bbisping.eqfiddle.spectroscopy.FastSpectroscopy
import de.bbisping.eqfiddle.algo.transform.BuildQuotientSystem
import de.bbisping.eqfiddle.algo.sigref.Bisimilarity

class VeryLargeTransitionSystems(val useSpectro: Int = 0) {

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
    "shared/src/test/assets/vlts/vasy_25_25.csv"
  )

  def arrowLabeling(o: Option[Syntax.Label]) = {
    Interpreting.fromOption(o.map(_.name) orElse(Some("")))
  }
  def nodeLabeling(o: Option[Syntax.NodeDeclaration]) = {
    Interpreting.fromOption(o.map(_.name) orElse(Some("")))
  }
  def actionIsOutput(a: String) = a.endsWith("!")
  def actionToInput(a: String): String = if (actionIsOutput(a)) actionToInput(a.dropRight(1)) else a

  def listMinimizations(fileName: String) = {

    println(fileName)

    val Some(loadedSystem) = new CSVTSLoader(fileName).result()

    // val parser: Parser = new Parser(CCSSamples.ltbts1)
    // val parser.ParseSuccess(esDef, _) = parser.parse
    // val interpreter = new Interpreter(esDef, NodeID(_).hash, arrowLabeling, nodeLabeling, actionToInput, actionIsOutput)
    // val Interpreting.Success(is) = interpreter.result(new WeakTransitionSystem(_, _, Set()))
    // val loadedSystem = is.asInstanceOf[WeakTransitionSystem[Int, String, String]]

    val strongBisim = new Bisimilarity(loadedSystem).computePartition()
    val system = new BuildQuotientSystem(loadedSystem, strongBisim).build()

    println(s"Strong bisim minimized system: ${system.nodes.size}")
    val startTime = System.nanoTime()

    val states = system.nodes.toList

    val comparedPairs = for {
      n1i <- 0 until states.length
      n2j <- (n1i + 1) until states.length
    } yield (states(n1i), states(n2j))

    val algo = new FastSpectroscopy(system)

    val result = algo.compute(comparedPairs, computeFormulas = false)
    printTiming(startTime, "Spectroscopy")

    //println(result.printStats())
    println(
      (result.spectrum.notions.map(_.name) zip
      result.toQuotients(result.spectrum.notions, Math.min).map(_.universeSize())).mkString("  "))
  }


  def run(): Unit = {
    for (i <- List(0,1,2,4,5,6)) {
      listMinimizations(vltsSamplesMedium(i))
    }
  }

  def printTiming(startTime: Long, text: String) = {
    println(s"$text took ${(System.nanoTime() - startTime) / 1000000}ms")
  }
}
