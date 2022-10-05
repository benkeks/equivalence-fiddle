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

class VeryLargeTransitionSystems(val useSpectro: Int = 0) {

  val vltsSamplesSmall = Seq(
    "shared/src/test/assets/vlts/vasy_0_1.csv" //   289,   1224,  no tau,  2
  )
    
  val vltsSamplesMedium = Seq(
    "shared/src/test/assets/vlts/vasy_1_4.csv", //  1183,   4464,    1213,  6
    "shared/src/test/assets/vlts/vasy_5_9.csv",
    "shared/src/test/assets/vlts/cwi_1_2.csv", //  1952,   2387,    2215, 26
    "shared/src/test/assets/vlts/cwi_3_14.csv",//  3996,  14552,   14551,  2
    "shared/src/test/assets/vlts/vasy_8_24.csv",
    "shared/src/test/assets/vlts/vasy_8_38.csv",
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

  def run(): Unit = {

    val Some(system) = new CSVTSLoader(vltsSamplesSmall(0)).result()

    val states = system.nodes.toList

    val comparedPairs = for {
      n1i <- 0 until states.length
      n2j <- (n1i + 1) until states.length
    } yield (states(n1i), states(n2j))

    val algo = new FastSpectroscopy(system)

    val result = algo.compute(comparedPairs, computeFormulas = false)

    println(result.printStats())
  }
}
