package io.equiv.eqfiddle.tool.benchmark

import io.equiv.eqfiddle.ccs.CCSSamples
import io.equiv.eqfiddle.hml.HML
import io.equiv.eqfiddle.tool.model.NodeID
import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.spectroscopy.Spectroscopy
import io.equiv.eqfiddle.spectroscopy.CCSSpectroscopy

/**
 * Checks each individual equivalence for each individual eq example of the strong spectrum.
 */
class LTBTSEquivalenceChecks(
  algorithm: (WeakTransitionSystem[NodeID,String,String]) => Spectroscopy[NodeID,String,String,HML.Formula[String]],
  config: Spectroscopy.Config
) {

  val examplePairs = List(
    ("L13", "R13"),
    ("L16", "R16"),
    ("L21", "R21"),
    ("R24", "L24"),
    ("L27", "R27"),
    ("R31", "L31"),
    ("L34", "R31"),
    ("L38", "R24"),
    ("L42", "R42"),
    ("L50", "R50")
  )

  def run(): Unit = {

    val ltbtsSystem = CCSSpectroscopy.loadLTS(CCSSamples.ltbts1)
      .fold(error => throw new IllegalArgumentException(error.toString), identity)

    val cfg = config copy (saveGameSize = true)

    for ((n1s, n2s) <- examplePairs) {
      val n1 = NodeID(n1s)
      val n2 = NodeID(n2s)
      val algo = algorithm(ltbtsSystem)

      println(
      s"""|{
          |  left:  $n1s,
          |  right: $n2s,
          |  comparisons: {""".stripMargin
      )
      for (notion <- algo.spectrum.notions) {
        val checkStartTime = System.nanoTime()
        val result = algo.checkIndividualPreorder(List((n1, n2)), notion.name, cfg)
        val checkTime = System.nanoTime() - checkStartTime
        println(
        s"""|    ${notion.name}: {
            |      result:    ${result.items.head.isMaintained},
            |      time_us:   ${checkTime / 1000},
            |      game_size: ${result.meta("game-positions").toInt + result.meta("game-moves").toInt}
            |    },""".stripMargin
        )
      }
      println("  }")
      println("},")
    }
  }
}
