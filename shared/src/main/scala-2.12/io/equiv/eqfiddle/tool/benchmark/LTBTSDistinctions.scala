package io.equiv.eqfiddle.tool.benchmark

import io.equiv.eqfiddle.ccs.CCSSamples
import io.equiv.eqfiddle.ccs.Parser
import io.equiv.eqfiddle.ccs.Interpreter
import io.equiv.eqfiddle.ccs.Syntax
import io.equiv.eqfiddle.util.Interpreting
import io.equiv.eqfiddle.tool.model.NodeID
import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.spectroscopy.SpectroscopyInterface
import io.equiv.eqfiddle.spectroscopy.StrongSpectroscopy
import io.equiv.eqfiddle.hml.HennessyMilnerLogic


/**
 * Creates the distinguishing formulas that are used in the paper / the classic LTBTS to illustrate the output of the algorithm.
 */
class LTBTSDistinctions(
  algorithm: (WeakTransitionSystem[NodeID,String,String]) => SpectroscopyInterface[NodeID,String,String,HennessyMilnerLogic.Formula[String]],
  config: SpectroscopyInterface.SpectroscopyConfig
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

  def arrowLabeling(o: Option[Syntax.Label]) = {
    Interpreting.fromOption(o.map(_.name) orElse(Some("")))
  }
  def nodeLabeling(o: Option[Syntax.NodeAnnotation]) = {
    Interpreting.fromOption(o.map(_.name) orElse(Some("")))
  }
  def actionIsOutput(a: String) = a.endsWith("!")
  def actionToInput(a: String): String = if (actionIsOutput(a)) actionToInput(a.dropRight(1)) else a

  def run(): Unit = {

    val parser: Parser = new Parser(CCSSamples.ltbts1)
    val parser.ParseSuccess(esDef, _) = parser.parse
    val interpreter = new Interpreter(esDef, NodeID(_), arrowLabeling, nodeLabeling, actionToInput, actionIsOutput)
    val Interpreting.Success(is) = interpreter.result(new WeakTransitionSystem(_, _, Set()))
    val ltbtsSystem = is.asInstanceOf[WeakTransitionSystem[NodeID, String, String]]

    for ((n1s, n2s) <- examplePairs) {
      val n1 = NodeID(n1s)
      val n2 = NodeID(n2s)

      println("--------------------")
      esDef.getDeclaration(n1s) map (_.process) foreach (println(_))
      esDef.getDeclaration(n2s) map (_.process) foreach (println(_))

      val result = {
        val algo = algorithm(ltbtsSystem)
        algo.compute(List((n1, n2)), SpectroscopyInterface.SpectroscopyConfig(computeFormulas = true))
      }

      for {
        res <- result.relationItems
        if res.left == n1 && res.right == n2
        (formula, obsClass, notions) <- res.distinctions
      } {
        println(formula.toString() + " ∈ " + notions.map(_.name).mkString(" ∩ "))
      }
    }
  }
}
