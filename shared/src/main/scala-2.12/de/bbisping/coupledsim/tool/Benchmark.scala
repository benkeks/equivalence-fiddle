package de.bbisping.coupledsim.tool

import de.bbisping.coupledsim.hml.HMLGamePlayer
import de.bbisping.coupledsim.algo.AlgorithmLogging
import de.bbisping.coupledsim.algo.AlgorithmLogging.{LogRelation, LogRichRelation}
import de.bbisping.coupledsim.ccs.CCSSamples
import de.bbisping.coupledsim.ccs.Parser
import de.bbisping.coupledsim.ccs.Interpreter
import de.bbisping.coupledsim.ccs.Syntax
import de.bbisping.coupledsim.util.Interpreting
import de.bbisping.coupledsim.tool.model.NodeID
import de.bbisping.coupledsim.ts.WeakTransitionSystem


/**
 * Creates the distinguishing formulas that are used in the paper to illustrate the output of the algorithm.
 */
object Benchmark extends App {

  AlgorithmLogging.loggingActive = true
  AlgorithmLogging.debugLogActive = false

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
  def nodeLabeling(o: Option[Syntax.NodeDeclaration]) = {
    Interpreting.fromOption(o.map(_.name) orElse(Some("")))
  }
  def actionIsOutput(a: String) = a.endsWith("!")
  def actionToInput(a: String): String = if (actionIsOutput(a)) actionToInput(a.dropRight(1)) else a

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

    val algo = new HMLGamePlayer(ltbtsSystem)
    algo.computeSpectroscopy(n1, n2)

    val log = for {
      r <- algo.getReplay()
    } yield r()

    val distinctionPart = """under ([^<]*)(<br>)?""".r

    log.foreach {
      case LogRichRelation(_, comment) if comment.startsWith(n1s + " distinguished") =>
        val distinctions = distinctionPart.findAllMatchIn(comment).map(_.group(1))
        println(distinctions.mkString("\n"))
      case _ =>
    }
  }
}
