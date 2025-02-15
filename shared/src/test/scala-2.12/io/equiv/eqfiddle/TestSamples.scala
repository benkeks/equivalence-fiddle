package io.equiv.eqfiddle

import io.equiv.eqfiddle.ccs.CCSSamples
import io.equiv.eqfiddle.ccs.Syntax
import io.equiv.eqfiddle.ts.Example
import io.equiv.eqfiddle.ts.WeakTransitionSystem
import io.equiv.eqfiddle.tool.model.NodeID
import io.equiv.eqfiddle.ccs.Interpreter
import io.equiv.eqfiddle.util.Interpreting
import io.equiv.eqfiddle.ccs.Parser

object TestSamples {
  
  def arrowLabeling(o: Option[Syntax.Label]) = {
    Interpreting.fromOption(o.map(_.name) orElse(Some("")))
  }
  def nodeLabeling(o: Option[Syntax.NodeDeclaration]) = {
    Interpreting.fromOption(o.map(_.name) orElse(Some("")))
  }
  def actionStrIsOutput(a: String) = a.endsWith("!")
  def actionStrToInput(a: String): String = if (actionStrIsOutput(a)) actionStrToInput(a.dropRight(1)) else a

  val samples = for {
    Example(slug, name, src) <- CCSSamples.namedSamples
    parser: Parser = new Parser(src)
    parser.ParseSuccess(esDef, _) = parser.parse
    interpreter = new Interpreter(esDef, NodeID(_), arrowLabeling, nodeLabeling, actionStrToInput, actionStrIsOutput)
    Interpreting.Success(is) = interpreter.result(new WeakTransitionSystem(_, _, Set("tau")))
  } yield (slug, is.asInstanceOf[WeakTransitionSystem[NodeID, String, String]])
  
}