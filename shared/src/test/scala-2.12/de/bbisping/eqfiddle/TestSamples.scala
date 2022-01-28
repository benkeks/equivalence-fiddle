package de.bbisping.eqfiddle

import de.bbisping.eqfiddle.ccs.CCSSamples
import de.bbisping.eqfiddle.ccs.Syntax
import de.bbisping.eqfiddle.ts.Samples
import de.bbisping.eqfiddle.ts.WeakTransitionSystem
import de.bbisping.eqfiddle.tool.model.NodeID
import de.bbisping.eqfiddle.ccs.Interpreter
import de.bbisping.eqfiddle.util.Interpreting
import de.bbisping.eqfiddle.ccs.Parser

object TestSamples  {
  
  def arrowLabeling(o: Option[Syntax.Label]) = {
    Interpreting.fromOption(o.map(_.name) orElse(Some("")))
  }
  def nodeLabeling(o: Option[Syntax.NodeDeclaration]) = {
    Interpreting.fromOption(o.map(_.name) orElse(Some("")))
  }

  val samples = for {
    Samples.Example(slug, name, src) <- CCSSamples.namedSamples
    parser: Parser = new Parser(src)
    parser.ParseSuccess(esDef, _) = parser.parse
    interpreter = new Interpreter(esDef, NodeID(_), arrowLabeling, nodeLabeling)
    Interpreting.Success(is) = interpreter.result(new WeakTransitionSystem(_, _, Set()))
  } yield (slug, is.asInstanceOf[WeakTransitionSystem[NodeID, String, String]])
  
}