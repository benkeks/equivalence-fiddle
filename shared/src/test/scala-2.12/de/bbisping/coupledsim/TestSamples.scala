package de.bbisping.coupledsim

import de.bbisping.coupledsim.ccs.CCSSamples
import de.bbisping.coupledsim.ccs.Syntax
import de.bbisping.coupledsim.ts.Samples
import de.bbisping.coupledsim.ts.WeakTransitionSystem
import de.bbisping.coupledsim.tool.model.NodeID
import de.bbisping.coupledsim.ccs.Interpreter
import de.bbisping.coupledsim.util.Interpreting
import de.bbisping.coupledsim.ccs.Parser

object TestSamples  {
  
  def arrowLabeling(o: Option[Syntax.Label]) = {
    Interpreting.fromOption(o.map(_.name) orElse(Some("")))
  }
  def nodeLabeling(o: Option[Syntax.NodeDeclaration]) = {
    Interpreting.fromOption(o.map(_.name) orElse(Some("")))
  }

  val samples = for {
    Samples.Example(slug, name, src) <- CCSSamples.namedSamples
    val parser: Parser = new Parser(src)
    val parser.ParseSuccess(esDef, _) = parser.parse
    val interpreter = new Interpreter(esDef, NodeID(_), arrowLabeling, nodeLabeling)
    val Interpreting.Success(is) = interpreter.result(new WeakTransitionSystem(_, _, Set()))
  } yield (slug, is.asInstanceOf[WeakTransitionSystem[NodeID, String, String]])
  
}