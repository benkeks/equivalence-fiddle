package de.bbisping.coupledsim.algo

import de.bbisping.coupledsim.tool.control.Samples
import de.bbisping.coupledsim.tool.control.Structure
import de.bbisping.coupledsim.tool.model.NodeID
import de.bbisping.coupledsim.ts.Interpreter
import de.bbisping.coupledsim.util.Interpreting
import de.bbisping.coupledsim.ts.Parser

object TestSamples  {
  
  val samples = for {
    Samples.Example(slug, name, src) <- Samples.namedSamples
    val parser: Parser = new Parser(src)
    val parser.ParseSuccess(esDef, _) = parser.parse
    val interpreter = new Interpreter(esDef, NodeID(_), Structure.arrowAnnotator, Structure.nodeAnnotator)
    val Interpreting.Success(is) = interpreter.result(Structure.transitionSystemConstructor(_, _))
  } yield (slug, is.asInstanceOf[Structure.TSStructure])
  
}