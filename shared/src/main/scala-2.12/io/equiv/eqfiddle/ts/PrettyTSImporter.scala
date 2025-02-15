package io.equiv.eqfiddle.ts

import io.equiv.eqfiddle.util.LabeledRelation
import io.equiv.eqfiddle.util.Interpreting
import io.equiv.eqfiddle.algo.AlgorithmLogging

class PrettyTSImporter(
    tsFileContent: String
  ) {
  
  val silentActions = Set(Symbol("tau"))
  
  def result(): Option[WeakTransitionSystem[Symbol, Symbol, Unit]] = {
    
    val parser = new Parser(tsFileContent)
    
    parser.parse match {
      case fail @ parser.ParseFail(msg, rest) =>
        AlgorithmLogging.debugLog(fail.toString())
        None
      case parser.ParseSuccess(ast, _) =>
        val interpretationResult =
          new Interpreter(ast, Symbol(_), (al => Interpreting.Success(Symbol(al.map(_.name).getOrElse("")))), (nl => Interpreting.Success(())))
            .result(new WeakTransitionSystem(_, _, silentActions.toSet))
      
        interpretationResult match {
          case problem: Interpreting.Problem =>
            AlgorithmLogging.debugLog("Interpretation failed: " + problem)
            None
          case Interpreting.Success(is: WeakTransitionSystem[Symbol, Symbol, Unit]) =>
            Some(is)
        }
    }
  }
}
