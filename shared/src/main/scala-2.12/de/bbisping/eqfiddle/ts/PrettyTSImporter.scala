package de.bbisping.eqfiddle.ts

import de.bbisping.eqfiddle.util.LabeledRelation
import de.bbisping.eqfiddle.util.Interpreting


class PrettyTSImporter(
    tsFileContent: String
  ) {
  
  val silentActions = Set(Symbol("tau"))
  
  def result(): Option[WeakTransitionSystem[Symbol, Symbol, Unit]] = {
    
    val parser = new Parser(tsFileContent)
    
    parser.parse match {
      case fail @ parser.ParseFail(msg, rest) =>
        println(fail)
        None
      case parser.ParseSuccess(ast, _) =>
        val interpretationResult =
          new Interpreter(ast, Symbol(_), (al => Interpreting.Success(Symbol(al.map(_.name).getOrElse("")))), (nl => Interpreting.Success(())))
            .result(new WeakTransitionSystem(_, _, silentActions.toSet))
      
        interpretationResult match {
          case problem: Interpreting.Problem =>
            println("Interpretation failed: " + problem)
            None
          case Interpreting.Success(is: WeakTransitionSystem[Symbol, Symbol, Unit]) =>
            Some(is)
        }
    }
  }
}
