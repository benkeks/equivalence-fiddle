package io.equiv.eqfiddle.tool.control

import scala.scalajs.js.Date
import scala.util.Try
import io.equiv.eqfiddle.tool.arch.Action
import io.equiv.eqfiddle.tool.arch.Control
import io.equiv.eqfiddle.ccs.Syntax
import io.equiv.eqfiddle.ccs
import io.equiv.eqfiddle.ccs.CCSSamples
import io.equiv.eqfiddle.ts.Samples
import io.equiv.eqfiddle.tool.control.Structure.NodeLabel
import io.equiv.eqfiddle.util.Parsing

class Source(val main: Control) extends ModelComponent {
  
  private var source: String = ""
  private var ast: Syntax.Definition = null
  private var problems: List[Source.Problem] = List()
  
  private val samples = CCSSamples.namedSamples
  
  def init() {
    broadcast(Source.ExamplesChange(samples))
  }
  
  def changeCode(code: String) = {
    source = code
    problems = List()
    
    val beginParse = Date.now
    val parser = new ccs.Parser(code)
    
    parser.parse match {
      case parser.ParseSuccess(ccsDef, _) =>
        println("Parsing took: " + (Date.now - beginParse) + "ms.")
        broadcast(Source.ProblemChange(source, List()))
        setAst(ccsDef, updateSource = false)
        
      case fail @ parser.ParseFail(msg, rest) =>
        val idx = fail.position
        markProblems(List(Source.Problem(msg, idx.line + 1, idx.col + 1)))
    }
  }
    
  def markProblems(errs: List[Source.Problem]) = {
    problems ++= errs
    broadcast(Source.ProblemChange(source, problems))
  }
  
  private def checkNodeName(name: String) = {
    true // we're liberal, now!
    //name != "" &&
    //name.forall(c => c.isLetterOrDigit || c == '_')
  }
  
  def updateEventDeclarationAttributes(updates: List[(String, NodeLabel)]): Boolean = {
    val names = updates.map(_._1)
    if (names.forall(checkNodeName(_))) {
      val affectedAst = ast
      val oldDecls = affectedAst.defs collect { case n: Syntax.NodeDeclaration => n }
      
      val newDecls = updates.map { case (nodeName, annotations) =>
        val oldDecl = oldDecls.find(d => d.name == nodeName)
        val pos = oldDecl.map(_.pos).getOrElse(Parsing.Pos0)
        val attribs = oldDecl.map(_.attribs).getOrElse(List()).toMap ++ annotations.toStringPairList
        (oldDecl, Syntax.NodeDeclaration(nodeName, attribs.toList, pos))
      }
      
      // group by oldDecls and project them away
      val newVsOldDecls = newDecls.groupBy(_._1).mapValues(_.map(_._2))
      
      val defsUpdatedOld = {
        affectedAst.defs.map {
          case d: Syntax.NodeDeclaration =>
            newVsOldDecls.get(Some(d)) match {
              case Some(dn :: _) =>
                dn
              case None =>
                d
              case _ =>
                ??? // from the context we know that empty groups are not possible
            }
          case o => o
        }
      }
      
      // enqueue updates that dont belong to an old declaration
      val (astBefore, astAfter) = defsUpdatedOld.splitAt(1 + defsUpdatedOld.lastIndexWhere(_.isInstanceOf[Syntax.NodeDeclaration]))
      val newDefs = astBefore ::: newVsOldDecls.getOrElse(None, List()) ::: astAfter 
      
      val newAst = Syntax.Definition(
          Syntax.fillInPos(newDefs))
      setAst(newAst)
      
      true
    } else {
      false
    }
  }
  
  def setAst(newAst: Syntax.Definition, updateSource: Boolean = true) {
    ast = newAst
    if (updateSource) {
      source = new ccs.PrettyPrinter().showDefinition(newAst).toString
    }
    broadcast(Source.SourceChange(source, ast))
  }
  
  override def notify(c: ModelComponent.Change) = c match {
    case Structure.StructureChangeFailed(p) =>
      println("structure problems " + p)
      val problems = p.expr.map { e => Source.Problem(p.msg, e.position.line + 1, e.position.col) } 
      markProblems(problems)
    case _ => 
  }
  
}

object Source {
  case class Problem(msg: String, line: Int, col: Int)
  
  abstract sealed class SourceAction extends Action {
    override def implement(target: ModelComponent): Boolean = target match {
      case s: Source => 
        implementSource(s)
      case _ =>
        false
    }
    
    def implementSource(source: Source): Boolean
  }
  
  case class LoadDefinition(code: String) extends SourceAction {
    override def implementSource(source: Source) = {
      source.changeCode(code)
      true
    }
  }
  
  case class UpdateEventDeclarationAttributes(updates: List[(String, NodeLabel)]) extends SourceAction {
    override def implementSource(source: Source) = {
      source.updateEventDeclarationAttributes(updates)
    }
  }
  
  case class SourceChange(source: String, ast: Syntax.Definition) extends ModelComponent.Change {  
    override def toString() = "SourceChange( ... )" 
  }
  
  case class ProblemChange(source: String, errs: List[Problem]) extends ModelComponent.Change  {  
    override def toString() = "ProblemChange( ..., " + errs.toString() + " )" 
  }
  
  case class ExamplesChange(samples: List[Samples.Example]) extends ModelComponent.Change {
    override def toString = "ExamplesChange"
  }
}