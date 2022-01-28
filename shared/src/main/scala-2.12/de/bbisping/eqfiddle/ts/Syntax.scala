package de.bbisping.eqfiddle.ts

import de.bbisping.eqfiddle.util.Parsing
import de.bbisping.eqfiddle.util.Parsing.Pos
import de.bbisping.eqfiddle.util.Parsing.Pos0

object Syntax {
  
  abstract sealed class Expression (val position: Pos) extends Parsing.AbstractExpression {
    def this() {
      this(Pos(0, 0))
    }
    
    def withPos(line: Int, col: Int): Expression = 
      withPos(Pos(line, col))
    
    def withPos(pos: Pos): Expression = this match {
      case MetaDeclaration(k, v, p0) => MetaDeclaration(k, v, pos)
      case SingleNode(n, p0) => SingleNode(n, pos)
      case NodeDeclaration(n, aa, p0) => NodeDeclaration(n, aa, pos)
      case Label(n, p0) => Label(n, pos)
      case StepTo(e1, l, e2, p0) => StepTo(e1, l, e2, pos)
      case Definition(defs) => Definition(defs) // no effect!
    }
    
    def prunePos: Expression = {
      this match {
        case MetaDeclaration(k, v, p0) => MetaDeclaration(k, v, Pos0)
        case SingleNode(n, p0) => SingleNode(n, Pos0)
        case NodeDeclaration(n, aa, p0) => NodeDeclaration(n, aa, Pos0)
        case Label(n, p0) => Label(n, Pos0)
        case StepTo(e1, l, e2, p0) => StepTo(e1.prunePos.asInstanceOf[SingleNode], l, e2.prunePos.asInstanceOf[SingleNode], Pos0)
        case Definition(defs) => Definition(defs.map(_.prunePos))
      }
    }
    
    def nodeNames(): List[String]
  }
    
  case class SingleNode(name: String, pos: Pos = Pos0) extends Expression(pos) {
    
    override def nodeNames() = List(name)
    
  }
  
  case class NodeDeclaration(val name: String, attribs: List[(String, String)], pos: Pos = Pos0) extends Expression(pos) {
    
    override def nodeNames() = List(name)
    
  }
  
  case class Label(name: String, pos: Pos = Pos0) extends Expression(pos) {
    
    override def nodeNames() = List()
    
  }

  abstract sealed class Relation(pos: Pos) extends Expression(pos)
  
  case class StepTo(val e1: SingleNode, val l: Label, val e2: SingleNode, pos: Pos = Pos0) extends Relation(pos) {
    
    override def nodeNames() = e1.nodeNames ++ e2.nodeNames
    
  }
  
  case class MetaDeclaration(key: String, value: String, pos: Pos = Pos0) extends Expression(pos) {
    
    override def nodeNames() = List()
    
  }
  
  case class Definition(val defs: List[Expression]) extends Expression(Pos0) {
    
    val metaInfo = defs.collect{ case md: MetaDeclaration => md }.groupBy(_.key)
    
    override def nodeNames() = defs flatMap (_.nodeNames()) 
    
  }

  implicit def stringToSingleNode(name: String): SingleNode = SingleNode(name)
  implicit def intPairToPos(pos: (Int, Int)) = Pos(pos._1, pos._2)
  
  /** replaces all Pos0 by the position of the previous expression */
  def fillInPos(ex: List[Expression]): List[Expression] = ex match {
    case e1 :: e2 :: rest if e2.position == Pos0 => 
      val e2n = (e2 withPos e1.position)
      e1 :: fillInPos(e2n :: rest)
    case e1 :: rest =>
      e1 :: fillInPos(rest)
    case Nil =>
      Nil
  }
}