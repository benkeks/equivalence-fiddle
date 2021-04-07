package de.bbisping.coupledsim.ccs

import de.bbisping.coupledsim.util.Parsing
import de.bbisping.coupledsim.util.Parsing.Pos
import de.bbisping.coupledsim.util.Parsing.Pos0

object Syntax {
  
  abstract sealed class Expression (val position: Pos) extends Parsing.AbstractExpression {
    def this() {
      this(Pos(0, 0))
    }
    
    def withPos(line: Int, col: Int): this.type = 
      withPos(Pos(line, col))
    
    def withPos(pos: Pos): this.type = (this match {
      case MetaDeclaration(k, v, p0) => MetaDeclaration(k, v, pos)
      case ProcessDeclaration(n, process, p0) => ProcessDeclaration(n, process, pos)
      case NodeDeclaration(n, aa, p0) => NodeDeclaration(n, aa, pos)
      case Label(n, p0) => Label(n, pos)
      case Prefix(l, proc, p0) => Prefix(l, proc, pos)
      case Restrict(lls, proc, p0) => Restrict(lls, proc, pos)
      case Parallel(procs, p0) => Parallel(procs, pos)
      case Choice(procs, p0) => Choice(procs, pos)
      case ProcessName(l, p0) => ProcessName(l, pos)
      case Definition(defs) => Definition(defs) // no effect!
    }).asInstanceOf[this.type]
    
    def prunePos: this.type = {
      (this match {
        case MetaDeclaration(k, v, p0) => MetaDeclaration(k, v, Pos0)
        case ProcessDeclaration(n, process, p0) => ProcessDeclaration(n, process, Pos0)
        case NodeDeclaration(n, aa, p0) => NodeDeclaration(n, aa, Pos0)
        case Label(n, p0) => Label(n, Pos0)
        case Prefix(l, proc, p0) => Prefix(l, proc.prunePos, Pos0)
        case Restrict(lls, proc, p0) => Restrict(lls, proc.prunePos, Pos0)
        case Parallel(procs, p0) => Parallel(procs map (_.prunePos), Pos0)
        case Choice(procs, p0) => Choice(procs map (_.prunePos), Pos0)
        case ProcessName(l, p0) => ProcessName(l, Pos0)
        case Definition(defs) => Definition(defs.map(_.prunePos))
      }).asInstanceOf[this.type]
    }
    
  }
    
  case class ProcessDeclaration(name: String, process: ProcessExpression, pos: Pos = Pos0) extends Expression(pos) {
    override def toString() = name + " = " + process.toString()
  }
  
  case class NodeDeclaration(val name: String, attribs: List[(String, String)], pos: Pos = Pos0) extends Expression(pos) {
  }
  
  case class Label(name: String, pos: Pos = Pos0) extends Expression(pos) {
    
    override def toString() = name

    def isOutput = name.endsWith("!")
    def toOutput = if (isOutput) this else Label(name + "!", pos)
    def toInput: Label = if (isOutput) Label(name.dropRight(1), pos).toInput else this

  }

  abstract sealed class ProcessExpression(pos: Pos) extends Expression(pos)
  
  case class Prefix(val l: Label, val proc: ProcessExpression, pos: Pos = Pos0) extends ProcessExpression(pos) {
    
    override def toString() = {
      val ps = proc.toString()
      if (l.isOutput) {
        l.toString + (if (ps.contains(" ")) "(" + ps + ")" else ps)
      } else {
        l.toString + "." + (if (ps.contains(" ")) "(" + ps + ")" else ps)
      }
    }
  }

  case class Choice(val procs: List[ProcessExpression], pos: Pos = Pos0) extends ProcessExpression(pos) {
    
    override def toString() = if (procs.isEmpty) {
      "0"
    } else {
      procs.mkString(" + ")
    }
  }

  case class Parallel(val procs: List[ProcessExpression], pos: Pos = Pos0) extends ProcessExpression(pos) {
    
    override def toString() = if (procs.isEmpty) {
      "0"
    } else {
      procs.mkString(" | ")
    }
  }

    case class Restrict(val names: List[Label], val proc: ProcessExpression, pos: Pos = Pos0) extends ProcessExpression(pos) {
    
    override def toString() = {
      val ps = proc.toString()
      (if (ps.contains(" ")) "(" + ps + ")" else ps) + names.mkString(" \\ {",",","}")
    }
  }

  case class ProcessName(val l: Label, pos: Pos = Pos0) extends ProcessExpression(pos) {
    
    override def toString() = l.toString
  }

  
  case class MetaDeclaration(key: String, value: String, pos: Pos = Pos0) extends Expression(pos) {

    
  }
  
  case class Definition(val defs: List[Expression]) extends Expression(Pos0) {
    
    val metaInfo = defs.collect { case md: MetaDeclaration => md }.groupBy(_.key)
    
    def getDeclaration(processID: String): Option[ProcessDeclaration] = defs.collectFirst {
      case pd @ ProcessDeclaration(n, _, _) if n == processID => pd
    }
  }

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