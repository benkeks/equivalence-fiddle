package de.bbisping.eqfiddle.ts

import Syntax._

class PrettyPrinter {
  
  def showAttribute(kv: (String, String)): String = kv match {
    case (k, v) => if (v.isEmpty) {
      k
    } else {
      k + "=" + v
    }
  }
  
  def show(e: Expression): String = e match {
    case MetaDeclaration(k, v, p0) =>
      "@" + k + (if (v != "") " \"" + v + "\" " else "")
    case SingleNode(n, p0) =>
      n
    case NodeDeclaration(n, aa, p0) =>
      if (aa.isEmpty) {
        n
      } else {
        n + aa.map(showAttribute).mkString("(", ", ", ")")
      }
    case StepTo(e1, l, e2, p0) if l.name.isEmpty() =>
      show(e1) + " --> " + show(e2)
    case StepTo(e1, l, e2, p0) =>
      show(e1) + " |-" + show(l) + "-> " + show(e2)
    case Label(l, p0) =>
      l
    case d: Definition =>
      showDefinition(d)
  }
  
  def showDefinition(d: Definition) = {
    val lineOffsets = 0 :: d.defs sliding 2 map {
      case List(a: Expression, b: Expression) =>
        (b.position.line - a.position.getEndLine) max 1
      case _ => 0
    } toList
    
    d.defs.map { e =>
      " " * e.position.col + show(e)
    } zip lineOffsets map { case (l, o) =>
      "\n"*o + l
    } mkString
  }
  
  def vsep(strs: Iterable[String]) = 
    strs.mkString("\n")

}