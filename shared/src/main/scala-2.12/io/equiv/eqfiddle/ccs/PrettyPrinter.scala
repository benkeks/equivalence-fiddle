package io.equiv.eqfiddle.ccs

import Syntax._

class PrettyPrinter {
  
  def showAttribute(kv: (String, String)): String = kv match {
    case (k, v) => if (v.isEmpty) {
      k
    } else {
      k + "=" + v
    }
  }

  def renderStringAtom(str: String): String = {
    if (str.forall(Parser.idChars(_)) && str.length() > 0 && !str.charAt(0).isDigit && str.charAt(0) != '-')
      str
    else
      "\"" + str + "\""
  }
  
  def show(e: Expression): String = e match {
    case MetaDeclaration(k, v, p0) =>
      val args = for {
        a <- v
        str = renderStringAtom(a)
      } yield str 
      "@" + k + " " + args.mkString(", ")
    case NodeAnnotation(n, aa, p0) =>
      val name = renderStringAtom(n)
      if (aa.isEmpty) {
        name
      } else {
        name + aa.map(showAttribute).mkString("(", ", ", ")")
      }
    case ProcessDefinition(name, proc, p0) =>
      name + " = " + show(proc)
    case Prefix(l, Choice(Nil, pos), p0) if l.isOutput =>
      show(l.toInput) + "!"
    case Prefix(l, proc, p0) if l.isOutput =>
      show(l.toInput) + "!" + show(proc)
    case Prefix(l, proc, p0) =>
      show(l) + "." + show(proc)
    case Choice(procs, pos) if procs.isEmpty =>
      "0"
    case Choice(procs, pos) =>
      procs.map(show(_)).mkString("(", " + ", ")")
    case Parallel(procs, pos) if procs.isEmpty =>
      "0"
    case Parallel(procs, pos) =>
      procs.map(show(_)).mkString("(", " | ", ")")
    case Restrict(names, proc, pos) =>
      show(proc) + " \\ " + names.map(show(_)).mkString("{", ", ", "}")
    case Renaming(renamings, proc, pos) =>
      val renamingString = if (renamings.forall(_._2.name == "τ")) {
        renamings.map(_._1).mkString(" \\hide {",",","}")
      } else {
        renamings.map { case (from, to) => s"$from -> $to" }.mkString("[", ",", "]")
      }
      show(proc) + renamingString
    case ProcessName(l, p0) =>
      show(l)
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
      val exprStr = show(e)
      " " * e.position.col + exprStr + (if (exprStr.endsWith("!")) "0" else "")
      // the trailing 0 ensures that a following process declaration is not accidentally merged with the last prefix
    } zip lineOffsets map { case (l, o) =>
      "\n"*o + l
    } mkString
  }
  
  def vsep(strs: Iterable[String]) = 
    strs.mkString("\n")

}