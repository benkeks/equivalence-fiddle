package de.bbisping.eqfiddle.util

trait Parsing {
  
  import Parsing._
  
  type Token
  
  def getTokenPosition(token: Token): Pos
  
  abstract sealed class Parsed[+A] {
    def map[B](f: A => B): Parsed[B]
    def flatMap[B](f: (A, List[Token]) => Parsed[B]): Parsed[B]
    def orElse[B >: A](f: List[Token] => Parsed[B]): Parsed[B]
    def or[B >: A](alternative: Parsed[B]): Parsed[B] = orElse(toks => alternative)
  }
  
  case class ParseFail[+A](msg: String, remainder: List[Token]) extends Parsed[A] {
    override def map[B](f: A => B): Parsed[B] = ParseFail[B](msg, remainder)
    override def flatMap[B](f: (A, List[Token]) => Parsed[B]): Parsed[B] = ParseFail[B](msg, remainder)
    override def orElse[B >: A](f: List[Token] => Parsed[B]): Parsed[B] = {
      f(remainder) match {
        case ParseFail(msg2, remainder2) =>
          if (remainder2.size < remainder.size) {
            ParseFail(msg2, remainder2)
          } else if (remainder2.size > remainder.size) {
            ParseFail(msg, remainder)
          } else {
            ParseFail(msg + "\n" + msg2, remainder)
          }
        case o => o
      }
    }
    
    def position = remainder.headOption.map(getTokenPosition).getOrElse(Pos0)
  }
  
  case class ParseSuccess[+A](get: A, remainder: List[Token]) extends Parsed[A] {
    override def map[B](f: A => B): Parsed[B] = ParseSuccess[B](f(get), remainder)
    override def flatMap[B](f: (A, List[Token]) => Parsed[B]): Parsed[B] = f(get, remainder)
    override def orElse[B >: A](f: List[Token] => Parsed[B]): Parsed[B] = this
  }
}

object Parsing {
  
  trait AbstractExpression {
    def position: Pos
  }
  
  case class Pos(val line: Int, val col: Int, endLine: Int = -1, endCol: Int = -1) {
    def incLine(lineCount: Int = 1) = Pos(line + lineCount, 0, (if (endLine > -1) endLine + lineCount else endLine), endCol)
    def incCol(columnCount: Int = 1) = Pos(line, col + columnCount, endLine , (if (endCol > -1) endCol + columnCount else endCol))
    
    def getEndLine = (if (endLine > -1) endLine else line)
    def getEndCol = (if (endCol > -1) endCol else col)
    
    def merge(other: Pos) = Pos(line, col, other.getEndLine, other.getEndCol)
  }
  
  object Pos0 extends Pos(0,0)
}