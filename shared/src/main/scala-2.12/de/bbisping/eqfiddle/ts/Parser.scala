package de.bbisping.eqfiddle.ts

import de.bbisping.eqfiddle.util.Parsing
import de.bbisping.eqfiddle.ts.Syntax._

class Parser(val input: String) extends Parsing {
  
  val idChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789".toSet
  
  import Parsing._
  
  abstract sealed class Token(val position: Pos)
  case class Identifier(name: String, pos: Pos) extends Token(pos)
  case class LiteralNumber(num: String, pos: Pos) extends Token(pos)
  case class LiteralString(string: String, pos: Pos) extends Token(pos)
  case class RoundBracketOpen(pos: Pos) extends Token(pos)
  case class RoundBracketClose(pos: Pos) extends Token(pos)
  case class CurlyBracketClose(pos: Pos) extends Token(pos)
  case class Comma(pos: Pos) extends Token(pos)
  case class Equals(pos: Pos) extends Token(pos)
  case class RelationArrow(pos: Pos) extends Token(pos)
  case class RelationArrowBegin(pos: Pos) extends Token(pos)
  case class RelationArrowEnd(pos: Pos) extends Token(pos)
  case class Star(pos: Pos) extends Token(pos)
  case class Percent(pos: Pos) extends Token(pos)
  case class Plus(pos: Pos) extends Token(pos)
  case class Bang(pos: Pos) extends Token(pos)
  case class Colon(pos: Pos) extends Token(pos)
  case class MetaSign(pos: Pos) extends Token(pos)
  case class ErrorToken(msg: String, pos: Pos) extends Token(pos)
  
  override def getTokenPosition(token: Token) = token.position
  
  val symbolToks = Set("(", ")", ",", "=", "-->", "|-", "->", "*", "%", "+", "!", ":", "@")
  
  def toToken(txt: String, pos: Pos) = txt match {
    case "(" => RoundBracketOpen(pos)
    case ")" => RoundBracketClose(pos)
    case "," => Comma(pos)
    case "=" => Equals(pos)
    case "-->" => RelationArrow(pos)
    case "|-" => RelationArrowBegin(pos)
    case "->" => RelationArrowEnd(pos)
    case "*" => Star(pos)
    case "%" => Percent(pos)
    case "+" => Plus(pos)
    case "!" => Bang(pos)
    case ":" => Colon(pos)
    case "@" => MetaSign(pos)
    case x if x startsWith "\"" =>
      LiteralString(x.drop(1).dropRight(1), pos)
    case x if x.startsWith("-") && x.tail.forall(_.isDigit) || x.forall(_.isDigit) =>
      LiteralNumber(x, pos)
    case x =>
      // might also be an illegal identifier!
      Identifier(x, pos)
  }
  
  def isLiteralChar(c: Char) =
    c.isLetterOrDigit || c == '_'
  
  def tokenize(in: String): List[Token] = {
    var line = 0
    var col = 0
    var input = in.toList
    val ws = Set('\u0020', '\u0009', '\u000a', '\u000d')
    var currTok = ""
    var toks = List[Token]()
    
    while (input.nonEmpty) {
      input = input.dropWhile { c =>
        if (c == '\u000a') {
          line += 1
          col = 0
          true
        } else {
          col += 1
          ws contains c 
        }
      }
      col -= 1
      
      symbolToks.find(input startsWith _) match {
        case Some(tok) =>
          toks = toToken(tok, Pos(line, col)) :: toks
          col += tok.size
          input = input.drop(tok.size)
        case None =>
          (input.headOption) match {
          case Some('\"') =>
            val r = input.indexOf('\"', 1)
        	  if (r < 0 || input.lastIndexOf('\u000a', r-1) >= 1) { // requires that there is no new line in strings!
        	    return ErrorToken("Missing second ‹\"› in string literal.", Pos(line, col)) :: toks.reverse
        	  }
        	  toks = toToken(input.take(r+1).mkString(""), Pos(line, col)) :: toks
        	  input = input.drop(r+1)
        	  col += r + 1
          case Some('-') =>
            val ns = input.tail.takeWhile(_.isDigit).mkString("")
        	  if (ns == "") return ErrorToken("Expected number after ‹-› sign.", Pos(line, col)) :: toks.reverse
        	  toks = toToken("-" + ns, Pos(line, col)) :: toks
        	  input = input.drop(1 + ns.length)
        	  col += 1 + ns.length
          case Some(_) =>
            val is = input.takeWhile(idChars(_)).mkString("")
        	  if (is == "") return ErrorToken("No processable character.", Pos(line, col)) :: toks.reverse
        	  toks = toToken(is.toString, Pos(line, col)) :: toks
        	  input = input.drop(is.length)
        	  col += is.length
          case None =>
        }
      }
    }
    toks.reverse
  }
  
  def node(in: List[Token]): Parsed[SingleNode] = in match {
    case Identifier(n, p) :: rest =>
      ParseSuccess(SingleNode(n, p), rest)
    case other =>
      ParseFail("Expected node identifier.", other)
  }
  
  def relation(in: List[Token]): Parsed[Expression] = {
    node(in) flatMap { (e1, in2) =>
      in2 match {
        case RelationArrow(_) :: in3 =>
          node(in3) flatMap { (e2, rt) =>
            ParseSuccess(StepTo(e1, Label(""), e2, e1.pos), rt)
          }
        case RelationArrowBegin(_) :: Identifier(label, p) :: RelationArrowEnd(_) :: in3 =>
          node(in3) flatMap { (e2, rt) =>
            ParseSuccess(StepTo(e1, Label(label, p), e2, e1.pos), rt)
          }
        case other =>
          ParseFail("Expected relation symbol.", other)
      }
    }
  }
  
  def attributeList(in: List[Token], list: List[(String, String)]): Parsed[List[(String, String)]] = in match {
    case RoundBracketClose(_) :: rest =>
      ParseSuccess(list.reverse, rest)
    case Identifier(n, p) :: Equals(_) :: LiteralNumber(num, _) :: RoundBracketClose(_) :: rest =>
      ParseSuccess(((n, num) :: list).reverse, rest)
    case Identifier(n, p) :: RoundBracketClose(_) :: rest =>
      ParseSuccess(((n, "") :: list).reverse, rest)
    case Identifier(n, p) :: Equals(_) :: LiteralNumber(num, _) :: Comma(_) :: rest =>
      attributeList(rest, (n, num) :: list)
    case Identifier(n, p) :: Comma(_) :: rest =>
      attributeList(rest, (n, "") :: list)
    case other =>
      ParseFail("Expected comma separated attribute list.", other)
  }
  
  def nodeDeclaration(in: List[Token]): Parsed[NodeDeclaration] = {
    node(in) flatMap { (e, in2) =>
      if (in2.headOption.exists(_.isInstanceOf[RoundBracketOpen])) {
        attributeList(in2.tail, List()) map (NodeDeclaration(e.name, _, e.position))
      } else {
        ParseSuccess(NodeDeclaration(e.name, List(), e.position), in2)
      }
    }
  }
  
  def metaDeclaration(in: List[Token]): Parsed[MetaDeclaration] = in match {
    case MetaSign(p) :: Identifier(n, _) :: LiteralString(v, _) :: rest =>
      ParseSuccess(MetaDeclaration(n, v, p), rest)
    case MetaSign(p) :: Identifier(n, _) :: rest =>
      ParseSuccess(MetaDeclaration(n, "", p), rest)
    case other =>
      ParseFail("Expected meta declaration.", other)
  }
  
  def topLevelExpression(in: List[Token]): Parsed[Expression] = {
    relation(in) orElse { _ =>
      metaDeclaration(in)
    } orElse { _ =>
      nodeDeclaration(in)
    }
  }

  
  def parseDefinitions(in: List[Token]): Parsed[List[Expression]] = {
    if (in.isEmpty) {
      ParseSuccess(List(), in)
    } else {
      topLevelExpression(in) flatMap { (ex, rt) =>
        parseDefinitions(rt) map (ex :: _)
      }
    }
  }
  
  def parseDefinition(in: List[Token]): Parsed[Definition] = {
    parseDefinitions(in) map (Definition(_))
  }
  
  def parse: Parsed[Definition] = {
    val tokens = tokenize(input)
    val fails = tokens.collect {case e: ErrorToken => e}
    if (fails.nonEmpty) {
      ParseFail("Unknown tokens. "+fails.head.msg, fails)
    } else {
      parseDefinition(tokens) match {
        case s @ ParseSuccess(esDef, Nil) =>
          s
        case ParseSuccess(esDef, rest) => 
          ParseFail("Extra characters after end of definition.", rest)
        case fail => fail
      }
    }
  }
}