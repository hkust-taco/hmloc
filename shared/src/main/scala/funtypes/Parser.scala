package funtypes

import funtypes.utils._, shorthands._
import scala.language.implicitConversions
import scala.util.chaining._
import Lexer._
import fastparse._

/** Inspired by and adapted from:
  *   scalaparse: https://github.com/lihaoyi/fastparse/tree/master/scalaparse
  *   pythonparse: https://github.com/lihaoyi/fastparse/tree/master/pythonparse
  */
@SuppressWarnings(Array("org.wartremover.warts.All"))
class Parser(indent: Int = 0, recordLocations: Bool = true) {
  //implicit def whitespace(cfg: P[_]): P[Unit] = Lexical.wscomment(cfg)
  implicit def whitespace(cfg: P[_]): P[Unit] = Lexer.nonewlinewscomment(cfg)
  
  type Location = (Int,Int)
  def Location(l:Int,r:Int)=(l,r)
  
  def Pa(t:Term,l:Opt[Location]):Term = {
    l.foreach{case(l,r)=>t.spanStart=l;t.spanEnd=r}
    t
  }
  def UnitLit = Tup(Nil)
  
  def loc(i0:Int,i1:Int) = if (recordLocations) Some(Location(i0,i1)) else None
  val noloc: Opt[Location] = None // for when it's easy to retrieve from the children
  
  def mk(i0:Int,t:Term,i1:Int) = Pa(t,loc(i0,i1))
  
  // NOTE: due to bug in fastparse, the parameter should be by-name!
  def locate[_:P](tree: => P[Term]) = (Index ~~ tree ~~ Index).map((mk _).tupled)
  
  def space[_: P] = P( CharIn(" \n") )
  def NEWLINE[_: P]: P0 = P( "\n" | End )
  def ENDMARKER[_: P]: P0 = P( End )//.opaque("unexpected token in this position")
  
  def nl_indents[_: P] = P( "\n" ~ emptyLines ~~ " ".repX(indent) )
  def emptyLines[_: P] = P( ("" ~ Lexer.comment.? ~ "\n").repX(0) )
  
  def spaces[_: P] = P( (Lexer.nonewlinewscomment.? ~~ "\n").repX(1) )
  
  def NAME[_: P]: P[Var] = ident.map(Var(_))
  
  def ident[_: P] = Lexer.identifier | "(" ~ operator.! ~ ")"
  
  def NUMBER[_: P]: P[Lit] =
    P( Lexer.longinteger | Lexer.integer ).map(IntLit) |
    P( Lexer.floatnumber ).map(DecLit)
  def STRING[_: P]: P[String] = Lexer.stringliteral
  
  def expr[_: P]: P[Term] = P( emptyLines ~ multilineBlock ~ emptyLines )
  
  def stmt[_: P]: P[Statement] = let | lams
  
  def let[_: P]: P[LetS] =
    P( kw("let") ~ kw("rec").!.? ~ commas ~ "=" ~ lams ).map {
      case (r, p, e) => LetS(r.isDefined, p, e)
    }
  
  def multilineBlock[_: P]: P[Blk] = P( stmt ~ (";" ~ stmt).rep ~ (";".? ~ nl_indents ~~ multilineBlock).? ).map {
    case (s, ss1, N) => Blk(s :: ss1.toList)
    case (s, ss1, S(Blk(ss2))) => Blk(s :: ss1.toList ::: ss2.toList)
  }
  
  def lams[_: P]: P[Term] = P( (commas ~ ("/".! | "=>".!)).rep ~ body ).map {
    case (xs, a) => xs.foldRight(a) {
      case ((x, "/"), acc) => App(x, acc)
      case ((x, "=>"), acc) => Lam(x, acc)
    }
  }
  def body[_: P]: P[Term] = P( commas | suite )
  
  def commas[_: P]: P[Term] = P( binops ~ (Index ~~ "," ~~ Index ~ commas).? ).map { // note: used to have `binops ~/`
    case (lhs, N) => lhs
    case (lhs, S((i0,i1,rhs))) => App(App(Var(","), lhs), rhs) // TODO
  }
  
  /** Note that `,` implicitly has the lowest precedence, followed by the ones below. */
  private val prec: Map[Char,Int] = List(
    //",", // Used to have it here; but that made it left-associative
    ":",
    "|",
    "^",
    "&",
    "= !",
    "< >",
    "+ -",
    "* / %",
    ".",
  ).zipWithIndex.flatMap {
    case (cs,i) => cs.filterNot(_ == ' ').map(_ -> i)
  }.toMap.withDefaultValue(Int.MaxValue)
  
  def precedence(op: String): Int = prec(op.head) min prec(op.last)
  
  // Note: There are three right-associative operators, dealt with above, not here: `=>`, `/`, and `,`
  // Adapted from: https://github.com/databricks/sjsonnet/blob/master/sjsonnet/src/sjsonnet/Parser.scala#L136-L180
  // def binops[_: P]: P[Pa] = P("" ~ apps ~ (Index ~~ operator.! ~~ Index ~/ apps).rep ~ "").map { case (pre, fs) =>
  def binops[_: P]: P[Term] = P("" ~ apps ~ (Index ~~ operator.! ~~ Index ~ apps).rep ~ "").map { case (pre, fs) =>
    var remaining = fs
    def climb(minPrec: Int, current: Term): Term = {
      var result = current
      while (
        remaining.headOption match {
          case None => false
          case Some((off0, op, off1, next)) =>
            val prec: Int = precedence(op)
            if (prec < minPrec) false
            else {
              remaining = remaining.tail
              val rhs = climb(prec + 1, next)
              //result = Pa(OtherBinop(result, op, rhs), loc(off0,off1))
              result = Pa(App(
                Pa(App(
                  Pa(Var(op),loc(off0,off1)),
                  result
                ), noloc),
                rhs
              ), noloc)
              true
            }
        }
      )()
      result
    }
    climb(0, pre)
  }
  
  def apps[_: P]: P[Term] = P( atomOrSelect.rep(1) ~ suite.? ).map {
    case (as, ao) => (as ++ ao.toList).reduceLeft(App(_, _))
  }
  
  def atomOrSelect[_: P]: P[Term] = P(atom ~ ("." ~ (Index ~~ ident ~~ Index)).?).map {
    case (lhs, Some((i0,str,i1))) => Sel(lhs, str)
    case (lhs, None) => lhs
  }
  
  // TOOD support suite
  def record[_: P]: P[Term] =
    P( "{" ~ (ident ~ ":" ~ (binops | suite)).rep(sep = ",") ~ "}" ).map(_.toList pipe Rcd)
  
  def atom[_: P]: P[Term] =
    "(" ~ expr ~ ")" | STRING.map(StrLit) | NAME | NUMBER | record
  
  def suite[_: P]: P[Term] = {
    // TOOD ignore empty and comment lines
    def nextIndentP = " ".repX(indent + 1).!.map(_.length)
    def indented = "\n" ~~ nextIndentP.flatMapX{ nextIndent =>
      new Parser(nextIndent,recordLocations).multilineBlock
    }
    P( indented ).opaque("indented block")
  }
  
  def repl_input[_: P]: P[Term] = P( (expr | P("").map(_ => Pa(UnitLit,noloc))) ~ ENDMARKER )
  
  def pgrm[_: P]: P[Blk] = P( multilineBlock ~ End )
  
}