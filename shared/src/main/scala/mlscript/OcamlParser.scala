package mlscript

import scala.util.chaining._
import scala.collection.mutable
import fastparse._, fastparse.ScalaWhitespace._
import mlscript.utils._, shorthands._
import mlscript.Lexer._

/** Parser for an ML-style input syntax, used in the legacy `ML*` tests. */
@SuppressWarnings(Array("org.wartremover.warts.All"))
class OcamlParser(origin: Origin, indent: Int = 0, recordLocations: Bool = true) {
  
  val keywords = Set(
    "def", "class", "trait", "type", "method", "mut",
    "let", "rec", "in", "fun", "with", "undefined", "null",
    "if", "then", "else", "match", "case", "of")
  def kw[p: P](s: String) = s ~~ !(letter | digit | "_" | "'")
  
  // NOTE: due to bug in fastparse, the parameter should be by-name!
  def locate[p:P, L <: Located](tree: => P[L]): P[L] = (Index ~~ tree ~~ Index).map {
    case (i0, n, i1) => n.withLoc(i0, i1, origin)
  }
  
  def toParams(t: Term): Tup = t match {
    case t: Tup => t
    case _ => Tup((N, Fld(false, false, t)) :: Nil)
  }
  def toParamsTy(t: Type): Tuple = t match {
    case t: Tuple => t
    case _ => Tuple((N, Field(None, t)) :: Nil)
  }
  
  def letter[p: P]     = P( lowercase | uppercase )
  def lowercase[p: P]  = P( CharIn("a-z") )
  def uppercase[p: P]  = P( CharIn("A-Z") )
  def digit[p: P]      = P( CharIn("0-9") )
  def number[p: P]: P[Int] = P( CharIn("0-9").repX(1).!.map(_.toInt) )
  def ident[p: P]: P[String] =
    P( (letter | "_") ~~ (letter | digit | "_" | "'").repX ).!.filter(!keywords(_))
  
  def termOrAssign[p: P]: P[Statement] = P( term ~ ("=" ~ term).? ).map {
    case (expr, N) => expr
    case (pat, S(bod)) => LetS(false, pat, bod)
  }

  def term[p: P]: P[Term] = P(let | fun | ite | withsAsc | _match)

  def lit[p: P]: P[Lit] =
    locate(number.map(x => IntLit(BigInt(x))) | Lexer.stringliteral.map(StrLit(_))
    | P(kw("undefined")).map(x => UnitLit(true)) | P(kw("null")).map(x => UnitLit(false)))
  
  def variable[p: P]: P[Var] = locate(ident.map(Var))

  def parenCell[p: P]: P[Either[Term, (Term, Boolean)]] = (("..." | kw("mut")).!.? ~ term).map {
    case (Some("..."), t) => Left(t)
    case (Some("mut"), t) => Right(t -> true)
    case (_, t) => Right(t -> false)
  }

  def parens[p: P]: P[Term] = locate(P( "(" ~/ parenCell.rep(0, ",") ~ ",".!.? ~ ")" ).map {
    case (Seq(Right(t -> false)), N) => Bra(false, t)
    case (Seq(Right(t -> true)), N) => Tup(N -> Fld(true, false, t) :: Nil) // ? single tuple with mutable
    case (ts, _) => 
      if (ts.forall(_.isRight)) Tup(ts.iterator.map {
        case R(f) => N -> Fld(f._2, false, f._1)
        case _ => die // left unreachable
      }.toList)
      else Splc(ts.map {
        case R((v, m)) => R(Fld(m, false, v))
        case L(spl) => L(spl)
      }.toList)
  })

  def subtermNoSel[p: P]: P[Term] = P( parens | record | lit | variable )
  
  def subterm[p: P]: P[Term] = P( Index ~~ subtermNoSel ~ (
    // Fields:
    ("." ~/ (variable | locate(("(" ~/ ident ~ "." ~ ident ~ ")")
      .map {case (prt, id) => Var(s"${prt}.${id}")})))
      .map {(t: Var) => Left(t)} |
    // Array subscripts:
    ("[" ~ term ~/ "]" ~~ Index).map {Right(_)}
    // Assignment:
    ).rep ~ ("<-" ~ term).?).map {
      case (i0, st, sels, a) =>
        val base = sels.foldLeft(st)((acc, t) => t match {
          case Left(se) => Sel(acc, se)
          case Right((su, i1)) => Subs(acc, su).withLoc(i0, i1, origin)
        })
        a.fold(base)(Assign(base, _))
    }

  def record[p: P]: P[Rcd] = locate(P(
      "{" ~/ (kw("mut").!.? ~ variable ~ "=" ~ term map L.apply).|(kw("mut").!.? ~ variable map R.apply).rep(sep = ";") ~ "}"
    ).map { fs => Rcd(fs.map{ 
        case L((mut, v, t)) => v -> Fld(mut.isDefined, false, t)
        case R(mut -> id) => id -> Fld(mut.isDefined, false, id) }.toList)})
  
  def fun[p: P]: P[Term] = locate(P( kw("fun") ~/ term ~ "->" ~ term ).map(nb => Lam(toParams(nb._1), nb._2)))
  
  def let[p: P]: P[Term] = locate(P(
      kw("let") ~/ kw("rec").!.?.map(_.isDefined) ~ variable ~ subterm.rep ~ "=" ~ term ~ kw("in") ~ term
    ) map {
      case (rec, id, ps, rhs, bod) => Let(rec, id, ps.foldRight(rhs)((i, acc) => Lam(toParams(i), acc)), bod)
    })
  
  def ite[p: P]: P[Term] = P( kw("if") ~/ term ~ kw("then") ~ term ~ kw("else") ~ term ).map(ite =>
    App(App(App(Var("if"), ite._1), ite._2), ite._3))
  
  def withsAsc[p: P]: P[Term] = P( withs ~ (":" ~/ ty).rep ).map {
    case (withs, ascs) => ascs.foldLeft(withs)(Asc)
  }
  
  def withs[p: P]: P[Term] = P( binops ~ (kw("with") ~ record).rep ).map {
    case (as, ws) => ws.foldLeft(as)((acc, w) => With(acc, w))
  }
  
  def mkApp(lhs: Term, rhs: Term): Term = App(lhs, toParams(rhs))
  def apps[p: P]: P[Term] = P( subterm.rep(1).map(_.reduce(mkApp)) )
  
  def _match[p: P]: P[CaseOf] =
    locate(P( kw("case") ~/ term ~ "of" ~ "{" ~ "|".? ~ matchArms ~ "}" ).map(CaseOf.tupled))
  def matchArms[p: P]: P[CaseBranches] = P(
    ( ("_" ~ "->" ~ term).map(Wildcard)
    | ((lit | variable) ~ "->" ~ term ~ matchArms2)
      .map { case (t, b, rest) => Case(t, b, rest) }
    ).?.map {
      case None => NoCases
      case Some(b) => b
    }
  )
  def matchArms2[p: P]: P[CaseBranches] = ("|" ~ matchArms).?.map(_.getOrElse(NoCases))
  
  private val prec: Map[Char,Int] = List(
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
  
  // Adapted from: https://github.com/databricks/sjsonnet/blob/master/sjsonnet/src/sjsonnet/Parser.scala#L136-L180
  def binops[p: P]: P[Term] =
    P(apps ~ (Index ~~ operator.! ~~ Index ~/ apps).rep ~ "").map { case (pre, fs) =>
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
                result = App(App(Var(op).withLoc(off0, off1, origin), toParams(result)), toParams(rhs))
                true
              }
          }
        )()
        result
      }
      climb(0, pre)
    }
  def operator[p: P]: P[Unit] = P(
    !symbolicKeywords ~~ (!StringIn("/*", "//") ~~ (CharsWhile(OpCharNotSlash) | "/")).rep(1)
  ).opaque("operator")
  def symbolicKeywords[p: P] = P{
    StringIn(
      "|",
      "~",
      ";", "=>", "=", "->", "<-",
      ":",
      "#", "@", "\\", "\u21d2", "\u2190"
    ) ~~ !OpChar
  }.opaque("symbolic keywords")
  
  def expr[p: P]: P[Term] = P( term ~ End )
  
  /** top level function defintions using let with single and multiple arguments */
  def ocamlDefDecl[p: P]: P[Def] =
    locate(P((kw("let") ~ variable ~ tyParams ~ ":" ~/ ty map {
      case (id, tps, t) => Def(true, id, R(PolyType(tps, t)), true)
    }) | (kw("rec").!.?.map(_.isDefined) ~ kw("let") ~/ variable ~ subterm.rep ~ "=" ~ term map {
      case (rec, id, ps, bod) => Def(rec, id, L(ps.foldRight(bod)((i, acc) => Lam(toParams(i), acc))), true)
    })))
  
  /** data constructor arguments
    * type 'a, 'b tup = Tup of ['a * 'b]
    * TODO handle type parameters
    * TODO handle type aliases
    * type heapVar = HeapInt of int | Heap of heap and
    *   heap = (string * int) list
    * 
    */
  def ocamlConstructorArguments[p: P]: P[(Ls[TypeName], Record)] =
    tyName.rep(1, "*")
    .map(args => {
      val fields = args.zipWithIndex.map { case (tname, i) =>
        (Var("_" + i.toString()), Field(None, tname))
      }.toList
      (Nil, Record(fields))
    })
    
  /** data constructor declaration
   *  type 'a, 'b tup = [Tup of 'a * 'b]
   * TODO: handle type parameters
   */
  def ocamlConstructorDecl[p: P]: P[TypeDef] =
    P((tyName ~ ("of" ~/ ocamlConstructorArguments).?).map {
      case (id, Some((params, body))) => TypeDef(Cls, id, params, body, Nil, Nil, Nil)
      case (id, None)                 => TypeDef(Cls, id, Ls.empty, Record(Ls.empty), Nil, Nil, Nil)
    })
    
  // https://v2.ocaml.org/manual/typedecl.html#ss:typedefs
  // TODO: handle record style type representation
  // type 'a lst = [Null | Cons of 'a * 'a lst]
  def ocamlTypeRepresentation[p: P]: P[List[TypeDef]] = ocamlConstructorDecl.rep(1, "|").map(_.toList)
  
  def tyKind[p: P]: P[TypeDefKind] = (kw("class") | kw("trait") | kw("type")).! map {
    case "class" => Cls
    case "trait" => Trt
    case "type"  => Als
  }
  /** Modified type declaration that parses type constructor
    * and data constructor and returns them all together as a
    * list
    */
  def tyDecl[p: P]: P[Ls[TypeDef]] =
    P((kw("type") ~/ tyName ~ "=" ~/ ocamlTypeRepresentation)
      .map { case (tname, bodies) =>
        val bodyTypes = bodies.map(tyDef => tyDef.nme)
        val aliasBody = bodyTypes.foldLeft[Type](bodyTypes(0))(Union)
        TypeDef(Als, tname, Nil, aliasBody, Nil, Nil, Nil) :: bodies
    })
  def tyParams[p: P]: P[Ls[TypeName]] =
    ("[" ~ tyName.rep(0, ",") ~ "]").?.map(_.toList.flatten)
  def mthDecl[p: P](prt: TypeName): P[R[MethodDef[Right[Term, Type]]]] = 
    P(kw("method") ~ variable ~ tyParams ~ ":" ~/ ty map {
      case (id, ts, t) => R(MethodDef[Right[Term, Type]](true, prt, id, ts, R(t)))
    })
  def mthDef[p: P](prt: TypeName): P[L[MethodDef[Left[Term, Type]]]] = 
    P(kw("rec").!.?.map(_.isDefined) ~ kw("method") ~ variable ~ tyParams ~ subterm.rep ~ "=" ~/ term map {
      case (rec, id, ts, ps, bod) =>
        L(MethodDef(rec, prt, id, ts, L(ps.foldRight(bod)((i, acc) => Lam(toParams(i), acc)))))
    })
  
  def ty[p: P]: P[Type] = P( tyNoAs ~ ("as" ~ tyVar).rep ).map {
    case (ty, ass) => ass.foldLeft(ty)((a, b) => Recursive(b, a))
  }
  def tyNoAs[p: P]: P[Type] = P( tyNoUnion.rep(1, "|") ).map(_.reduce(Union))
  def tyNoUnion[p: P]: P[Type] = P( tyNoInter.rep(1, "&") ).map(_.reduce(Inter))
  def tyNoInter[p: P]: P[Type] = P( tyNoFun ~ ("->" ~/ tyNoInter).? ).map {
    case (l, S(r)) => Function(toParamsTy(l), r)
    case (l, N) => l
  }
  // Note: field removal types are not supposed to be explicitly used by programmers,
  //    and they won't work in negative positions,
  //    but parsing them is useful in tests (such as shared/src/test/diff/mlscript/Annoying.mls)
  def tyNoFun[p: P]: P[Type] = P( (rcd | ctor | parTy) ~ ("\\" ~ variable).rep(0) ) map {
    case (ty, Nil) => ty
    case (ty, ids) => Rem(ty, ids.toList)
  }
  def ctor[p: P]: P[Type] = locate(P( tyName ~ "[" ~ ty.rep(0, ",") ~ "]" ) map {
    case (tname, targs) => AppliedType(tname, targs.toList)
  }) | tyNeg | tyName | tyVar | tyWild | litTy
  def tyNeg[p: P]: P[Type] = locate(P("~" ~/ tyNoFun map { t => Neg(t) }))
  def tyName[p: P]: P[TypeName] = locate(P(ident map TypeName))
  def tyVar[p: P]: P[TypeVar] = locate(P("'" ~ ident map (id => TypeVar(R("'" + id), N))))
  def tyWild[p: P]: P[Bounds] = locate(P("?".! map (_ => Bounds(Bot, Top))))
  def rcd[p: P]: P[Record] =
    locate(P( "{" ~/ ( kw("mut").!.? ~ variable ~ ":" ~ ty).rep(sep = ";") ~ "}" )
      .map(_.toList.map {
        case (None, v, t) => v -> Field(None, t)
        case (Some(_), v, t) => v -> Field(Some(t), t)
      } pipe Record))

  def parTyCell[p: P]: P[Either[Type, (Type, Boolean)]] = (("..." | kw("mut")).!.? ~ ty). map {
    case (Some("..."), t) => Left(t)
    case (Some("mut"), t) => Right(t -> true)
    case (_, t) => Right(t -> false)
  }

  def parTy[p: P]: P[Type] = locate(P( "(" ~/ parTyCell.rep(0, ",").map(_.map(N -> _).toList) ~ ",".!.? ~ ")" ).map {
    case (N -> Right(ty -> false) :: Nil, N) => ty
    case (fs, _) => 
      if (fs.forall(_._2.isRight))
        Tuple(fs.map {
          case (l, Right(t -> false)) => l -> Field(None, t)
          case (l, Right(t -> true)) => l -> Field(Some(t), t)
          case _ => ??? // unreachable
        })
      else Splice(fs.map{ _._2 match { 
        case L(l) => L(l) 
        case R(r -> true) => R(Field(Some(r), r))
        case R(r -> false) => R(Field(None, r))
      } })
  })
  def litTy[p: P]: P[Type] = P( lit.map(l => Literal(l).withLocOf(l)) )
  
  def toplvl[p: P]: P[Ls[Statement]] =
    P( ocamlDefDecl.map(_ :: Nil) | tyDecl | termOrAssign.map(_ :: Nil) )
  def pgrm[p: P]: P[Pgrm] = P( (";".rep ~ toplvl ~ topLevelSep.rep).rep.map(_.toList.flatten) ~ End ).map(Pgrm)
  def topLevelSep[p: P]: P[Unit] = ";"
  
  private var curHash = 0
  def nextHash: Int = {
    val res = curHash
    curHash = res + 1
    res
  }
  
}
object OcamlParser {
  
  def addTopLevelSeparators(lines: IndexedSeq[Str]): IndexedSeq[Str] = {
    (lines.iterator ++ lines.lastOption).toList.sliding(2).map {
      case l0 :: l1 :: Nil =>
        if (l1.startsWith(" ") || l1.startsWith("\t")) l0 + "\n"
        else l0 + ";"
      case l :: Nil => l
      case _ => die
    }.toIndexedSeq
  }
  
}
