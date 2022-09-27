package mlscript

import scala.util.chaining._
import scala.collection.mutable
import fastparse._, fastparse.ScalaWhitespace._
import mlscript.utils._, shorthands._
import mlscript.Lexer._
import java.io.File
import java.nio.file.Path

/** Parser for an ML-style input syntax, used in the legacy `ML*` tests. */
@SuppressWarnings(Array("org.wartremover.warts.All"))
class OcamlParser(origin: Origin, indent: Int = 0, recordLocations: Bool = true) {

  val keywords = Set(
    "def",
    "class",
    "trait",
    "type",
    "method",
    "mut",
    "let",
    "rec",
    "in",
    "fun",
    "with",
    "undefined",
    "null",
    "if",
    "then",
    "else",
    "match",
    "case",
    "of"
  )

  def letter[p: P] = P(lowercase | uppercase)
  def lowercase[p: P] = P(CharIn("a-z"))
  def uppercase[p: P] = P(CharIn("A-Z"))
  def digit[p: P] = P(CharIn("0-9"))
  def number[p: P]: P[Int] = P(CharIn("0-9").repX(1).!.map(_.toInt))

  // new identifiers except keywords
  def ident[p: P]: P[String] =
    P((letter | "_") ~~ (letter | digit | "_" | "'").repX).log.?.!.filter(!keywords(_)).log

  // introduce keyword
  def kw[p: P](s: String) = s ~~ !(letter | digit | "_" | "'")

  // NOTE: due to bug in fastparse, the parameter should be by-name!
  def locate[p: P, L <: Located](tree: => P[L]): P[L] = (Index ~~ tree ~~ Index).map { case (i0, n, i1) =>
    n.withLoc(i0, i1, origin)
  }
  def variable[p: P]: P[Var] = locate(ident.map(Var)).log

  def tyName[p: P]: P[TypeName] = locate(P(ident map TypeName))
  def tyParam[p: P]: P[TypeName] =
    ((CharIn("'") ~ tyName))
  
  // type ['a, 'b] tup = Tup of 'a * 'b
  def tyParams[p: P]: P[Ls[TypeName]] =
    (tyParam.rep(0, ",")).?.map(_.toList.flatten)
    
  // type ['a, 'b tup] = Tup of 'a * 'b
  def typeExpression[p: P]: P[(Ls[TypeName], TypeName)] =
    (tyParams ~ tyName).map { case args @ (tparams, tname) =>
      if (tname.name.isEmpty) (Ls.empty, tparams(0)) else args
    }

  // type 'a, 'b tup = Tup of ['a * 'b]
  def constructorArguments[p: P]: P[(List[TypeName], Record)] = typeExpression
    .rep(1, "*")
    .map(args => {
      println(args)
      val typeParams = args.map(_._1).flatMap(_.iterator).distinct.toList
      val fields = args.zipWithIndex.map { case ((tparams, tname), i) =>
        val fieldType = if (tparams.isEmpty) tname else AppliedType(tname, tparams)
        (Var("_" + i.toString()), Field(None, fieldType))
      }.toList
      (typeParams, Record(fields))
    })

  // type 'a, 'b tup = [Tup of 'a * 'b]
  def constructorDecl[p: P]: P[TypeDef] =
    P((tyName ~ ("of" ~/ constructorArguments).?).map {
      case (id, Some((params, body))) => TypeDef(Cls, id, params, body)
      case (id, None)                 => TypeDef(Cls, id, List.empty, Record(List.empty))
    })

  // https://v2.ocaml.org/manual/typedecl.html#ss:typedefs
  // TODO: handle record style type representation
  // type 'a lst = [Null | Cons of 'a * 'a lst]
  def typeRepresentation[p: P]: P[List[TypeDef]] = constructorDecl.rep(1, "|").map(_.toList)

  /** Type definition declares an alias. The alias is a union of type names of the class defintions defined within it.
    * The class defintitions and then the body is returned as a list.
    */
  // [type 'a lst = Null | Cons of 'a * 'a lst]
  def typeDefinition[p: P]: P[Ls[TypeDef]] =
    P(("type" ~/ tyParams ~ tyName).flatMap { case (ts, id) =>
      "=" ~/ typeRepresentation map (body => {
        val bodyTypes =
          body.map(tyDef => if (tyDef.tparams.isEmpty) tyDef.nme else AppliedType(tyDef.nme, tyDef.tparams))
        val aliasBody = bodyTypes.foldLeft[Type](bodyTypes(0))((t1, t2) => Union(t1, t2))
        body :+ TypeDef(Als, id, ts, aliasBody)
      })
    })

  // [Cons(3, Cons(4, Null))]
  def initializeConstructor[p: P]: P[Term] = P(
    (variable ~ "(" ~/ (lit | variable | initializeConstructor).rep(0, ",") ~ ")").map {
    case (typeName, arguments: Seq[Term]) =>
      val argRecord = arguments.zipWithIndex.map {
        case (t, i) => Var(s"_$i") -> (t -> true)
      }.toList
      App(typeName, Tup(None -> (Rcd(argRecord) -> true) :: Nil))
  })
  
  // term application
  def ocamlTerm[p: P]: P[Term] = P(
    (variable ~ (variable | "(" ~/ initializeConstructor ~ ")").rep(0, " ")).map {
      case (appliedVar, params) =>
        val paramsTup = params.map {
          case t => N -> (t -> true)
        }.toList
        if (paramsTup.isEmpty)
          appliedVar
        else
          App(appliedVar, Tup(paramsTup))
    }
  )
  def parseExpression[p: P]: P[Term] = initializeConstructor | ocamlTerm | matchCase
  def letBinding[p: P]: P[Ls[Statement]] =
    P(("let" ~/ kw("rec").!.? ~ variable ~ variable.rep(0, " ") ~ "=" ~ parseExpression).map {
      // [let x = Cons(3, Cons(4, Null))]
      case (isRec, name, Seq(), body) =>
        LetS(isRec.isDefined, name, body) :: Nil
      // [let rec f x y = x f y]
      case (isRec, name, args, body) =>
        val desugarLet = args.foldRight(body){ case (arg, acc) =>
          Lam(toParams(arg), acc)
        }
        Def(isRec.isDefined, name, L(desugarLet)) :: Nil
  })
  
  def matchCaseArm[p:P]: P[CaseBranches] = P(
  (
    ("_" ~ "->" ~ parseExpression).map(Wildcard)
    | ((lit | variable) ~ parseExpression ~ matchArmsNoCase).map {
        case (t, b, rest) => Case(t, b, rest)
      }).?.map {
    case None => NoCases
    case Some(b) => b
  })
  def matchArmsNoCase[p: P]: P[CaseBranches] = ("|" ~ matchCaseArm).?.map(_.getOrElse(NoCases))
  def matchCase[p:P]: P[CaseOf] = P(
  locate(("match" ~/ variable ~/ "with" ~ matchCaseArm).map(CaseOf.tupled)))
  
  def tempArm[p: P]: P[CaseBranches] = P(
    (("_" ~ "->" ~ term).map(Wildcard)
      | ((lit | variable) ~ "->" ~ term ~ matchArms2)
        .map { case (t, b, rest) => Case(t, b, rest) }).?.map {
      case None    => NoCases
      case Some(b) => b
    }
  )

  def toplvl[p: P]: P[Ls[Statement]] = (typeDefinition | letBinding)
  // def toplvl[p: P]: P[Ls[Statement]] =
  //   P(defDecl | tyDecl | termOrAssign, )
  // def toplvl[p: P]: P[Ls[Statement]] =
    // P(typeDefinition | let.map(_ :: Nil))
    // P(letS.map(_ :: Nil))
  def pgrm[p: P]: P[Pgrm] = P((";".rep ~ toplvl ~ topLevelSep.rep).rep ~ End).map(stmts => Pgrm(stmts.flatten.toList))
    
  // def letExpression[p:P]: P[Let] =
  //   P(("let" ~~ "rec" ~~ variable.rep(1, " ")).map {
  //     case (t) => 
  //   })

  def let[p: P]: P[Term] = locate(
    P(
      kw("let") ~/ kw("rec").!.?.map(_.isDefined) ~ variable ~ subterm.rep ~ "=" ~ term ~ kw("in") ~ term
    ) map { case (rec, id, ps, rhs, bod) =>
      Let(rec, id, ps.foldRight(rhs)((i, acc) => Lam(toParams(i), acc)), bod)
    }
  )

  def lit[p: P]: P[Lit] =
    locate(
      number.map(x => IntLit(BigInt(x))) | Lexer.stringliteral.map(StrLit(_))
        | P(kw("undefined")).map(x => UnitLit(true)) | P(kw("null")).map(x => UnitLit(false))
    )

  // def ocamlTerm[p: P]: P[Term] =
  //   P((ident ~ "(" ~ ocamlTerm ~ ")").map{
  //     case (nme, body) => App(Var(nme), Tup((None -> (body -> false)) :: Nil))
  //   } | lit).log

  def letS[p: P]: P[LetS] = locate(
    P(
      kw("let") ~/ kw("rec").!.?.map(_.isDefined) ~ variable.log ~ "=" ~ initializeConstructor
    ) map { case (rec, id, rhs) =>
      LetS(rec, id, rhs)
    }
  ).log
  def toParams(t: Term): Tup = t match {
    case t: Tup => t
    case _      => Tup((N, (t, false)) :: Nil)
  }

  /// ocaml above ^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def tyDecl[p: P]: P[TypeDef] =
    P((tyKind ~/ tyName ~ tyParams).flatMap {
      case (k @ (Cls | Trt), id, ts) =>
        (":" ~ ty).? ~ (mthDecl(id) | mthDef(id)).rep.map(_.toList) map { case (bod, ms) =>
          TypeDef(k, id, ts, bod.getOrElse(Top), ms.collect { case R(md) => md }, ms.collect { case L(md) => md })
        }
      case (k @ Als, id, ts) => "=" ~ ty map (bod => TypeDef(k, id, ts, bod))
    })
  def toParamsTy(t: Type): Tuple = t match {
    case t: Tuple => t
    case _        => Tuple((N, Field(None, t)) :: Nil)
  }

  def termOrAssign[p: P]: P[Statement] = P(term ~ ("=" ~ term).?).map {
    case (expr, N)     => expr
    case (pat, S(bod)) => LetS(false, pat, bod)
  }

  def term[p: P]: P[Term] = P(let | fun | ite | withsAsc | _match)

  def parens[p: P]: P[Term] = locate(P("(" ~/ (kw("mut").!.? ~ term).rep(0, ",") ~ ",".!.? ~ ")").map {
    case (Seq(None -> t), N)    => Bra(false, t)
    case (Seq(Some(_) -> t), N) => Tup(N -> (t, true) :: Nil) // ? single tuple with mutable
    case (ts, _)                => Tup(ts.iterator.map(f => N -> (f._2, f._1.isDefined)).toList)
  })

  def subtermNoSel[p: P]: P[Term] = P(parens | record | lit | variable)

  def subterm[p: P]: P[Term] = P(
    Index ~~ subtermNoSel ~ (
      // Fields:
      ("." ~/ (variable | locate(
        ("(" ~/ ident ~ "." ~ ident ~ ")")
          .map { case (prt, id) => Var(s"${prt}.${id}") }
      )))
        .map { (t: Var) => Left(t) } |
        // Array subscripts:
        ("[" ~ term ~/ "]" ~~ Index).map { Right(_) }
      // Assignment:
    ).rep ~ ("<-" ~ term).?
  ).map { case (i0, st, sels, a) =>
    val base = sels.foldLeft(st)((acc, t) =>
      t match {
        case Left(se)        => Sel(acc, se)
        case Right((su, i1)) => Subs(acc, su).withLoc(i0, i1, origin)
      }
    )
    a.fold(base)(Assign(base, _))
  }

  def record[p: P]: P[Rcd] = locate(
    P(
      "{" ~/ (kw("mut").!.? ~ variable ~ "=" ~ term map L.apply)
        .|(kw("mut").!.? ~ variable map R.apply)
        .rep(sep = ";") ~ "}"
    ).map { fs =>
      Rcd(fs.map {
        case L((mut, v, t)) => v -> (t -> mut.isDefined)
        case R(mut -> id)   => id -> (id -> mut.isDefined)
      }.toList)
    }
  )

  def fun[p: P]: P[Term] = locate(P(kw("fun") ~/ term ~ "->" ~ term).map(nb => Lam(toParams(nb._1), nb._2)))



  def ite[p: P]: P[Term] = P(kw("if") ~/ term ~ kw("then") ~ term ~ kw("else") ~ term).map(ite =>
    App(App(App(Var("if"), ite._1), ite._2), ite._3)
  )

  def withsAsc[p: P]: P[Term] = P(withs ~ (":" ~/ ty).rep).map { case (withs, ascs) =>
    ascs.foldLeft(withs)(Asc)
  }.log

  def withs[p: P]: P[Term] = P(binops ~ (kw("with") ~ record).rep).map { case (as, ws) =>
    ws.foldLeft(as)((acc, w) => With(acc, w))
  }.log

  def mkApp(lhs: Term, rhs: Term): Term = App(lhs, toParams(rhs))
  def apps[p: P]: P[Term] = P(subterm.rep(1).map(_.reduce(mkApp))).log

  def _match[p: P]: P[CaseOf] =
    locate(P(kw("case") ~/ term ~ "of" ~ "{" ~ "|".? ~ matchArms ~ "}").map(CaseOf.tupled))
  def matchArms[p: P]: P[CaseBranches] = P(
    (("_" ~ "->" ~ term).map(Wildcard)
      | ((lit | variable) ~ "->" ~ term ~ matchArms2)
        .map { case (t, b, rest) => Case(t, b, rest) }).?.map {
      case None    => NoCases
      case Some(b) => b
    }
  )
  def matchArms2[p: P]: P[CaseBranches] = ("|" ~ matchArms).?.map(_.getOrElse(NoCases))

  private val prec: Map[Char, Int] = List(
    ":",
    "|",
    "^",
    "&",
    "= !",
    "< >",
    "+ -",
    "* / %",
    "."
  ).zipWithIndex
    .flatMap { case (cs, i) =>
      cs.filterNot(_ == ' ').map(_ -> i)
    }
    .toMap
    .withDefaultValue(Int.MaxValue)
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
        ) ()
        result
      }
      climb(0, pre)
    }.log
  def operator[p: P]: P[Unit] = P(
    !symbolicKeywords ~~ (!StringIn("/*", "//") ~~ (CharsWhile(OpCharNotSlash) | "/")).rep(1).log
  ).opaque("operator")
  def symbolicKeywords[p: P] = P {
    StringIn(
      "|",
      "~",
      ";",
      "=>",
      "=",
      "->",
      "<-",
      ":",
      "#",
      "@",
      "\\",
      "\u21d2",
      "\u2190"
    ) ~~ !OpChar
  }.opaque("symbolic keywords")

  def expr[p: P]: P[Term] = P(term ~ End)

  def defDecl[p: P]: P[Def] =
    locate(P((kw("def") ~ variable ~ tyParams ~ ":" ~/ ty map { case (id, tps, t) =>
      Def(true, id, R(PolyType(tps, t)))
    }) | (kw("rec").!.?.map(_.isDefined) ~ kw("def") ~/ variable ~ subterm.rep ~ "=" ~ term map {
      case (rec, id, ps, bod) => Def(rec, id, L(ps.foldRight(bod)((i, acc) => Lam(toParams(i), acc))))
    })))

  def tyKind[p: P]: P[TypeDefKind] = (kw("class") | kw("trait") | kw("type")).! map {
    case "class" => Cls
    case "trait" => Trt
    case "type"  => Als
  }
  def mthDecl[p: P](prt: TypeName): P[R[MethodDef[Right[Term, Type]]]] =
    P(kw("method") ~ variable ~ tyParams ~ ":" ~/ ty map { case (id, ts, t) =>
      R(MethodDef[Right[Term, Type]](true, prt, id, ts, R(t)))
    })
  def mthDef[p: P](prt: TypeName): P[L[MethodDef[Left[Term, Type]]]] =
    P(kw("rec").!.?.map(_.isDefined) ~ kw("method") ~ variable ~ tyParams ~ subterm.rep ~ "=" ~/ term map {
      case (rec, id, ts, ps, bod) =>
        L(MethodDef(rec, prt, id, ts, L(ps.foldRight(bod)((i, acc) => Lam(toParams(i), acc)))))
    })

  def ty[p: P]: P[Type] = P(tyNoAs ~ ("as" ~ tyVar).rep).map { case (ty, ass) =>
    ass.foldLeft(ty)((a, b) => Recursive(b, a))
  }
  def tyNoAs[p: P]: P[Type] = P(tyNoUnion.rep(1, "|")).map(_.reduce(Union))
  def tyNoUnion[p: P]: P[Type] = P(tyNoInter.rep(1, "&")).map(_.reduce(Inter))
  def tyNoInter[p: P]: P[Type] = P(tyNoFun ~ ("->" ~/ tyNoInter).?).map {
    case (l, S(r)) => Function(toParamsTy(l), r)
    case (l, N)    => l
  }
  // Note: field removal types are not supposed to be explicitly used by programmers,
  //    and they won't work in negative positions,
  //    but parsing them is useful in tests (such as shared/src/test/diff/mlscript/Annoying.mls)
  def tyNoFun[p: P]: P[Type] = P((rcd | ctor | parTy) ~ ("\\" ~ variable).rep(0)) map {
    case (ty, Nil) => ty
    case (ty, ids) => Rem(ty, ids.toList)
  }
  def ctor[p: P]: P[Type] = locate(P(tyName ~ "[" ~ ty.rep(0, ",") ~ "]") map { case (tname, targs) =>
    AppliedType(tname, targs.toList)
  }) | tyNeg | tyName | tyVar | tyWild | litTy
  def tyNeg[p: P]: P[Type] = locate(P("~" ~/ tyNoFun map { t => Neg(t) }))
  def tyVar[p: P]: P[TypeVar] = locate(P("'" ~ ident map (id => TypeVar(R("'" + id), N))))
  def tyWild[p: P]: P[Bounds] = locate(P("?".! map (_ => Bounds(Bot, Top))))
  def rcd[p: P]: P[Record] =
    locate(
      P("{" ~/ (kw("mut").!.? ~ variable ~ ":" ~ ty).rep(sep = ";") ~ "}")
        .map(_.toList.map {
          case (None, v, t)    => v -> Field(None, t)
          case (Some(_), v, t) => v -> Field(Some(t), t)
        } pipe Record)
    )
  def parTy[p: P]: P[Type] = locate(
    P("(" ~/ (kw("mut").!.? ~ ty).rep(0, ",").map(_.map(N -> _).toList) ~ ",".!.? ~ ")").map {
      case (N -> (N -> ty) :: Nil, N) => ty
      case (fs, _) =>
        Tuple(fs.map {
          case (l, N -> t)    => l -> Field(None, t)
          case (l, S(_) -> t) => l -> Field(Some(t), t)
        })
    }
  )
  def litTy[p: P]: P[Type] = P(lit.map(l => Literal(l).withLocOf(l)))

  // def toplvl[p: P]: P[Ls[Statement]] = typeDefinition
  // def toplvl[p: P]: P[Ls[Statement]] =
  //   P(defDecl | tyDecl | termOrAssign, )
  // def toplvl[p: P]: P[Ls[Statement]] =
    // P(typeDefinition | let.map(_ :: Nil))
    // P(letS.map(_ :: Nil))
  // def pgrm[p: P]: P[Pgrm] = P((";".rep ~ toplvl ~ topLevelSep.rep).rep ~ End).map(stmts => Pgrm(stmts.flatten.toList))
  def topLevelSep[p: P]: P[Unit] = ";"
  // def pgrm[p: P]: P[Pgrm] = P((typeDefinition).map(Pgrm))

  private var curHash = 0
  def nextHash: Int = {
    val res = curHash
    curHash = res + 1
    res
  }

}
object OcamlParser {

  def addTopLevelSeparators(lines: IndexedSeq[Str]): IndexedSeq[Str] = {
    (lines.iterator ++ lines.lastOption).toList
      .sliding(2)
      .map {
        case l0 :: l1 :: Nil =>
          if (l1.startsWith(" ") || l1.startsWith("\t")) l0 + "\n"
          else l0 + ";"
        case l :: Nil => l
        case _        => die
      }
      .toIndexedSeq
  }
}
