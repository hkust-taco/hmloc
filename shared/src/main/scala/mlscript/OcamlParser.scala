package mlscript

import scala.util.chaining._
import scala.collection.mutable
import fastparse._, fastparse.ScalaWhitespace._
import mlscript.utils._, shorthands._
import mlscript.Lexer._

/** Parser for an ML-style input syntax, used in the legacy `ML*` tests. */
@SuppressWarnings(Array("org.wartremover.warts.All"))
class OcamlParser(origin: Origin, indent: Int = 0, recordLocations: Bool = true) {
  import OcamlParser.whitespace
  
  val keywords = Set(
    "def", "class", "trait", "type", "method", "mut",
    "let", "rec", "in", "fun", "with", "undefined", "null",
    "if", "then", "else", "match", "case", "of",
    // ocaml keywords
    "and"
    )
  def kw[p: P](s: String) = s ~~ !(letter | digit | "_" | "'")
  
  // NOTE: due to bug in fastparse, the parameter should be by-name!
  def locate[p:P, L <: Located](tree: => P[L]): P[L] = (Index ~~ tree ~~ Index).map {
    case (i0, n, i1) => n.withLoc(i0, i1, origin)
  }
  
  def toParams(t: Term): Tup = t match {
    case t: Tup => t
    case Bra(false, t) => toParams(t)
    case _ => Tup((N, Fld(false, false, t)) :: Nil)
  }
  def toParams(t: Ls[Term]): Tup = {
    val fields = t.map(t => (N, Fld(false, false, t)))
    Tup(fields)
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
  // ocaml operators adapted from https://v2.ocaml.org/manual/lex.html#operator-char
  def ocamlOps[p:P]: P[Var] = P("::").!.map(Var)
  def ident[p: P]: P[String] =
    P( (letter | "_") ~~ (letter | digit | "_" | "'").repX ).!.filter(!keywords(_))

  /** all top level statements and .
    * Note: this is will treat 1 = 2 as let 1 = 2 and not 1 == 2
    */
  def termOrAssign[p: P]: P[Statement] = P( term ~ ("=" ~ term).? ).map {
    case (expr, N) => expr
    case (pat, S(bod)) => LetS(false, pat, bod)
  }

  /** Top level term */
  def term[p: P]: P[Term] = P(let | fun | ite | ocamlWithAsc | _match)

  def lit[p: P]: P[Lit] =
    locate(number.map(x => IntLit(BigInt(x))) | Lexer.stringliteral.map(StrLit(_))
    | P(kw("undefined")).map(x => UnitLit(true)) | P(kw("null")).map(x => UnitLit(false)))
  def ocamlList[p: P]: P[Term] = P("[" ~ lit.rep(0, ",") ~ "]").map(vals => {
    // assumes that the standard library defining list
    // also defines a helper function to create lists
    val emptyList: Term = Var("Nil")
    vals.foldRight(emptyList)((v, list) =>
      mkApp(Var("Cons"), Tup((N, Fld(false, false, v)) :: (N, Fld(false, false, list)) :: Nil)
    ))
  })
  def variable[p: P]: P[Var] = locate(ident.map(Var))
  def ocamlLabelName[p: P]: P[Var] = locate((ident).map(Var) | "(" ~ ocamlOps ~ ")")

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

  def subtermNoSel[p: P]: P[Term] = P( parens | record | lit | variable | ocamlList )
  
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
  
  /** Parses an expression of the form `expr (: type) (= expr2)`. This is the
    * an important term parser because it is the one that actually parses the
    * sub terms
    * 
    * the type ascription is optional
    * 
    * The equality check is optional but needed for parsing ocaml style
    * equality check for terms as in the following examples
    * a [= 3]
    * b: List [= Cons(0, Nil)]
    *
    * Ocaml parses comma-separated terms as a tuple handle them differently
    */
  def ocamlWithAsc[p: P]: P[Term] = P(withs
    ~ (":" ~/ ty).rep  // ascription
    ~ ("=" ~/ term).?  // equality check
    ~ ("," ~/ withsAsc).?  // ocaml creates implicit tuples for comma separated terms
  ).map {
    case (withs, ascs, equateTerm, tupleTerm) =>
      val trm1 = ascs.foldLeft(withs)(Asc)
      val trm2 = equateTerm.fold(trm1)(App(OpApp("==", trm1), _))
      tupleTerm.fold(trm2)(trm3 => toParams(trm2 :: trm3))
  }
  /** Inner call to withsAsc term which parses a list of terms optionally
    * type ascribed
    * equated
    * comma-separated
    */
  def withsAsc[p: P]: P[Ls[Term]] = P(withs
    ~ (":" ~/ ty).rep  // ascription
    ~ ("=" ~/ term).?  // equality check
    ~ ("," ~/ withsAsc).?  // ocaml creates implicit tuples for comma separated terms
  ).map {
    case (withs, ascs, equateTerm, tupleTerm) =>
      val trm1 = ascs.foldLeft(withs)(Asc)
      val trm2 = equateTerm.fold(trm1)(App(OpApp("==", trm1), _))
      tupleTerm.fold(trm2 :: Nil)(trm2 :: _)
  }
  def withs[p: P]: P[Term] = P( binops ~ (kw("with") ~ record).rep ).map {
    case (as, ws) => ws.foldLeft(as)((acc, w) => With(acc, w))
  }
  
  /** Subsitute operators with functions to handle special cases in ocaml */
  def appSubstitution: PartialFunction[Term, Term] = {
    // substitute x :: xs with Cons(x, xs) recursively
    // while retaining location information
    // the representation will be (App(App(Var("::"), Tup(...)), Tup(...)))
    // the tups fields are further substituted in the recursive calls
    case App(App(op@Var("::"), lhs), rhs) =>
      val newLhs = appSubstitution(lhs)
      val newRhs = appSubstitution(rhs)
      App(op.copy(name = "Cons"), toParams(newLhs :: newRhs :: Nil))
    case t@Tup(N -> Fld(false, false, value) :: Nil) =>
      t.copy(fields = N -> Fld(false, false, appSubstitution(value)) :: Nil)
    case t@Tup(fields) =>
      val newFields = fields.map { case (v, fld) => (v, fld.copy(value = appSubstitution(fld.value))) }
      t.copy(fields = newFields)
    case t => t
  }
  def mkApp(lhs: Term, rhs: Term): Term = 
    App(appSubstitution(lhs), toParams(rhs))
  /** Parses where one or more subterms are applied to one subterm. It is used
    * in binops which is used in withs to parse terms.
    */
  def apps[p: P]: P[Term] = P( subterm.rep(1).map(_.reduce(mkApp)) )
  
  def _match[p: P]: P[If] =
    locate(P( kw("match") ~/ term ~ "with" ~ matchArms).map {
      case (matchVar, branches) =>
        val ifBlocks = IfBlock(branches.map(L.apply))
        If(IfOpApp(matchVar, Var("is"), ifBlocks), N)
    })
  def matchArms[p:P]: P[Ls[IfBody]] = P(
   (
    ("_" ~ "->" ~ term).map(IfElse(_) :: Nil)
    | (term ~ "->" ~ term ~ matchArms2).map {
        case (t, b, rest) =>
          IfThen(t, b) :: rest
      }
  ).?.map {
    case None => Ls.empty
    case Some(b) => b
  })
  def matchArms2[p: P]: P[Ls[IfBody]] = ("|" ~ matchArms).?.map(_.getOrElse(Ls.empty))

  private val infixity: Set[String] = Set("::", "<>")
  private val prec: Map[Char,Int] = List(
    "|",
    "^",
    "&",
    "= !",
    // cons operator higher precedence that relational operators
    ":",
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
              // right associative infix operators like `::` list cons
              if (infixity(op) && prec < minPrec) {
                remaining = remaining.tail
                val rhs = climb(prec + 1, next)
                result = mkApp(mkApp(Var(op).withLoc(off0, off1, origin), result), rhs)
                true
              }
              else if (prec < minPrec) false
              else {
                remaining = remaining.tail
                val rhs = climb(prec + 1, next)
                result = mkApp(mkApp(Var(op).withLoc(off0, off1, origin), result), rhs)
                true
              }
          }
        )()
        result
      }
      val appliedOp = climb(0, pre)
      appSubstitution(appliedOp)
    }
  def operator[p: P]: P[Unit] = P(
    !symbolicKeywords ~~ (!StringIn("/*", "//", "(*") ~~ (CharsWhile(OpCharNotSlash) | "/" | "*)")).rep(1)
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
  
  /** top level function defintions using let with single and multiple arguments
   *  let ap a b = a b
   *  let :: a b = Cons a b
  */
  def ocamlDefDecl[p: P]: P[Def] =
    locate(P((kw("let") ~ ocamlLabelName ~ tyParams ~ ":" ~/ ty map {
      case (id, tps, t) => Def(true, id, R(PolyType(tps, t)), true)
    }) | (kw("let") ~ kw("rec").!.?.map(_.isDefined) ~/ ocamlLabelName ~ subterm.rep ~ "=" ~ term map {
      case (rec, id, ps, bod) => Def(rec, id, L(ps.foldRight(bod)((i, acc) => Lam(toParams(i), acc))), true)
    })))
    
  /** Parse each ocaml type parameters like
   * 'a (string * int) list
   * and return the type variables and the type
  */
  def ocamlTypeBodyPart[p: P]: P[(Set[TypeName], Type)] = (
    tyName.map(tname => (Set.empty[TypeName], tname))
  | ocamlTyParam.map(param => (Set(param), param))
  | "(" ~ ocamlConstructorBody ~ ")"
  ).rep(1)
    .map {
      case Seq(b) => b
      case parts =>
        val tparams = parts.flatMap(_._1).toSet
        parts.last._2 match {
          case tname: TypeName =>
            val args = parts.init.toList.map(_._2).toList
            (tparams, AppliedType(tname, args))
          case t =>
            // last parameter has to be a type name when there are
            // multiple parameters being applied
            throw new Error(s"Parameters applied to non-TypeName $t")
        }
    }
  
  /** data constructor body
    * type 'a, 'b tup = Tup of ['a * 'b] => ('a, 'b)
    * type 'a, 'b tup = Tup of ['a list * 'b] => (List['a], 'b)
    * type 'a, 'b tup = Tup of ['a * 'b list] => ('a, List['b])
    * type 'a, 'b tup = Tup of [('a * 'b) list] => (List[('a, 'b)])
    */
  def ocamlConstructorBody[p: P]: P[(Set[TypeName], Record)] =
    ocamlTypeBodyPart.rep(1, "*")
    .map(args => {
      val fields = args.zipWithIndex.map {
        // Heap of [string]
        case ((isEmpty, tname), i) =>
          (Var("_" + i.toString()), Field(None, tname))
        // if the type name is the parameter itself don't apply it
        // Heap of ['a]
        case ((tparams, tname: TypeName), i) if tparams(tname) =>
          (Var("_" + i.toString()), Field(None, tname))
        // Heap of ['a list]
        case ((tparams, tname: TypeName), i) =>
          (Var("_" + i.toString()), Field(None, AppliedType(tname, tparams.toList)))
        // Heap of ['a * 'b list]
        case ((tparams, tbody), i) =>
          (Var("_" + i.toString()), Field(None, tbody))
      }.toList
      val tparams = args.flatMap(_._1).toSet
      (tparams, Record(fields))
    })
    
  /** data constructor declaration
   *  type 'a, 'b tup = [Tup of 'a * 'b]
   */
  def ocamlConstructorDecl[p: P]: P[TypeDef] =
    P((tyName ~ ("of" ~/ ocamlConstructorBody).?).map {
      case (id, Some((params, body))) =>
        val positionals = body.fields.map(_._1)
        TypeDef(Cls, id, params.toList, body, Nil, Nil, positionals)
      case (id, None)                 => TypeDef(Cls, id, Nil, Record(Ls.empty), Nil, Nil, Nil)
    })
    
  // https://v2.ocaml.org/manual/typedecl.html#ss:typedefs
  // TODO: handle record style type representation
  // type 'a lst = [Null | Cons of 'a * 'a lst]
  def ocamlTypeRepresentation[p: P]: P[List[TypeDef]] = ocamlConstructorDecl.rep(1, "|").map(_.toList)
  
  /** Parse type alias like heap in the example below
   * 
    * type heapVar = HeapInt of int | Heap of heap and
    *   [heap = (string * int) list] => type alias Heap = (string * int) list
    */
  def ocamlTyDeclTyAlias[p: P]: P[TypeDef] =
    P((tyName ~ "=" ~/ ocamlConstructorBody).map {
      case (nme, (alsParams, alsBody)) =>
        TypeDef(Als, nme, alsParams.toList, alsBody, Nil, Nil, Nil)
    })
  
  def tyKind[p: P]: P[TypeDefKind] = (kw("class") | kw("trait") | kw("type")).! map {
    case "class" => Cls
    case "trait" => Trt
    case "type"  => Als
  }
  /** Modified type declaration that parses type constructor
    * and data constructor and returns them all together as a
    * list
    */
  def ocamlTyDecl[p: P]: P[Ls[TypeDef]] =
    P((kw("type") ~/ ocamlTyParams ~ tyName ~ "=" ~/ ocamlTypeRepresentation ~ ("and" ~ ocamlTyDeclTyAlias).?)
      .map { case (tparams, tname, bodies, alias) =>
        val paramSet = tparams.toSet
        val unionType: TypeDef => Type = (tyDef) => {
          val appliedParams = tyDef.tparams.filter(paramSet(_))
          appliedParams match {
            case Nil => tyDef.nme
            case applied => AppliedType(tyDef.nme, applied)
          }
        }
        val initialBody = unionType(bodies(0))
        // TODO: apply if parameters are common to alias
        // otherwise just use type name
        val aliasBody = bodies.foldLeft[Type](initialBody){
          case (union, tdef) => Union(unionType(tdef), union)
        }
        val result = TypeDef(Als, tname, tparams, aliasBody, Nil, Nil, Nil) :: bodies
        alias.map(als => als :: result).getOrElse(result)
    })
  // create a helper function for a class constructor
  // for e.g. Cons[A] = {_0: A; _1: List[A]} gets
  // def Cons (a, b) = Cons {_0 = a; _1 = b}
  def ocamlTyDeclHelper(tyDef: TypeDef): Opt[Def] = {
    tyDef.kind match {
      case Cls => {
        tyDef.body match {
          case Record(Nil) =>
            val funApp = mkApp(Var(tyDef.nme.name), Rcd(Nil))
            val fun = Def(false, Var(tyDef.nme.name), L(funApp), true)
            S(fun)
          case Record(fields) =>
            val numArgs = fields.length
            val args = Range(0, numArgs).map(i => Var((97+i).toChar.toString)).toList
            val funBody = Rcd(args.zipWithIndex.map{ case (arg, i) => Var("_" + i.toString) -> Fld(false, false, arg) })
            val funApp = mkApp(Var(tyDef.nme.name), funBody)
            val fun = Def(false, Var(tyDef.nme.name), L(Lam(toParams(args), funApp)), true)
            S(fun)
          case _ => N
        }
      }
      case _ => N
    }
  }
  def ocamlTyDeclAndHelper[p:P]: P[Ls[Statement]] = ocamlTyDecl.map(tyDefs => {
    // only create helpers for classes
    val helpers = tyDefs.flatMap(ocamlTyDeclHelper)
    tyDefs ::: helpers
  })
  def tyParams[p: P]: P[Ls[TypeName]] =
    ("[" ~ tyName.rep(0, ",") ~ "]").?.map(_.toList.flatten)
  def ocamlTyParams[p: P]: P[Ls[TypeName]] =
    (ocamlTyParam.rep(0, ",")).?.map(_.toList.flatten)
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
  def ocamlTyParam[p: P]: P[TypeName] = locate(P(("'" ~~ lowercase.!).map(param =>
      TypeName("'" + param)
    )))
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

  /** Exceptions are definitions returning empty records
    * Raising them is not relevant to the type checking flow so the raise is defined
    * as raise: anything -> nothing and simply consumes the exception
    */
  def ocamlExceptionDef[p: P]: P[Def] = P(
    "exception" ~ ident
  ).map(name => Def(false, Var(name), L(Rcd(Nil)), false))
  def toplvl[p: P]: P[Ls[Statement]] =
    P(ocamlDefDecl.map(_ :: Nil) | ocamlTyDeclAndHelper | ocamlExceptionDef.map(_ :: Nil) | termOrAssign.map(_ :: Nil))
  def pgrm[p: P]: P[Pgrm] = P( (";".rep ~ toplvl ~ topLevelSep.rep).rep.map(_.toList.flatten) ~ End ).map(Pgrm)
  def topLevelSep[p: P]: P[Unit] = ";"
}

object OcamlParser {
  
  import scala.annotation.{Annotation, switch, tailrec}
  import fastparse.internal.Util
  
  // scala style white space parser modified to handle ocaml style comments too
  implicit val whitespace: ParsingRun[_] => ParsingRun[Unit] = {implicit ctx: ParsingRun[_] =>
    val input = ctx.input
    val startIndex = ctx.index
    @tailrec def rec(current: Int, state: Int, nesting: Int): ParsingRun[Unit] = {
      if (!input.isReachable(current)) {
        if (state === 0 || state === 1) ctx.freshSuccessUnit(current)
        else if(state === 2 && nesting === 0) ctx.freshSuccessUnit(current - 1)
        else {
          ctx.cut = true
          val res = ctx.freshFailure(current)
          if (ctx.verboseFailures) ctx.setMsg(startIndex, () => Util.literalize("*/ or *)"))
          res
        }
      } else {
        val currentChar = input(current)
        (state: @switch) match{
          case 0 =>
            (currentChar: @switch) match{
              case ' ' | '\t' | '\n' | '\r' => rec(current + 1, state, 0)
              case '/' => rec(current + 1, state = 2, 0)
              case '(' => rec(current + 1, state = 2, 0)  // ocaml comments are like "(* ... *)"
              case _ => ctx.freshSuccessUnit(current)
            }
          case 1 => rec(current + 1, state = if (currentChar === '\n') 0 else state, 0)
          case 2 =>
            (currentChar: @switch) match{
              case '/' =>
                if (nesting === 0) rec(current + 1, state = 1, 0)
                else rec(current + 1, state = 2, nesting)
              case '*' => rec(current + 1, state = 3, nesting + 1)
              case _ =>
                if (nesting === 0) ctx.freshSuccessUnit(current - 1)
                else rec(current + 1, state = 3, nesting)
            }
          case 3 =>
            (currentChar: @switch) match{
              case '/' => rec(current + 1, state = 2, nesting)
              case '*' => rec(current + 1, state = 4 , nesting)
              case _ => rec(current + 1, state = state, nesting)
            }
          case 4 =>
            (currentChar: @switch) match{
              case '/' => rec(current + 1, state = if (nesting === 1) 0 else 3 , nesting - 1)
              // ocaml comments are like "(* ... *)"
              case ')' => rec(current + 1, state = if (nesting === 1) 0 else 3, nesting - 1)
              case '*' => rec(current + 1, state = 4, nesting)
              case _ => rec(current + 1, state = 3, nesting)
            }
        }
      }
    }
    rec(current = ctx.index, state = 0, nesting = 0)
  }
  
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
