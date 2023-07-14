package hmloc

import hmloc.Message._
import hmloc.utils._
import hmloc.utils.shorthands._

import scala.collection.mutable
import scala.collection.mutable.{Map => MutMap}

/** A class encapsulating type inference state.
 *  It uses its own internal representation of types and type variables, using mutable data structures.
 *  Inferred SimpleType values are then turned into CompactType values for simplification.
 */
class Typer(var dbg: Boolean, var verbose: Bool, var explainErrors: Bool)
    extends TypeDefs with TypeSimplifier {
  
  type Raise = Diagnostic => Unit
  type Binding = Str -> TypeScheme
  type Bindings = Map[Str, TypeScheme]
  
  /**  `env`: maps the names of all global and local bindings to their types
    *  Keys of `mthEnv`:
    * `L` represents the inferred types of method definitions. The first value is the parent name,
    *   and the second value is the method name.
    * `R` represents the actual method types.
    *   The first optional value is the parent name, with `N` representing implicit calls,
    *   and the second value is the method name.
    * The public helper functions should be preferred for manipulating `mthEnv`
   */
  case class Ctx(
                  parent: Opt[Ctx],
                  env: MutMap[Str, VarSymbol],
                  lvl: Int,
                  inPattern: Bool,
                  tyDefs: Map[Str, TypeDef],
  ) {
    def +=(b: Str -> VarSymbol): Unit = env += b
    def ++=(bs: IterableOnce[Str -> VarSymbol]): Unit = bs.iterator.foreach(+=)
    def get(name: Str): Opt[VarSymbol] = env.get(name) orElse parent.dlof(_.get(name))(N)
    def contains(name: Str): Bool = env.contains(name) || parent.exists(_.contains(name))
    def nest: Ctx = copy(Some(this), MutMap.empty)
    def nextLevel: Ctx = copy(lvl = lvl + 1)
    private val abcCache: MutMap[Str, Set[TypeName]] = MutMap.empty
  }
  object Ctx {
    def init: Ctx = Ctx(
      parent = N,
      env = MutMap.from(builtinBindings.iterator.map(nt => nt._1 -> VarSymbol(nt._2, Var(nt._1)))),
      lvl = 0,
      inPattern = false,
      tyDefs = Map.from(builtinTypes.map(t => t.nme.name -> t)),
    )
    val empty: Ctx = init
  }
  implicit def lvl(implicit ctx: Ctx): Int = ctx.lvl

  import TypeProvenance.{apply => tp}
  import sourcecode._
  def ttp(trm: Term, desc: Str = "")(implicit file: FileName, line: Line): TypeProvenance =
    TypeProvenance(trm.toLoc, if (desc === "") trm.describe else desc)

  def originProv(loco: Opt[Loc], desc: Str, name: Str): TypeProvenance = {
    tp(loco, desc, S(name), isType = true)
    // ^ If we did not treat "origin provenances" differently,
    //    it would yields unnatural errors like:
      //│ ╟── expression of type `B` is not a function
      //│ ║  l.6: 	    method Map[B]: B -> A
      //│ ║       	               ^
    // So we should keep the info but not shadow the more relevant later provenances
  }

  object NoProv extends TypeProvenance(N, "expression") {
    override def toString: Str = "[NO PROV]"
  }
  def noProv: TypeProvenance = NoProv
  def noTyProv: TypeProvenance = TypeProvenance(N, "type", isType = true)
  
  val TopType: ExtrType = ExtrType(false)(noTyProv)
  val BotType: ExtrType = ExtrType(true)(noTyProv)
  val UnitType: TypeRef = TypeRef(TypeName("unit"), Nil)(noTyProv)
  val BoolType: TypeRef = TypeRef(TypeName("bool"), Nil)(noTyProv)
  val IntType: TypeRef = TypeRef(TypeName("int"), Nil)(noTyProv)
  val FloatType: TypeRef = TypeRef(TypeName("float"), Nil)(noTyProv)
  val StringType: TypeRef = TypeRef(TypeName("string"), Nil)(noTyProv)

  val builtinTypes: Ls[TypeDef] =
    TypeDef(Cls, TypeName("int"), Nil, TopType, N, S(TypeName("int")->Nil)) ::
    TypeDef(Cls, TypeName("number"), Nil, TopType, N, S(TypeName("number")->Nil)) ::
    TypeDef(Cls, TypeName("bool"), Nil, TopType, N, S(TypeName("bool")->Nil)) ::
    TypeDef(Cls, TypeName("true"), Nil, TopType, N, S(TypeName("bool")->Nil)) ::
    TypeDef(Cls, TypeName("false"), Nil, TopType, N, S(TypeName("bool")->Nil)) ::
    TypeDef(Cls, TypeName("string"), Nil, TopType, N, S(TypeName("string")->Nil)) ::
    TypeDef(Als, TypeName("anything"), Nil, TopType, N) ::
    TypeDef(Als, TypeName("nothing"), Nil, BotType, N) ::
    TypeDef(Cls, TypeName("error"), Nil, TopType, N) ::
    TypeDef(Cls, TypeName("float"), Nil, TopType, N, S(TypeName("float")->Nil)) ::
    TypeDef(Cls, TypeName("unit"), Nil, TopType, N) :: {
      val listTyVar: TypeVariable = freshVar(noProv, S("'a"))(1)
      val td = TypeDef(Cls, TypeName("list"), Ls((TypeName("A"), listTyVar)), TopType, N, S(TypeName("list"), Nil))
      td.tvarVariances = S(MutMap(listTyVar -> VarianceInfo.co))
      td
    } ::
    // Dummy class declaration to store adt info
    // actual definition is given in bindings
    TypeDef(Cls, TypeName("Cons"), Nil, TopType, N, S(TypeName("list") -> Ls(0))) ::
    TypeDef(Cls, TypeName("Nil"), Nil, TopType, N, S(TypeName("list") -> Nil)) ::
    Nil

  val primitiveTypes: Set[Str] =
    builtinTypes.iterator.map(_.nme.name).toSet

  // built in operators bound to their type schemes
  val builtinBindings: Bindings = {
    import FunctionType.{apply => fun}
    Map(
      "true" -> BoolType,
      "false" -> BoolType,
      "eq" -> {
        val v = freshVar(noProv)(1)
        PolymorphicType(0, fun(v, fun(v, BoolType)(noProv))(noProv))
      },
      "error" -> BotType,
      "Cons" -> {
        val listTyVar: TypeVariable = freshVar(noProv, S("'a"))(1)
        val ListType: TypeRef = TypeRef(TypeName("list"), Ls(listTyVar))(noTyProv)
        val args = TupleType(Ls(N -> listTyVar, N -> ListType))(noTyProv)
        PolymorphicType(0, fun(args, ListType)(noProv))
      },
      "Nil" -> {
        val listTyVar: TypeVariable = freshVar(noProv, S("'a"))(1)
        val ListType: TypeRef = TypeRef(TypeName("list"), Ls(listTyVar))(noTyProv)
        PolymorphicType(0, ListType)
      },
      "::" -> {
        val listTyVar: TypeVariable = freshVar(noProv, S("'a"))(1)
        val ListType: TypeRef = TypeRef(TypeName("list"), Ls(listTyVar))(noTyProv)
        PolymorphicType(0, fun(listTyVar, fun(ListType, ListType)(noProv))(noProv))
      },
    )
  }

  /* Parameters `vars` and `newDefsInfo` are used for typing `TypeName`s.
   * If the key is found in `vars`, the type is typed as the associated value. Use case: type arguments.
   * If the key is found in `newDefsInfo`, the type is typed as a `TypeRef`, where the associated value
   *   is used to check the kind of the definition and the number of type arguments expected. Use case:
   *   for typing bodies of type definitions with mutually recursive references. */
  def typeType(ty: Type, simplify: Bool = true)
        (implicit ctx: Ctx, raise: Raise, vars: Map[Str, SimpleType]): SimpleType =
    typeType2(ty, simplify)._1
  
  /* Also returns an iterable of `TypeVariable`s instantiated when typing `TypeVar`s.
   * Useful for instantiating them by substitution when expanding a `TypeRef`. */
  def typeType2(ty: Type, simplify: Bool = true)
        (implicit ctx: Ctx, raise: Raise, vars: Map[Str, SimpleType],
        newDefsInfo: Map[Str, (TypeDefKind, Int)] = Map.empty): (SimpleType, Iterable[TypeVariable]) =
      trace(s"$lvl. Typing type ${ty.show}") {
    println(s"vars=$vars newDefsInfo=$newDefsInfo")
    def typeNamed(loc: Opt[Loc], name: Str): (() => ST) \/ (TypeDefKind, Int) =
      newDefsInfo.get(name)
        .orElse(ctx.tyDefs.get(name).map(td => (td.kind, td.tparamsargs.size)))
        .toRight(() => err("type identifier not found: " + name, loc)(raise))
    val localVars = mutable.Map.empty[TypeVar, TypeVariable]
    def tyTp(loco: Opt[Loc], desc: Str, originName: Opt[Str] = N) =
      TypeProvenance(loco, desc, originName, isType = true)
    def rec(ty: Type)(implicit ctx: Ctx, recVars: Map[TypeVar, TypeVariable]): SimpleType = ty match {
      case Top => ExtrType(false)(tyTp(ty.toLoc, "top type"))
      case Bot => ExtrType(true)(tyTp(ty.toLoc, "bottom type"))
      case Tuple(fields) =>
        TupleType(fields.map(fld =>
            N -> rec(fld).withProv(tp(fld.toLoc, "tuple field"))
          ))(tyTp(ty.toLoc, "tuple type"))
      case Inter(lhs, rhs) => (if (simplify) rec(lhs) & (rec(rhs), _: TypeProvenance)
          else ComposedType(false, rec(lhs), rec(rhs)) _
        )(tyTp(ty.toLoc, "intersection type"))
      case Union(lhs, rhs) => (if (simplify) rec(lhs) | (rec(rhs), _: TypeProvenance)
          else ComposedType(true, rec(lhs), rec(rhs)) _
        )(tyTp(ty.toLoc, "union type"))
      case Function(lhs, rhs) => FunctionType(rec(lhs), rec(rhs))(tyTp(ty.toLoc, "function type"))
      case TypeName("this") =>
        ctx.env.getOrElse("this", err(msg"undeclared this" -> ty.toLoc :: Nil)) match {
          case VarSymbol(t: TypeScheme, _) => t.instantiate
        }
      case tn @ TypeTag(name) => rec(TypeName(name))
      case tn @ TypeName(name) =>
        val tyLoc = ty.toLoc
        val tpr = tyTp(tyLoc, "type reference")
        vars.getOrElse(name, {
          typeNamed(tyLoc, name) match {
            case R((_, tpnum)) =>
              if (tpnum =/= 0) {
                err(msg"Type $name takes parameters", tyLoc)(raise)
              } else TypeRef(tn, Nil)(tpr)
            case L(e) => e()
          }
        })
      case tv: TypeVar =>
        // assert(ty.toLoc.isDefined)
        recVars.getOrElse(tv,
          localVars.getOrElseUpdate(tv, freshVar(noProv, tv.identifier.toOption))
            .withProv(tyTp(ty.toLoc, "type variable")))
      case AppliedType(base, targs) =>
        val prov = tyTp(ty.toLoc, "applied type reference")
        typeNamed(ty.toLoc, base.name) match {
          case R((_, tpnum)) =>
            val realTargs = if (targs.size === tpnum) targs.map(rec) else {
              err(msg"Wrong number of type arguments – expected ${tpnum.toString}, found ${
                  targs.size.toString}", ty.toLoc)(raise)
              (targs.iterator.map(rec) ++ Iterator.continually(freshVar(noProv))).take(tpnum).toList
            }
            TypeRef(base, realTargs)(prov)
          case L(e) => e()
        }
    }
    (rec(ty)(ctx, Map.empty), localVars.values)
  }(r => s"=> ${r._1} | ${r._2.mkString(", ")}")
  
  def typePattern(pat: Term)(implicit ctx: Ctx, raise: Raise, vars: Map[Str, SimpleType] = Map.empty): SimpleType =
    typeTerm(pat)(ctx.copy(inPattern = true), raise, vars)
  
  
  def typeStatement(s: Statement, allowPure: Bool)
        (implicit ctx: Ctx, raise: Raise): PolymorphicType \/ Ls[Binding] = s match {
    case Def(false, Var("_"), L(rhs), isByname) => typeStatement(rhs, allowPure)
    case Def(isrec, nme, L(rhs), isByname) => // TODO reject R(..)
      if (nme.name === "_")
        err(msg"Illegal definition name: ${nme.name}", nme.toLoc)(raise)
      val ty_sch = typeLetRhs(isrec, nme, rhs)
      nme.uid = S(nextUid)
      ctx += nme.name -> VarSymbol(ty_sch, nme)
      R(nme.name -> ty_sch :: Nil)
    case t @ Tup(fs) if !allowPure => // Note: not sure this is still used!
      warn(s"Useless tuple in statement position.", t.toLoc)
      L(PolymorphicType(0, typeTerm(t)))
    case t: Term =>
      val ty = typeTerm(t)
      if (!allowPure) {
        if (t.isInstanceOf[Var] || t.isInstanceOf[Lit])
          warn("Pure expression does nothing in statement position.", t.toLoc)
        else
          uniState.unify(mkProv(ty, TypeProvenance(t.toCoveringLoc, "expression in statement position")), UnitType)
      }
      L(PolymorphicType(0, ty))
    case _ =>
      err(msg"Illegal position for this ${s.describe} statement.", s.toLoc)(raise)
      R(Nil)
  }
  
  /** Infer the type of a let binding right-hand side. */
  def typeLetRhs(isrec: Boolean, nme: Var, rhs: Term)(implicit ctx: Ctx, raise: Raise,
      vars: Map[Str, SimpleType] = Map.empty): PolymorphicType = {
    val res = if (isrec) {
      val e_ty = freshVar(
        // It turns out it is better to NOT store a provenance here,
        //    or it will obscure the true provenance of constraints causing errors
        //    across recursive references.
        // noProv,
        // TypeProvenance(rhs.toLoc, "let-bound value"),
        // TypeProvenance(rhs.toLoc, "let-bound value"),
        TypeProvenance(nme.toLoc, "recursive binding"),
        S(nme.name)
      )(lvl + 1)
      ctx += nme.name -> VarSymbol(e_ty, nme)
      val ty = typeTerm(rhs)(ctx.nextLevel, raise, vars)
      uniState.unify(ty, e_ty)
      e_ty
    } else typeTerm(rhs)(ctx.nextLevel, raise, vars)
    PolymorphicType(lvl, res)
  }

  def mkProv(ty: SimpleType, prov: TypeProvenance): SimpleType = ProvType(ty)(prov)

  // TODO also prevent rebinding of "not"
  val reservedNames: Set[Str] = Set("|", "&", "~", ",", "neg", "and", "or")

  object ValidVar {
    def unapply(v: Var)(implicit raise: Raise): S[Str] = S {
      if (reservedNames(v.name))
        err(s"Illegal use of ${if (v.name.head.isLetter) "keyword" else "operator"}: " + v.name,
          v.toLoc)(raise)
      v.name
    }
  }

  object ValidPatVar {
    def unapply(v: Var)(implicit ctx: Ctx, raise: Raise): Opt[Str] =
      if (ctx.inPattern && v.isPatVar) {
        ctx.parent.dlof(_.get(v.name))(N) |>? {
          case S(VarSymbol(ts: TypeScheme, _)) => ts.instantiate(0).unwrapProvs
        }
        ValidVar.unapply(v)
      } else N
  }
  
  /**
    * Pass no prov in the case of application expression. This is to ensure
    * those locations don't get double counted when simplifying error
    * messages.
    *
    * @param p
    * @return
    */
  def hintProv(p: TP): TP = noProv
  
  /** Infer the type of a term. */
  def typeTerm(term: Term, desc: String = "")(implicit ctx: Ctx, raise: Raise, vars: Map[Str, SimpleType] = Map.empty): SimpleType
        = trace(s"$lvl. Typing ${if (ctx.inPattern) "pattern" else "term"} $term ${term.getClass.getSimpleName}") {
    // implicit val prov: TypeProvenance = ttp(term)
    implicit def prov(implicit file: FileName, line: Line): TypeProvenance = ttp(term, desc)
    
    /** Constrain lhs and rhs type and handle errors if any
      *
      * @param lhs
      * @param rhs
      * @param res
      * @return
      */
    def con(lhs: SimpleType, rhs: SimpleType, res: SimpleType): SimpleType = {
      uniState.unify(lhs, rhs)
      res
    }
    term match {
      case v @ Var("_") =>
        if (ctx.inPattern) freshVar(tp(v.toLoc, "wildcard"))
        else err(msg"Widlcard in expression position.", v.toLoc)
      case Asc(trm, ty) =>
        val trm_ty = typeTerm(trm)
        val ty_ty = typeType(ty)(ctx.copy(inPattern = false), raise, vars)
        con(trm_ty, ty_ty, ty_ty)
        if (ctx.inPattern)
          con(ty_ty, trm_ty, ty_ty) // In patterns, we actually _unify_ the pattern and ascribed type 
        else ty_ty
      case (v @ ValidPatVar(nme)) =>
//        val prov = tp(if (verboseConstraintProvenanceHints) v.toLoc else N, "variable")
        val prov = tp(v.toLoc, "variable")
        // Note: only look at ctx.env, and not the outer ones!
        ctx.env.get(nme).collect { case VarSymbol(ts, dv) => assert(v.uid.isDefined); v.uid = dv.uid; ts.instantiate }
          .getOrElse {
            val res = freshVar(prov)(lvl)
            v.uid = S(nextUid)
            ctx += nme -> VarSymbol(res, v)
            res
          }
      case v @ ValidVar(name) =>
        val ty = ctx.get(name).fold(err("identifier not found: " + name, term.toLoc): TypeScheme) {
          case VarSymbol(ty: TypeScheme, _) => ty
        }.instantiate
        mkProv(ty, prov)
        // ^ TODO maybe use a description passed in param?
        // currently we get things like "flows into variable reference"
        // but we used to get the better "flows into object receiver" or "flows into applied expression"...
      case _: IntLit => IntType.withProv(prov)
      case _: StrLit => StringType.withProv(prov)
      case _: DecLit => FloatType.withProv(prov)
      case Tup(fs) =>
        TupleType(fs.map { t =>
          val tym = typeTerm(t)
          val fprov = tp(t.toLoc, "tuple field")
          N -> tym.withProv(fprov)
        })(tp(term.toLoc, "tuple literal"))
        // TODO is this supported in ocaml
      case pat if ctx.inPattern =>
        err(msg"Unsupported pattern shape${
          if (dbg) " ("+pat.getClass.toString+")" else ""}:", pat.toLoc)(raise)
      case Lam(pat, body) =>
        val newCtx = ctx.nest
        val param_ty = typePattern(pat)(newCtx, raise, vars)
        val body_ty = typeTerm(body)(newCtx, raise, vars)
        FunctionType(param_ty, body_ty)(tp(term.toLoc, "function"))
      case App(f, a) =>
        // version 2 simplified style
        val fun_ty = typeTerm(f)
        val arg_ty = typeTerm(a)
        val funProv = tp(f.toCoveringLoc, "applied expression")
        def go(f_ty: ST): ST = f_ty.unwrapProvs match {
          case FunctionType(l, r) =>
            con(arg_ty, l, r.withProv(prov))
          case _ =>
            val res = freshVar(prov, N)
            val resTy = con(fun_ty, FunctionType(arg_ty, res)(
              // prov
              funProv // TODO: better?
            ), res)
            resTy
        }
        go(fun_ty)
      case Let(isrec, nme, rhs, bod) =>
        val n_ty = typeLetRhs(isrec, nme, rhs)
        val newCtx = ctx.nest
        newCtx += nme.name -> VarSymbol(n_ty, nme)
        typeTerm(bod)(newCtx, raise)
      case Blk(stmts) => typeTerms(stmts, false, Nil)(ctx.nest, raise, prov)
      case iff @ If(cond, body) =>
        println(PrettyPrintHelper.inspect(iff))
        body match {
          // handle this case separately for better error messages
          case Ls(IfThen(Var("true"), trueArm), IfThen(Var("false"), falseArm)) =>
            val cond_ty = typeTerm(cond, "if-then-else condition")
            con(cond_ty, BoolType, cond_ty)
            val ret_ty = freshVar(prov.copy(desc = "if-then-else expression"))
            con(typeTerm(trueArm, "`then` branch"), ret_ty, ret_ty)
            con(typeTerm(falseArm, "`else` branch"), ret_ty, ret_ty)
          case _ =>
            println(s"typed condition term ${cond}")
            val cond_ty = typeTerm(cond)
            val ret_ty = if (body.length === 1) {
              freshVar(prov.copy(desc = "let expression"))
            } else {
              freshVar(prov.copy(desc = "match expression"))
            }

            // cache type argument variables for adts
            // so as to reuse them for each case expression
            // that has the same adt type
            val adtCache: MutMap[Str, Ls[TV]] = MutMap()

            // the assumed shape of an IfBody is a List[IfThen, IfThen, IfElse] with an optional IfElse at the end
            body.foreach {
              case IfElse(expr) => con(typeTerm(expr), ret_ty, ret_ty)
              case IfThen(Var("_"), rhs) => con(typeTerm(rhs), ret_ty, ret_ty)
              // `case x -> expr`
              // `case End -> expr` or `case false -> expr`
              case IfThen(v@Var(name), rhs) =>
                println(s"type pattern $v with loc: ${v.toLoc}")
                // update context with variables
                ctx.tyDefs.get(name).map {
                  case tdef if tdef.adtData.isDefined => (tdef.nme, tdef.adtData.getOrElse(die))
                }.fold {
                  // `case x -> expr` catch all with a new variable in the context
                  println(s"catch all $v")
                  val nestCtx = ctx.nest
                  nestCtx += name -> VarSymbol(cond_ty, v)
                  nestCtx |> { implicit ctx =>
                    con(typeTerm(rhs), ret_ty, ret_ty)
                  }
                } {
                  // `case End -> expr` or `case false -> expr` or `case [] -> expr`
                  // where case is a variant of an adt with no type arguments
                  case (_, (adtName, Nil)) =>
                    // get adt from cache or initialize a new one with fresh vars
                    // this is so that all case expressions can share
                    // the same type variables for the adt
                    val newTargs = adtCache.getOrElseUpdate(
                      adtName.name,
                      ctx.tyDefs.getOrElse(adtName.name, lastWords(s"Could not find ${adtName}"))
                        .targs.map(tv => freshVar(tv.prov, tv.nameHint))
                    )
                    println(s"pattern is adt: $adtName with $newTargs")
                    val adt_ty = TypeRef(adtName, newTargs)(TypeProvenance(v.toLoc, "pattern"))
                      .withProv(TypeProvenance(cond.toLoc, "match `condition`"))
                    con(cond_ty, adt_ty, cond_ty)
                    con(typeTerm(rhs), ret_ty, ret_ty)
                }
              case IfThen(tup@Tup(fs), rhs) =>
                println(s"fields $fs")
                val tupArgs = adtCache.getOrElseUpdate("Tup" + fs.length.toString, fs.map(_ => freshVar(noProv)))

                // initialize new tuple and fields with type variables and appropriate provenances
                val newTargs = tupArgs.zip(fs).zipWithIndex.map {
                  case ((tvar, fld), i) => tvar.withProv(TypeProvenance(fld.toLoc, s"${i} element of tuple"))
                }
                val fld_ty = newTargs.map(elem => N -> elem)
                val caseAdtTyp = TypeProvenance(tup.toLoc, "pattern")
                val adt_ty = TupleType(fld_ty)(caseAdtTyp)
                  .withProv(TypeProvenance(cond.toLoc, "match `condition`"))

                // constrain tuple fields or add names to context
                // add any vars to nested context pattern
                val nestCtx = ctx.nest
                fs.zipWithIndex.foreach {
                  // case (x, y)
                  case (argTerm: Var, fieldIdx) =>
                    println(s"Typing $argTerm field $fieldIdx in tup")
                    val fieldType = newTargs(fieldIdx)
                    println(s"Field $argTerm : $fieldType")
                    nestCtx += argTerm.name -> VarSymbol(fieldType, argTerm)
                  // case (0, 1)
                  case (argTerm, fieldIdx) =>
                    val fieldType = newTargs(fieldIdx)
                    val argTy = typeTerm(argTerm)
                    con(fieldType, argTy, fieldType)
                }

                con(cond_ty, adt_ty, adt_ty)
                nestCtx |> { implicit ctx =>
                  con(typeTerm(rhs), ret_ty, ret_ty)
                }
              // case Left x -> expr or Left 2 -> expr the type variable for the adt is constrained
              // with the argument to the constructor Left in this case
              case IfThen(caseAdt@App(Var(ctorNme), fs), rhs) =>
                println(s"Typing case ($ctorNme)")
                // override global adt_ty and args with custom
                val (_, (adtName, argsPos)) = ctx.tyDefs.get(ctorNme).collect {
                  case tdef if tdef.adtData.isDefined => (tdef.nme, tdef.adtData.getOrElse(die))
                }.getOrElse(lastWords(s"$ctorNme does not have adt data"))
                println(s"adt name $adtName and args pos $argsPos")
                // get adt from cache or initialize a new one with fresh vars
                // this is so that all case expressions can share
                // the same type variables for the adt
                val newTargs = adtCache.getOrElseUpdate(
                  adtName.name,
                  ctx.tyDefs.getOrElse(adtName.name, lastWords(s"Could not find $adtName"))
                    .targs.map(tv => freshVar(tv.prov, tv.nameHint))
                )

                val fields = fs match {
                  case t: Tup => t.fields
                  case t => t :: Nil
                }
                println(s"fields $fs ~> $fields")

                // find constructor type and map field types
                val ctorType = ctx.get(ctorNme) match {
                  case S(VarSymbol(PolymorphicType(_, res), _)) => res
                  case _ => die
                }
                println(s"ctor type: $ctorType")
                val (originalFieldTypes, ctorTargs) = ctorType.unwrapProvs match {
                  case FunctionType(fieldTy, TypeRef(defn, targs)) =>
                    assert(defn === adtName)
                    (fieldTy match {
                      case TupleType(fs) => fs.map(_._2)
                      case ty => ty :: Nil
                    }) -> targs.map {
                      case tv: TV => tv
                      case _ => die
                    }
                  case _ => die
                }
                assert(ctorTargs.sizeCompare(newTargs) === 0)
                val mapping = ctorTargs.zip(newTargs).toMap[ST, ST]
                val fieldTypes = originalFieldTypes.map { ft => subst(ft, mapping) }
                println(s"fieldTypes: $fieldTypes")

                val caseAdtTyp = TypeProvenance(caseAdt.toLoc, "pattern")
                val adt_ty = TypeRef(adtName, newTargs)(caseAdtTyp)
                  .withProv(TypeProvenance(cond.toLoc, "match `condition`"))
                con(cond_ty, adt_ty, cond_ty)
                println(s"adt_ty $adt_ty")

                val nestCtx = ctx.nest
                // type each match arm field and constraint with adt type variable
                // add any vars to nested context pattern
                fields.zipWithIndex.foreach {
                  // in case of Left x also add x to nested scope
                  case (argTerm: Var, fieldIdx) =>
                    println(s"Typing field ($fieldIdx) $argTerm")
                    val fieldType = fieldTypes(fieldIdx)
                    println(s"Field $argTerm : $fieldType")
                    nestCtx += argTerm.name -> VarSymbol(fieldType, argTerm)
                  // in case of 0 :: tl the type of list must be constrained with 0
                  case (argTerm, fieldIdx) =>
                    val fieldType = fieldTypes(fieldIdx)
                    val argTy = typeTerm(argTerm)
                    con(fieldType, argTy, fieldType)
                }

                // constraint match arm type with the return type of match expression
                nestCtx |> { implicit ctx =>
                  con(typeTerm(rhs), ret_ty, ret_ty)
                }
              // case 1 -> parsed as IntLit(1)
              // case "hi" -> parsed as StrLit("hi")
              // and others
              case IfThen(expr, rhs) =>
                con(cond_ty, typeTerm(expr), cond_ty)
                con(typeTerm(rhs), ret_ty, ret_ty)
              case ifbody =>
                lastWords(s"Cannot handle pattern ${ifbody} tree: ${PrettyPrintHelper.inspect(ifbody)}")
            }
            ret_ty
        }
      case _ => lastWords(s"Cannot type term: ${PrettyPrintHelper.inspect(term)}")
    }
  }(r => s"$lvl. : ${r}")

  def typeTerms(term: Ls[Statement], rcd: Bool, fields: List[Opt[Var] -> SimpleType])
        (implicit ctx: Ctx, raise: Raise, prov: TypeProvenance): SimpleType
      = term match {
    case (trm @ Var(nme)) :: sts if rcd => // field punning
      typeTerms(Tup(trm :: Nil) :: sts, rcd, fields)
    case Blk(sts0) :: sts1 => typeTerms(sts0 ::: sts1, rcd, fields)
    case Tup(Nil) :: sts => typeTerms(sts, rcd, fields)
    case Tup(trm :: ofs) :: sts =>
      val ty = typeTerm(trm)
      typeTerms(Tup(ofs) :: sts, rcd, (N -> ty) :: fields)
    case (trm: Term) :: Nil =>
      if (fields.nonEmpty)
        warn("Previous field definitions are discarded by this returned expression.", trm.toLoc)
      typeTerm(trm)
    case s :: sts =>
      val newBindings = typeStatement(s, allowPure = false).toOption
      ctx ++= newBindings.iterator.flatten.map(nt => nt._1 -> VarSymbol(nt._2, Var(nt._1)))
      typeTerms(sts, rcd, fields)
    case Nil => TupleType(fields.reverseIterator.toList)(prov)
  }

  /** Convert an inferred SimpleType into the immutable Type representation using Unification information. */
  def expandUnifiedType(st: SimpleType, stopAtTyVars: Bool = false, showTV: Set[TV] = Set(), ocamlStyle: Bool = false)(implicit ctx: Ctx): Type = {
    var unified: Ls[TypeVar -> Ls[Type]] = Nil

    val seenVars = mutable.Set.empty[TV]

    def go(st: SimpleType)(implicit cache: Set[TV]): Type =
    // trace(s"expand $st") {
      st.unwrapProvs match {
        case tv: TypeVariable if ocamlStyle =>
          if (showTV(tv)) {
            tv.asTypeVar
          } else {
            tv.asTypeVar
              .copy(nameHint = S("_"))
          }
        case tv: TypeVariable if stopAtTyVars => tv.asTypeVar
        case tv: TypeVariable =>
          val nv = tv.asTypeVar
          if (!seenVars(tv)) {
            seenVars += tv
            val mapping = tv.uniConcreteTypes.iterator.map(go).toList
            if (mapping.nonEmpty) unified ::= (nv, mapping)
          }
          nv
        case FunctionType(l, r) => Function(go(l), go(r))
        case ComposedType(true, l, r) => Union(go(l), go(r))
        case ComposedType(false, l, r) => Inter(go(l), go(r))
        case TupleType(fs) => Tuple(fs.map{ case (_, fld) => go(fld)})
        case ExtrType(true) => Bot
        case ExtrType(false) => Top
        case ProvType(und) => go(und)
        case tag: RigidTypeVariable => tag.id match {
          case Var(n) =>
            if (primitiveTypes.contains(n) // primitives like `int` are internally maintained as class tags
              || n.isCapitalized // rigid type params like A in class Foo[A]
              || n.startsWith("'") // rigid type varibales
              || n === "this" // `this` type
            ) TypeName(n)
            else TypeTag(n.capitalize)
        }
        case TypeRef(td, Nil) => td
        case tr @ TypeRef(td, targs) => AppliedType(td, tr.mapTargs(S(true)) {
          case (_, ty) => go(ty)
        })
        case _ => ???
      }
    // }(r => s"~> $r")

    val res = go(st)(Set.empty)
    if (unified.isEmpty) res
    else Unified(res, unified)
  }

  private var curUid: Int = 0
  def nextUid: Int = {
    val res = curUid
    curUid += 1
    res
  }
}
