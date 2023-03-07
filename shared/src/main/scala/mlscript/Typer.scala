package mlscript

import scala.collection.mutable
import scala.collection.mutable.{Map => MutMap, Set => MutSet}
import scala.collection.immutable.{SortedMap, SortedSet}
import scala.util.chaining._
import scala.annotation.tailrec
import mlscript.utils._
import shorthands._
import mlscript.Message._

import scala.collection.immutable
import mlscript.Diagnostic._

/** A class encapsulating type inference state.
 *  It uses its own internal representation of types and type variables, using mutable data structures.
 *  Inferred SimpleType values are then turned into CompactType values for simplification.
 *  In order to turn the resulting CompactType into a mlscript.Type, we use `expandCompactType`.
 */
class Typer(var dbg: Boolean, var verbose: Bool, var explainErrors: Bool)
    extends TypeDefs with TypeSimplifier {
  
  def funkyTuples: Bool = false
  def doFactorize: Bool = false
  def setErrorSimplification(simplifyError: Bool): Unit =
    errorSimplifer.simplifyError = simplifyError
    
  def reporCollisionErrors: Bool = true
  
  def showSuspiciousLocations()(implicit raise: Raise): Unit =
    errorSimplifer.reportInfo(N, 8)
  
  /** If flag is set proxy types are created during constraint resolution. This
    * is needed for debugging, showing verbose error messages traces and
    * creating simplified error messages.
    * 
    * It should be set wherever such features are needed
    */
  var recordProvenances: Boolean = true
  
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
    *   (See the case for `Sel` in `typeTerm` for documentation on explicit vs. implicit calls.)
    * The public helper functions should be preferred for manipulating `mthEnv`
   */
  case class Ctx(
      parent: Opt[Ctx],
      env: MutMap[Str, TypeInfo],
      lvl: Int,
      inPattern: Bool,
      tyDefs: Map[Str, TypeDef],
  ) {
    def +=(b: Str -> TypeInfo): Unit = env += b
    def ++=(bs: IterableOnce[Str -> TypeInfo]): Unit = bs.iterator.foreach(+=)
    def get(name: Str): Opt[TypeInfo] = env.get(name) orElse parent.dlof(_.get(name))(N)
    def contains(name: Str): Bool = env.contains(name) || parent.exists(_.contains(name))
    def nest: Ctx = copy(Some(this), MutMap.empty)
    def nextLevel: Ctx = copy(lvl = lvl + 1)
    private val abcCache: MutMap[Str, Set[TypeName]] = MutMap.empty
    def allBaseClassesOf(name: Str): Set[TypeName] = abcCache.getOrElseUpdate(name,
      tyDefs.get(name).fold(Set.empty[TypeName])(_.allBaseClasses(this)(Set.empty)))
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
  val ErrTypeId: SimpleTerm = Var("error")

  val builtinTypes: Ls[TypeDef] =
    TypeDef(Cls, TypeName("int"), Nil, Nil, TopType, Set.empty, N, Nil, S(TypeName("int")->Nil)) ::
    TypeDef(Cls, TypeName("number"), Nil, Nil, TopType, Set.empty, N, Nil, S(TypeName("number")->Nil)) ::
    TypeDef(Cls, TypeName("bool"), Nil, Nil, TopType, Set.empty, N, Nil, S(TypeName("bool")->Nil)) ::
    TypeDef(Cls, TypeName("true"), Nil, Nil, TopType, Set.single(TypeName("bool")), N, Nil, S(TypeName("bool")->Nil)) ::
    TypeDef(Cls, TypeName("false"), Nil, Nil, TopType, Set.single(TypeName("bool")), N, Nil, S(TypeName("bool")->Nil)) ::
    TypeDef(Cls, TypeName("string"), Nil, Nil, TopType, Set.empty, N, Nil, S(TypeName("string")->Nil)) ::
    TypeDef(Als, TypeName("undefined"), Nil, Nil, ClassTag(UnitLit(true), Set.empty)(noProv), Set.empty, N, Nil) ::
    TypeDef(Als, TypeName("null"), Nil, Nil, ClassTag(UnitLit(false), Set.empty)(noProv), Set.empty, N, Nil) ::
    TypeDef(Als, TypeName("anything"), Nil, Nil, TopType, Set.empty, N, Nil) ::
    TypeDef(Als, TypeName("nothing"), Nil, Nil, BotType, Set.empty, N, Nil) ::
    TypeDef(Cls, TypeName("error"), Nil, Nil, TopType, Set.empty, N, Nil) ::
    TypeDef(Cls, TypeName("unit"), Nil, Nil, TopType, Set.empty, N, Nil) ::
    TypeDef(Cls, TypeName("float"), Nil, Nil, TopType, Set.empty, N, Nil) :: {
      val listTyVar: TypeVariable = freshVar(noProv, N)(1)
      val td = TypeDef(Cls, TypeName("list"), Ls((TypeName("A"), listTyVar)), Ls(listTyVar), TopType, Set.empty, N, Nil, S(TypeName("list"), Nil))
      td.tvarVariances = S(MutMap(listTyVar -> VarianceInfo.co))
      td
    } ::
    Nil
  val primitiveTypes: Set[Str] =
    builtinTypes.iterator.map(_.nme.name).toSet

  // built in operators bound to their type schemes
  val builtinBindings: Bindings = {
    import FunctionType.{ apply => fun }
    Map(
      "true" -> BoolType,
      "false" -> BoolType,
      "eq" -> {
        val v = freshVar(noProv)(1)
        PolymorphicType(0, fun(v, fun(v, BoolType)(noProv))(noProv))
      },
      "error" -> BotType,
      "Cons" -> {
        val listTyVar: TypeVariable = freshVar(noProv, S("'0"))(1)
        val ListType: TypeRef = TypeRef(TypeName("list"), Ls(listTyVar))(noTyProv)
        val args = TupleType(Ls(N -> FieldType(N, listTyVar)(noTyProv), N -> FieldType(N, ListType)(noTyProv)))(noTyProv)
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
    val typeType2 = ()
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
      case Bounds(Bot, Top) =>
        val p = tyTp(ty.toLoc, "type wildcard")
        TypeBounds(ExtrType(true)(p), ExtrType(false)(p))(p)
      case Bounds(lb, ub) => TypeBounds(rec(lb), rec(ub))(tyTp(ty.toLoc, "type bounds"))
      case Tuple(fields) =>
        TupleType(fields.map(fld =>
            N -> FieldType(N, rec(fld))(tp(fld.toLoc, "tuple field"))
          ))(tyTp(ty.toLoc, "tuple type"))
      case Inter(lhs, rhs) => (if (simplify) rec(lhs) & (rec(rhs), _: TypeProvenance)
          else ComposedType(false, rec(lhs), rec(rhs)) _
        )(tyTp(ty.toLoc, "intersection type"))
      case Union(lhs, rhs) => (if (simplify) rec(lhs) | (rec(rhs), _: TypeProvenance)
          else ComposedType(true, rec(lhs), rec(rhs)) _
        )(tyTp(ty.toLoc, "union type"))
      case Neg(t) => NegType(rec(t))(tyTp(ty.toLoc, "type negation"))
      case Record(fs) => 
        val prov = tyTp(ty.toLoc, "record type")
        fs.groupMap(_._1.name)(_._1).foreach { case s -> fieldNames if fieldNames.sizeIs > 1 => err(
            msg"Multiple declarations of field name ${s} in ${prov.desc}" -> ty.toLoc
              :: fieldNames.map(tp => msg"Declared at" -> tp.toLoc))(raise)
          case _ =>
        }
        RecordType.mk(fs.map { nt =>
          if (nt._1.name.isCapitalized)
            err(msg"Field identifiers must start with a small letter", nt._1.toLoc)(raise)
          nt._1 -> FieldType(N, rec(nt._2))(
            tp(App(nt._1, Var("").withLocOf(nt._2)).toCoveringLoc, "record field"))
        })(prov)
      case Function(lhs, rhs) => FunctionType(rec(lhs), rec(rhs))(tyTp(ty.toLoc, "function type"))
      case WithExtension(b, r) => WithType(rec(b),
        RecordType(
            r.fields.map { case (n, f) => n -> FieldType(N, rec(f))(
              tyTp(App(n, Var("").withLocOf(f)).toCoveringLoc, "extension field")) }
          )(tyTp(r.toLoc, "extension record")))(tyTp(ty.toLoc, "extension type"))
      case Literal(lit) => ClassTag(lit, lit.baseClasses)(tyTp(ty.toLoc, "literal type"))
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
            case L(e) =>
              if (name.isEmpty || !name.head.isLower) e()
              else (typeNamed(tyLoc, name), ctx.tyDefs.get(name)) match {
                case (R((kind, _)), S(td)) => kind match {
                  case Cls => clsNameToNomTag(td)(tyTp(tyLoc, "class tag"), ctx)
                  case Trt => trtNameToNomTag(td)(tyTp(tyLoc, "trait tag"), ctx)
                  case Als => err(
                    msg"Type alias ${name} cannot be used as a type tag", tyLoc)(raise)
                  case Nms => err(
                    msg"Namespaces ${name} cannot be used as a type tag", tyLoc)(raise)
                }
                case _ => e()
              }
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
      case Recursive(uv, body) =>
        val tv = freshVar(tyTp(ty.toLoc, "local type binding"), uv.identifier.toOption)
        val bod = rec(body)(ctx, recVars + (uv -> tv))
        tv.upperBounds ::= bod
        tv.lowerBounds ::= bod
        tv
      case Rem(base, fs) => Without(rec(base), fs.toSortedSet)(tyTp(ty.toLoc, "field removal type"))
      case Constrained(base, where) =>
        val res = rec(base)
        where.foreach { case (tv, Bounds(lb, ub)) =>
          constrain(rec(lb), tv)(raise, tp(lb.toLoc, "lower bound specifiation"), ctx)
          constrain(tv, rec(ub))(raise, tp(ub.toLoc, "upper bound specifiation"), ctx)
        }
        res
    }
    (rec(ty)(ctx, Map.empty), localVars.values)
  }(r => s"=> ${r._1} | ${r._2.mkString(", ")}")
  
  def typePattern(pat: Term)(implicit ctx: Ctx, raise: Raise, vars: Map[Str, SimpleType] = Map.empty): SimpleType =
    typeTerm(pat)(ctx.copy(inPattern = true), raise, vars)
  
  
  def typeStatement(s: DesugaredStatement, allowPure: Bool)
        (implicit ctx: Ctx, raise: Raise): PolymorphicType \/ Ls[Binding] = s match {
    case Def(false, Var("_"), L(rhs), isByname) => typeStatement(rhs, allowPure)
    case Def(isrec, nme, L(rhs), isByname) => // TODO reject R(..)
      if (nme.name === "_")
        err(msg"Illegal definition name: ${nme.name}", nme.toLoc)(raise)
      val ty_sch = typeLetRhs(isrec, nme.name, rhs)
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
          constrain(mkProxy(ty, TypeProvenance(t.toCoveringLoc, "expression in statement position")), UnitType)(
            raise = err => raise(WarningReport( // Demote constraint errors from this to warnings
              msg"Expression in statement position should have type `unit`." -> N ::
              msg"Use the `discard` function to discard non-unit values, making the intent clearer." -> N ::
              err.allMsgs)),
            prov = TypeProvenance(t.toLoc, t.describe), ctx)
      }
      L(PolymorphicType(0, ty))
    case _ =>
      err(msg"Illegal position for this ${s.describe} statement.", s.toLoc)(raise)
      R(Nil)
  }
  
  /** Infer the type of a let binding right-hand side. */
  def typeLetRhs(isrec: Boolean, nme: Str, rhs: Term)(implicit ctx: Ctx, raise: Raise,
      vars: Map[Str, SimpleType] = Map.empty): PolymorphicType = {
    val res = if (isrec) {
      val e_ty = freshVar(
        // It turns out it is better to NOT store a provenance here,
        //    or it will obscure the true provenance of constraints causing errors
        //    across recursive references.
        noProv,
        // TypeProvenance(rhs.toLoc, "let-bound value"),
        S(nme)
      )(lvl + 1)
      ctx += nme -> VarSymbol(e_ty, Var(nme))
      val ty = typeTerm(rhs)(ctx.nextLevel, raise, vars)
      constrain(ty, e_ty)(raise, TypeProvenance(rhs.toLoc, "binding of " + rhs.describe), ctx)
      e_ty
    } else typeTerm(rhs)(ctx.nextLevel, raise, vars)
    PolymorphicType(lvl, res)
  }
  def typeLetRhsMono(isrec: Boolean, nme: Str, rhs: Term)(implicit ctx: Ctx, raise: Raise,
      vars: Map[Str, SimpleType] = Map.empty): ST = {
    val res = if (isrec) {
      val e_ty = freshVar(
        // It turns out it is better to NOT store a provenance here,
        //    or it will obscure the true provenance of constraints causing errors
        //    across recursive references.
        noProv,
        // TypeProvenance(rhs.toLoc, "let-bound value"),
        S(nme)
      )(lvl)
      ctx += nme -> VarSymbol(e_ty, Var(nme))
      val ty = typeTerm(rhs)(ctx, raise, vars)
      constrain(ty, e_ty)(raise, TypeProvenance(rhs.toLoc, "binding of " + rhs.describe), ctx)
      e_ty
    } else typeTerm(rhs)(ctx, raise, vars)
    res
  }
  
  def mkProxy(ty: SimpleType, prov: TypeProvenance): SimpleType = {
    if (recordProvenances) ProvType(ty)(prov)
    else ty // TODO don't do this when debugging errors
    // TODO switch to return this in perf mode:
    // ty
  }
  
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
        ctx.parent.dlof(_.get(v.name))(N) |>? { case S(VarSymbol(ts: TypeScheme, _)) =>
          ts.instantiate(0).unwrapProxies } |>? {
            case S(ClassTag(Var(v.name), _)) =>
              warn(msg"Variable name '${v.name}' already names a symbol in scope. " +
                s"If you want to refer to that symbol, you can use `scope.${v.name}`; " +
                s"if not, give your future readers a break and use another name :^)", v.toLoc)
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
        = trace(s"$lvl. Typing ${if (ctx.inPattern) "pattern" else "term"} $term") {
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
      var errorsCount = 0
      constrain(lhs, rhs)({
        case err: ErrorReport =>
          // Note that we do not immediately abort constraining because we still
          //  care about getting the non-erroneous parts of the code return meaningful types.
          // In other words, this is so that errors do not interfere too much
          //  with the rest of the (hopefully good) code.
          if (errorsCount === 0) {
            // constrain(errType, res)(_ => (), noProv, ctx)
            // ^ This is just to get error types leak into the result
            raise(err)
          } else if (errorsCount < 3) {
            // Silence further errors from this location.
          } else {
            return res
            // ^ Stop constraining, at this point.
            //    This is to avoid rogue (explosive) constraint solving from badly-behaved error cases.
            //    For instance see the StressTraits.mls test.
          }
          errorsCount += 1
        case diag => raise(diag)
      }, prov, ctx)

      // also unify types
//      unifyTypes(lhs, rhs)(MutSet(), ctx, raise)
      res
    }
    term match {
      case v @ Var("_") =>
        if (ctx.inPattern || funkyTuples) freshVar(tp(v.toLoc, "wildcard"))
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
        mkProxy(ty, prov)
        // ^ TODO maybe use a description passed in param?
        // currently we get things like "flows into variable reference"
        // but we used to get the better "flows into object receiver" or "flows into applied expression"...
      case _: IntLit => TypeRef(TypeName("int"), Nil)(prov)
      case _: StrLit => TypeRef(TypeName("string"), Nil)(prov)
      case _: DecLit => TypeRef(TypeName("float"), Nil)(prov)
      case lit: Lit => ClassTag(lit, lit.baseClasses)(prov)
      case Rcd(fs) =>
        val prov = tp(term.toLoc, "record literal")
        fs.groupMap(_._1.name)(_._1).foreach { case s -> fieldNames if fieldNames.sizeIs > 1 => err(
            msg"Multiple declarations of field name ${s} in ${prov.desc}" -> term.toLoc
              :: fieldNames.map(tp => msg"Declared at" -> tp.toLoc))(raise)
          case _ =>
        }
        RecordType.mk(fs.map { case (n, t) =>
          if (n.name.isCapitalized)
            err(msg"Field identifiers must start with a small letter", term.toLoc)(raise)
          val tym = typeTerm(t)
          val fprov = tp(App(n, t).toLoc, "record field")
          (n, tym.toUpper(fprov))
        })(prov)
      case Tup(fs) =>
        TupleType(fs.map { t =>
          val tym = typeTerm(t)
          val fprov = tp(t.toLoc, "tuple field")
          N -> FieldType(N, tym)(fprov)
        })(tp(term.toLoc, "tuple literal"))
        // TODO is this supported in ocaml
      case Subs(a, i) =>
        val t_a = typeTerm(a)
        val t_i = typeTerm(i)
        con(t_i, IntType, TopType)
        val elemType = freshVar(prov)
        elemType.upperBounds ::=
          // * We forbid using [⋅] indexing to access elements that possibly have `undefined` value,
          // *  which could result in surprising behavior and bugs in the presence of parametricity!
          // * Note that in modern JS, `undefined` is arguably not a value you're supposed to use explicitly;
          // *  `null` should be used instead for those willing to indulge in the Billion Dollar Mistake.
          TypeRef(TypeName("undefined"), Nil)(noProv).neg(
            prov.copy(desc = "prohibited undefined element")) // TODO better reporting for this; the prov isn't actually used
        con(t_a, ArrayType(elemType.toUpper(tp(i.toLoc, "array element")))(prov), elemType) |
          TypeRef(TypeName("undefined"), Nil)(prov.copy(desc = "possibly-undefined array access"))
      case Assign(s @ Sel(r, f), rhs) =>
        val o_ty = typeTerm(r)
        val sprov = tp(s.toLoc, "assigned selection")
        val fieldType = freshVar(sprov, Opt.when(!f.name.startsWith("_"))(f.name))
        val obj_ty =
          // Note: this proxy does not seem to make any difference:
          mkProxy(o_ty, tp(r.toCoveringLoc, "receiver"))
        con(obj_ty, RecordType.mk((f, FieldType(Some(fieldType), TopType)(
          tp(f.toLoc, "assigned field")
        )) :: Nil)(sprov), fieldType)
        val vl = typeTerm(rhs)
        con(vl, fieldType, UnitType.withProv(prov))
      case Assign(s @ Subs(a, i), rhs) => 
        val a_ty = typeTerm(a)
        val sprov = tp(s.toLoc, "assigned array element")
        val elemType = freshVar(sprov)
        val arr_ty =
            // Note: this proxy does not seem to make any difference:
            mkProxy(a_ty, tp(a.toCoveringLoc, "receiver"))
        con(arr_ty, ArrayType(FieldType(Some(elemType), elemType)(sprov))(prov), TopType)
        val i_ty = typeTerm(i)
        con(i_ty, IntType, TopType)
        val vl = typeTerm(rhs)
        con(vl, elemType, UnitType.withProv(prov))
      case Assign(lhs, rhs) =>
        err(msg"Illegal assignment" -> prov.loco
          :: msg"cannot assign to ${lhs.describe}" -> lhs.toLoc :: Nil)
      case pat if ctx.inPattern =>
        err(msg"Unsupported pattern shape${
          if (dbg) " ("+pat.getClass.toString+")" else ""}:", pat.toLoc)(raise)
      case Lam(pat, body) =>
        val newCtx = ctx.nest
        val param_ty = typePattern(pat)(newCtx, raise, vars)
        val body_ty = typeTerm(body)(newCtx, raise, vars)
        FunctionType(param_ty, body_ty)(tp(term.toLoc, "function"))
      case App(f, a) =>
        val fun_ty = typeTerm(f)
        val arg_ty = typeTerm(a)
        val res = freshVar(prov)
//        val resTy = con(fun_ty, FunctionType(arg_ty, res)(hintProv(prov)), res)
        def go(f_ty: ST): ST = f_ty.unwrapProxies match {
//          case FunctionType(l, r) =>
//            con(arg_ty, l, r.withProv(prov))
          case _ =>
            val res = freshVar(prov, N)
            val resTy = con(fun_ty, FunctionType(arg_ty, res)(
              prov
//               funProv // TODO: better?
            ), res)
            resTy
        }
        go(fun_ty)
      case Sel(obj, fieldName) =>
        def rcdSel(obj: Term, fieldName: Var) = {
          val o_ty = typeTerm(obj)
          val res = freshVar(prov, Opt.when(!fieldName.name.startsWith("_"))(fieldName.name))
          val obj_ty = mkProxy(o_ty, tp(obj.toCoveringLoc, "receiver"))
          val rcd_ty = RecordType.mk(
            fieldName -> res.toUpper(tp(fieldName.toLoc, "field selector")) :: Nil)(hintProv(prov))
          con(obj_ty, rcd_ty, res)
        }

        // methods have been removed only field selection works
        rcdSel(obj, fieldName)
      case Let(isrec, nme, rhs, bod) =>
        val n_ty = typeLetRhsMono(isrec, nme.name, rhs)
        val newCtx = ctx.nest
        newCtx += nme.name -> VarSymbol(n_ty, nme)
        typeTerm(bod)(newCtx, raise)
      case Blk(stmts) => typeTerms(stmts, false, Nil)(ctx.nest, raise, prov)
      case Bind(l, r) =>
        val l_ty = typeTerm(l)
        val newCtx = ctx.nest // so the pattern's context don't merge with the outer context!
        val r_ty = typePattern(r)(newCtx, raise)
        ctx ++= newCtx.env
        con(l_ty, r_ty, r_ty)
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
            // find constructor for each arm
            // handle tuple arms differently
            val adtData = body.flatMap(_.topLevelCtors).map {
              case v@Var("Tup1") => TypeName("Tup") -> Ls(0) -> v
              case v@Var("Tup2") => TypeName("Tup") -> Ls(0, 1) -> v
              case v@Var("Tup3") => TypeName("Tup") -> Ls(0, 1, 2) -> v
              case v@Var(name) => ctx.tyDefs.getOrElse(name, lastWords(s"could not find type definition ${name}"))
                .adtData.getOrElse(lastWords(s"could not find adt data for type definition ${name}")) -> v
            }
            // find and retrieve the common adt for all the constructors
            val ((adtName, argsPos), caseAdt) = adtData match {
              case Nil => lastWords("match case without any arms")
              case adt :: Nil => adt
              case head :: tail =>
                val adtName = head._1._1
                tail.foreach(v => if (v._1._1 =/= adtName) lastWords(s"incompatible adt data for ${head} and ${v}"))
                head
            }
            println(s"ADT name: $adtName")

            val (adt_ty, newTargs) = adtName match {
              case TypeName("Tup") =>
                val elem_ty = argsPos.map(pos => {
                  val typ = TypeProvenance(caseAdt.toLoc, s"${pos} element of this tuple")
                  freshVar(typ)
                })
                val fld_ty = elem_ty.map(elem => {
                  N -> FieldType(N, elem)(elem.prov)
                })
                val caseAdtTyp = TypeProvenance(caseAdt.toLoc, "pattern")
                val adt_ty = TupleType(fld_ty)(caseAdtTyp).withProv(TypeProvenance(cond.toLoc, "`match` condition"))
                (adt_ty, elem_ty)
              case _ =>
                val adtDef = ctx.tyDefs.getOrElse(adtName.name, lastWords(s"Could not find ${adtName} in context"))
                val newTargs = adtDef.targs.map(tv => freshVar(tv.prov, tv.nameHint))
                // provenance for the first case expression from where we find the adt
                val caseAdtTyp = TypeProvenance(caseAdt.toLoc, "pattern")
                // TODO weird duplication in OcamlPresentation errors
                val adt_ty = TypeRef(adtName, newTargs)(caseAdtTyp).withProv(TypeProvenance(cond.toLoc, "`match` condition"))
                println(s"ADT type: $adt_ty")
                (adt_ty, newTargs)
            }

            println(s"typed condition term ${cond}")
            val cond_ty = typeTerm(cond)
            val ret_ty = if (body.length === 1) {
              freshVar(prov.copy(desc = "let expression"))
            } else {
              freshVar(prov.copy(desc = "match expression"))
            }
            con(cond_ty, adt_ty, adt_ty)

            // the assumed shape of an IfBody is a List[IfThen, IfThen, IfElse] with an optional IfElse at the end
            body.zipWithIndex.foreach {
              case (IfThen(Tup(fs), rhs), _) =>
                assert(fs.length === newTargs.length)
                println(s"fields $fs")
                val nestCtx = ctx.nest
                // add any vars to nested context pattern
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
                // constraint match arm type with the return type of match expression
                nestCtx |> { implicit ctx =>
                  con(typeTerm(rhs), ret_ty, ret_ty)
                }
              // case x -> expr catch all with a new variable in the context
              case (IfThen(v@Var(name), rhs), _) =>
                // update context with variables
                val newCtx = ctx.nest
                newCtx += name -> VarSymbol(cond_ty, v)
                con(typeTerm(rhs), ret_ty, ret_ty)
              // case Left x -> expr or Left 2 -> expr the type variable for the adt is constrained
              // with the argument to the constructor Left in this case
              // case (IfThen(App(Var(ctorNme), Tup(fields)), rhs), index) =>
              case (IfThen(App(Var(ctorNme), fs), rhs), index) =>
                val fields = fs match {
                  case t: Tup => t.fields
                  case Bra(false, t: Tup) => t.fields
                  case t => t :: Nil
                }
                println(s"fields $fs ~> $fields")
                println(s"Typing case $index ($ctorNme)")
                val ctorType = ctx.get(ctorNme) match {
                  case S(VarSymbol(PolymorphicType(_, res), _)) => res
                  case _ => die
                }
                println(s"ctor type: ${ctorType}")
                val (originalFieldTypes, ctorTargs) = ctorType.unwrapProxies match {
                  case FunctionType(fieldTy, TypeRef(defn, targs)) =>
                    assert(defn === adtName)
                    (fieldTy match {
                      case TupleType(fs) => fs.map(_._2.ub)
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
                val adtArgIndex = adtData(index)._2
                val nestCtx = ctx.nest

                // type each match arm field and constraint with adt type variable
                // add any vars to nested context pattern
                fields.zipWithIndex.foreach {
                  // in case of Left x also add x to nested scope
                  // case ((argTerm: Var, adtArgIndex), fieldIdx) =>
                  case (argTerm: Var, fieldIdx) =>
                    println(s"Typing field $argTerm ($adtArgIndex)")
                    val fieldType = fieldTypes(fieldIdx)
                    println(s"Field $argTerm : $fieldType")
                    nestCtx += argTerm.name -> VarSymbol(fieldType, argTerm)
                  // in case of 0 :: tl the type of list must be constrained with 0
                  case (argTerm, fieldIdx) =>
                    val fieldType = fieldTypes(fieldIdx)
                    val argTy = typeTerm(argTerm)
                    // TODO is the direction of constraint correct
                    con(fieldType, argTy, fieldType)
                }
                // constraint match arm type with the return type of match expression
                nestCtx |> { implicit ctx =>
                  con(typeTerm(rhs), ret_ty, ret_ty)
                }
              case (IfElse(expr), _) => con(typeTerm(expr), ret_ty, ret_ty)
              case ifbody => lastWords(s"Cannot handle pattern ${ifbody}")
            }
            ret_ty
        }
      case _ => lastWords(s"Cannot type term: ${PrettyPrintHelper.inspect(term)}")
    }
  }(r => s"$lvl. : ${r}")
  
  def typeArms(scrutVar: Opt[Var], arms: CaseBranches)
      (implicit ctx: Ctx, raise: Raise, lvl: Int)
      : Ls[SimpleType -> SimpleType] -> Ls[SimpleType] = arms match {
    case NoCases => Nil -> Nil
    case Wildcard(b) =>
      val fv = freshVar(tp(arms.toLoc, "wildcard pattern"))
      val newCtx = ctx.nest
      scrutVar match {
        case Some(v) =>
          newCtx += v.name -> VarSymbol(fv, v)
          val b_ty = typeTerm(b)(newCtx, raise)
          (fv -> TopType :: Nil) -> (b_ty :: Nil)
        case _ =>
          (fv -> TopType :: Nil) -> (typeTerm(b) :: Nil)
      }
    case Case(pat, bod, rest) =>
      val patTy = pat match {
        case lit: Lit =>
          ClassTag(lit, lit.baseClasses)(tp(pat.toLoc, "literal pattern"))
        case Var(nme) =>
          val tpr = tp(pat.toLoc, "type pattern")
          ctx.tyDefs.get(nme) match {
            case None =>
              err("type identifier not found: " + nme, pat.toLoc)(raise)
              val e = ClassTag(ErrTypeId, Set.empty)(tpr)
              return ((e -> e) :: Nil) -> (e :: Nil)
            case Some(td) =>
              td.kind match {
                case Als => err(msg"can only match on classes and traits", pat.toLoc)(raise)
                case Nms => err(msg"can only match on classes and traits", pat.toLoc)(raise)
                case Cls => clsNameToNomTag(td)(tp(pat.toLoc, "class pattern"), ctx)
                case Trt => trtNameToNomTag(td)(tp(pat.toLoc, "trait pattern"), ctx)
              }
          }
      }
      val newCtx = ctx.nest
      val (req_ty, bod_ty, (tys, rest_ty)) = scrutVar match {
        case S(v) =>
          val tv = freshVar(tp(v.toLoc, "refined scrutinee"),
            // S(v.name), // this one seems a bit excessive
          )
          newCtx += v.name -> VarSymbol(tv, v)
          val bod_ty = typeTerm(bod)(newCtx, raise)
          (patTy -> tv, bod_ty, typeArms(scrutVar, rest))
        case N =>
          val bod_ty = typeTerm(bod)(newCtx, raise)
          (patTy -> TopType, bod_ty, typeArms(scrutVar, rest))
      }
      (req_ty :: tys) -> (bod_ty :: rest_ty)
  }
  
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
    // case (trm: Term) :: Nil =>
    //   assert(!rcd)
    //   val ty = typeTerm(trm)
    //   typeBra(Nil, rcd, (N, ty) :: fields)
    case s :: sts =>
      val (diags, desug) = s.desugared
      diags.foreach(raise)
      val newBindings = desug.flatMap(typeStatement(_, allowPure = false).toOption)
      ctx ++= newBindings.iterator.flatten.map(nt => nt._1 -> VarSymbol(nt._2, Var(nt._1)))
      typeTerms(sts, rcd, fields)
    case Nil =>
      if (rcd) {
        val fs = fields.reverseIterator.zipWithIndex.map {
          case ((S(n), t), i) =>
            n -> t.toUpper(noProv)
          case ((N, t), i) =>
            // err("Missing name for record field", t.prov.loco)
            warn("Missing name for record field", t.prov.loco)
            (Var("_" + (i + 1)), t.toUpper(noProv))
        }.toList
        RecordType.mk(fs)(prov)
      } else TupleType(fields.reverseIterator.mapValues(_.toUpper(noProv)))(prov)
  }

  /** Convert an inferred SimpleType into the immutable Type representation. */
  def expandType(st: SimpleType, stopAtTyVars: Bool = false)(implicit ctx: Ctx): Type = {
    val expandType = ()
    
    import Set.{empty => semp}
    
    var bounds: Ls[TypeVar -> Bounds] = Nil
    
    val seenVars = mutable.Set.empty[TV]
    
    def field(ft: FieldType): Type = go(ft.ub)

    def go(st: SimpleType): Type =
            // trace(s"expand $st") {
          st.unwrapProvs match {
        case tv: TypeVariable if stopAtTyVars => tv.asTypeVar
        case tv: TypeVariable =>
          val nv = tv.asTypeVar
          if (!seenVars(tv)) {
            seenVars += tv
            val l = go(tv.lowerBounds.foldLeft(BotType: ST)(_ | _))
            val u = go(tv.upperBounds.foldLeft(TopType: ST)(_ & _))
            if (l =/= Bot || u =/= Top)
              bounds ::= nv -> Bounds(l, u)
          }
          nv
        case FunctionType(l, r) => Function(go(l), go(r))
        case ComposedType(true, l, r) => Union(go(l), go(r))
        case ComposedType(false, l, r) => Inter(go(l), go(r))
        case RecordType(fs) => Record(fs.mapValues(field))
        case TupleType(fs) => Tuple(fs.map{ case (_, fld) => field(fld)})
        case ArrayType(FieldType(None, ub)) => AppliedType(TypeName("Array"), go(ub) :: Nil)
        case NegType(t) => Neg(go(t))
        case ExtrType(true) => Bot
        case ExtrType(false) => Top
        case WithType(base, rcd) =>
          WithExtension(go(base), Record(rcd.fields.mapValues(field)))
        case ProxyType(und) => go(und)
        case tag: ObjectTag => tag.id match {
          case Var(n) =>
            if (primitiveTypes.contains(n) // primitives like `int` are internally maintained as class tags
              || n.isCapitalized // rigid type params like A in class Foo[A]
              || n.startsWith("'") // rigid type varibales
              || n === "this" // `this` type
            ) TypeName(n)
            else TypeTag(n.capitalize)
          case lit: Lit => Literal(lit)
        }
        case TypeRef(td, Nil) => td
        case tr @ TypeRef(td, targs) => AppliedType(td, tr.mapTargs(S(true)) {
          case ta @ ((S(true), TopType) | (S(false), BotType)) => Bounds(Bot, Top)
          case (_, ty) => go(ty)
        })
        case TypeBounds(lb, ub) => Bounds(go(lb), go(ub))
        case Without(base, names) => Rem(go(base), names.toList)
        case _ => ???
    }
    // }(r => s"~> $r")
    
    val res = go(st)
    if (bounds.isEmpty) res
    else Constrained(res, bounds)
  }
  
  
  private var curUid: Int = 0
  def nextUid: Int = {
    val res = curUid
    curUid += 1
    res
  }
}
