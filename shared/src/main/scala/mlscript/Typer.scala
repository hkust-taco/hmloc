package mlscript

import scala.collection.mutable
import scala.collection.mutable.{Map => MutMap, Set => MutSet}
import scala.collection.immutable.{SortedSet, SortedMap}
import scala.util.chaining._
import scala.annotation.tailrec
import mlscript.utils._, shorthands._
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
  val UnitType: ClassTag = ClassTag(Var("unit"), Set.empty)(noTyProv)
  val BoolType: ClassTag = ClassTag(Var("bool"), Set.empty)(noTyProv)
  val TrueType: ClassTag = ClassTag(Var("true"), Set.single(TypeName("bool")))(noTyProv)
  val FalseType: ClassTag = ClassTag(Var("false"), Set.single(TypeName("bool")))(noTyProv)
  val IntType: ClassTag = ClassTag(Var("int"), Set.empty)(noTyProv)
  val DecType: ClassTag = ClassTag(Var("number"), Set.empty)(noTyProv)
  val StrType: ClassTag = ClassTag(Var("string"), Set.empty)(noTyProv)
  // Add class tags for list constructors
  val FloatType: ClassTag = ClassTag(Var("float"), Set.empty)(noTyProv)
  
  val ErrTypeId: SimpleTerm = Var("error")
  
  // TODO rm this obsolete definition (was there for the old frontend)
  private val primTypes =
    List("unit" -> UnitType, "bool" -> BoolType, "int" -> IntType, "number" -> DecType, "string" -> StrType,
      "anything" -> TopType, "nothing" -> BotType, "float" -> FloatType)
  
  val builtinTypes: Ls[TypeDef] =
    TypeDef(Cls, TypeName("int"), Nil, Nil, TopType, Set.empty, N, Nil) ::
    TypeDef(Cls, TypeName("number"), Nil, Nil, TopType, Set.empty, N, Nil) ::
    TypeDef(Cls, TypeName("bool"), Nil, Nil, TopType, Set.empty, N, Nil) ::
    TypeDef(Cls, TypeName("true"), Nil, Nil, TopType, Set.single(TypeName("bool")), N, Nil) ::
    TypeDef(Cls, TypeName("false"), Nil, Nil, TopType, Set.single(TypeName("bool")), N, Nil) ::
    TypeDef(Cls, TypeName("string"), Nil, Nil, TopType, Set.empty, N, Nil) ::
    TypeDef(Als, TypeName("undefined"), Nil, Nil, ClassTag(UnitLit(true), Set.empty)(noProv), Set.empty, N, Nil) ::
    TypeDef(Als, TypeName("null"), Nil, Nil, ClassTag(UnitLit(false), Set.empty)(noProv), Set.empty, N, Nil) ::
    TypeDef(Als, TypeName("anything"), Nil, Nil, TopType, Set.empty, N, Nil) ::
    TypeDef(Als, TypeName("nothing"), Nil, Nil, BotType, Set.empty, N, Nil) ::
    TypeDef(Cls, TypeName("error"), Nil, Nil, TopType, Set.empty, N, Nil) ::
    TypeDef(Cls, TypeName("unit"), Nil, Nil, TopType, Set.empty, N, Nil) ::
    {
      val tv = freshVar(noTyProv)(1)
      val tyDef = TypeDef(Als, TypeName("Array"), List(TypeName("A") -> tv), Nil,
        ArrayType(FieldType(None, tv)(noTyProv))(noTyProv), Set.empty, N, Nil)
        // * ^ Note that the `noTyProv` here is kind of a problem
        // *    since we currently expand primitive types eagerly in DNFs.
        // *  For instance, see `inn2 v1` in test `Yicong.mls`.
        // *  We could instead treat these primitives like any other TypeRef,
        // *    but that currently requires more simplifier work
        // *    to get rid of things like `1 & int` and `T | nothing`.
      tyDef.tvarVariances = S(MutMap(tv -> VarianceInfo.co))
      tyDef
    } ::
    {
      val tv = freshVar(noTyProv)(1)
      val tyDef = TypeDef(Als, TypeName("MutArray"), List(TypeName("A") -> tv), Nil,
        ArrayType(FieldType(Some(tv), tv)(noTyProv))(noTyProv), Set.empty, N, Nil)
      tyDef.tvarVariances = S(MutMap(tv -> VarianceInfo.in))
      tyDef
    } ::
    TypeDef(Cls, TypeName("float"), Nil, Nil, TopType, Set.empty, N, Nil) ::
    Nil
  val primitiveTypes: Set[Str] =
    builtinTypes.iterator.map(_.nme.name).toSet
  def singleTup(ty: ST): ST =
    if (funkyTuples) ty else TupleType((N, ty.toUpper(ty.prov) ) :: Nil)(noProv)

  // built in operators bound to their type schemes
  val builtinBindings: Bindings = {
    val tv = freshVar(noProv)(1)
    import FunctionType.{ apply => fun }
    val intBinOpTy = fun(singleTup(IntType), fun(singleTup(IntType), IntType)(noProv))(noProv)
    val intBinPred = fun(singleTup(IntType), fun(singleTup(IntType), BoolType)(noProv))(noProv)
    val numberBinOpTy = fun(singleTup(DecType), fun(singleTup(DecType), DecType)(noProv))(noProv)
    val numberBinPred = fun(singleTup(DecType), fun(singleTup(DecType), BoolType)(noProv))(noProv)
    Map(
      "true" -> TrueType,
      "false" -> FalseType,
      "document" -> BotType,
      "window" -> BotType,
      "toString" -> fun(singleTup(TopType), StrType)(noProv),
      "not" -> fun(singleTup(BoolType), BoolType)(noProv),
      "succ" -> fun(singleTup(IntType), IntType)(noProv),
      "log" -> PolymorphicType(0, fun(singleTup(tv), UnitType)(noProv)),
      "discard" -> PolymorphicType(0, fun(singleTup(tv), UnitType)(noProv)),
      "negate" -> fun(singleTup(IntType), IntType)(noProv),
      "add" -> intBinOpTy,
      "sub" -> intBinOpTy,
      "mul" -> intBinOpTy,
      "div" -> intBinOpTy,
      "sqrt" -> fun(singleTup(IntType), IntType)(noProv),
      "lt" -> intBinPred,
      "le" -> intBinPred,
      "gt" -> intBinPred,
      "ge" -> intBinPred,
      "concat" -> fun(singleTup(StrType), fun(singleTup(StrType), StrType)(noProv))(noProv),
      "eq" -> {
        val v = freshVar(noProv)(1)
        PolymorphicType(0, fun(singleTup(v), fun(singleTup(v), BoolType)(noProv))(noProv))
      },
      "ne" -> {
        val v = freshVar(noProv)(1)
        PolymorphicType(0, fun(singleTup(v), fun(singleTup(v), BoolType)(noProv))(noProv))
      },
      "error" -> BotType,
      "+" -> intBinOpTy,
      "-" -> intBinOpTy,
      "*" -> intBinOpTy,
      "%" -> intBinOpTy,
      "/" -> intBinOpTy,
      "<" -> intBinPred,
      ">" -> intBinPred,
      "<=" -> intBinPred,
      ">=" -> intBinPred,
      "==" -> intBinPred,
      "&&" -> fun(singleTup(BoolType), fun(singleTup(BoolType), BoolType)(noProv))(noProv),
      "||" -> fun(singleTup(BoolType), fun(singleTup(BoolType), BoolType)(noProv))(noProv),
      "id" -> {
        val v = freshVar(noProv)(1)
        PolymorphicType(0, fun(singleTup(v), v)(noProv))
      },
      "if" -> {
        val v = freshVar(noProv)(1)
        // PolymorphicType(0, fun(singleTup(BoolType), fun(singleTup(v), fun(singleTup(v), v)(noProv))(noProv))(noProv))
        PolymorphicType(0, fun(BoolType, fun(v, fun(v, v)(noProv))(noProv))(noProv))
      },
    ) ++ primTypes
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
      trace(s"$lvl. Typing type $ty") {
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
        TupleType(fields.mapValues(f =>
            FieldType(f.in.map(rec), rec(f.out))(tp(f.toLoc, "tuple field"))
          ))(tyTp(ty.toLoc, "tuple type"))
      case Splice(fields) => 
        SpliceType(fields.map{ 
          case L(l) => {
            val t = rec(l)
            val res = ArrayType(freshVar(t.prov).toUpper(t.prov))(t.prov)
            constrain(t, res)(raise, t.prov, ctx)
            L(t)
          }
          case R(f) => {
            R(FieldType(f.in.map(rec), rec(f.out))(tp(f.toLoc, "splice field")))
          }
          })(tyTp(ty.toLoc, "splice type"))
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
          nt._1 -> FieldType(nt._2.in.map(rec), rec(nt._2.out))(
            tp(App(nt._1, Var("").withLocOf(nt._2)).toCoveringLoc,
              (if (nt._2.in.isDefined) "mutable " else "") + "record field"))
        })(prov)
      case Function(lhs, rhs) => FunctionType(rec(lhs), rec(rhs))(tyTp(ty.toLoc, "function type"))
      case WithExtension(b, r) => WithType(rec(b),
        RecordType(
            r.fields.map { case (n, f) => n -> FieldType(f.in.map(rec), rec(f.out))(
              tyTp(App(n, Var("").withLocOf(f)).toCoveringLoc, "extension field")) }
          )(tyTp(r.toLoc, "extension record")))(tyTp(ty.toLoc, "extension type"))
      case Literal(lit) => ClassTag(lit, lit.baseClasses)(tyTp(ty.toLoc, "literal type"))
      case TypeName("this") =>
        ctx.env.getOrElse("this", err(msg"undeclared this" -> ty.toLoc :: Nil)) match {
          case VarSymbol(t: TypeScheme, _) => t.instantiate
        }
      case tn @ TypeTag(name) => rec(TypeName(name.decapitalize))
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
              else (typeNamed(tyLoc, name.capitalize), ctx.tyDefs.get(name.capitalize)) match {
                case (R((kind, _)), S(td)) => kind match {
                  case Cls => clsNameToNomTag(td)(tyTp(tyLoc, "class tag"), ctx)
                  case Trt => trtNameToNomTag(td)(tyTp(tyLoc, "trait tag"), ctx)
                  case Als => err(
                    msg"Type alias ${name.capitalize} cannot be used as a type tag", tyLoc)(raise)
                  case Nms => err(
                    msg"Namespaces ${name.capitalize} cannot be used as a type tag", tyLoc)(raise)
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
      val thing = fs match {
        case (S(_), _) :: Nil => "field"
        case Nil => "empty tuple"
        case _ => "tuple"
      }
      warn(s"Useless $thing in statement position.", t.toLoc)
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
        val prov = tp(if (verboseConstraintProvenanceHints) v.toLoc else N, "variable")
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
      case intlit: IntLit => TypeRef(TypeName("int"), Nil)(prov)
      case strlit: StrLit => TypeRef(TypeName("string"), Nil)(prov)
      case declit: DecLit => TypeRef(TypeName("float"), Nil)(prov)
      case lit: Lit => ClassTag(lit, lit.baseClasses)(prov)
      case App(Var("neg" | "~"), trm) => typeTerm(trm).neg(prov)
      case App(App(Var("|"), lhs), rhs) =>
        typeTerm(lhs) | (typeTerm(rhs), prov)
      case App(App(Var("&"), lhs), rhs) =>
        typeTerm(lhs) & (typeTerm(rhs), prov)
      case Rcd(fs) =>
        val prov = tp(term.toLoc, "record literal")
        fs.groupMap(_._1.name)(_._1).foreach { case s -> fieldNames if fieldNames.sizeIs > 1 => err(
            msg"Multiple declarations of field name ${s} in ${prov.desc}" -> term.toLoc
              :: fieldNames.map(tp => msg"Declared at" -> tp.toLoc))(raise)
          case _ =>
        }
        RecordType.mk(fs.map { case (n, Fld(mut, _, t)) => 
          if (n.name.isCapitalized)
            err(msg"Field identifiers must start with a small letter", term.toLoc)(raise)
          val tym = typeTerm(t)
          val fprov = tp(App(n, t).toLoc, (if (mut) "mutable " else "") + "record field")
          if (mut) {
            val res = freshVar(fprov, S(n.name))
            val rs = con(tym, res, res)
            (n, FieldType(Some(rs), rs)(fprov))
          } else (n, tym.toUpper(fprov))
        })(prov)
      case tup: Tup if funkyTuples =>
        typeTerms(tup :: Nil, false, Nil)
      case tup: Tup if (tup.isInstanceOf[ImplicitTup]) =>
        tup.fields match {
          case ((N, Fld(false, false, t)) :: Nil) =>
            val tym = typeTerm(t)
            // Note: Do not pass extra prov here
            // an implicit tuple is a function argument
            // App(f, a) case already creates a proxy type
            // to store provance of argument storing prov
            // again leads to duplication
            val rettype = TupleType((N, tym.toUpper(noProv)) :: Nil)(noProv)
            rettype.implicitTuple = true
            rettype
          case _ => err(msg"Implicit tuple with no fields is not allowed", N)(raise)
        }
      case Tup(fs) =>
        TupleType(fs.map { case (n, Fld(mut, _, t)) =>
          val tym = typeTerm(t)
          val fprov = tp(t.toLoc, (if (mut) "mutable " else "") + "tuple field")
          if (mut) {
            val res = freshVar(fprov, n.map(_.name))
            val rs = con(tym, res, res)
            (n, FieldType(Some(rs), rs)(fprov))
          } else (n, tym.toUpper(fprov))
        })(fs match {
          case Nil | ((N, _) :: Nil) => noProv
          case _ => tp(term.toLoc, "tuple literal")
        })
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
      case Splc(es) => 
        SpliceType(es.map{
          case L(l) => L({
            val t_l = typeTerm(l)
            val t_a = ArrayType(freshVar(prov).toUpper(prov))(prov)
            con(t_l, t_a, t_l)
          }) 
          case R(Fld(mt, sp, r)) => {
            val t = typeTerm(r)
            if (mt) { R(FieldType(Some(t), t)(t.prov)) } else {R(t.toUpper(t.prov))}
          }
        })(prov)
      case Bra(false, trm: Blk) => typeTerm(trm)
      case Bra(rcd, trm @ (_: Tup | _: Blk)) if funkyTuples => typeTerms(trm :: Nil, rcd, Nil)
      case Bra(_, trm) => typeTerm(trm)
      case Blk((s: Term) :: Nil) => typeTerm(s)
      case Blk(Nil) => UnitType.withProv(prov)
      case pat if ctx.inPattern =>
        err(msg"Unsupported pattern shape${
          if (dbg) " ("+pat.getClass.toString+")" else ""}:", pat.toLoc)(raise)
      case Lam(pat, body) =>
        val newCtx = ctx.nest
        val param_ty = typePattern(pat)(newCtx, raise, vars)
        val body_ty = typeTerm(body)(newCtx, raise, vars)
        FunctionType(param_ty, body_ty)(tp(term.toLoc, "function"))
      case App(App(Var("and"), lhs), rhs) =>
        val lhs_ty = typeTerm(lhs)
        val newCtx = ctx.nest // TODO use
        val rhs_ty = typeTerm(lhs)
        ??? // TODO
      case App(f, a) =>
        val f_ty = typeTerm(f)
        val a_ty = typeTerm(a)
        val res = freshVar(prov)
        val arg_ty = mkProxy(a_ty, tp(a.toCoveringLoc, "argument"))
          // ^ Note: this no longer really makes a difference, due to tupled arguments by default
        val funProv = tp(f.toCoveringLoc, "applied expression")
        // val fun_ty = mkProxy(f_ty, funProv)
        val fun_ty = f_ty
          // ^ This is mostly not useful, except in test Tuples.fun with `(1, true, "hey").2`
        val resTy = con(fun_ty, FunctionType(arg_ty, res)(
          hintProv(prov)
          // funProv // TODO: better?
          ), res)
        resTy
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
        val n_ty = typeLetRhs(isrec, nme.name, rhs)
        val newCtx = ctx.nest
        newCtx += nme.name -> VarSymbol(n_ty, nme)
        typeTerm(bod)(newCtx, raise)
      // case Blk(s :: stmts) =>
      //   val (newCtx, ty) = typeStatement(s)
      //   typeTerm(Blk(stmts))(newCtx, lvl, raise)
      case Blk(stmts) => typeTerms(stmts, false, Nil)(ctx.nest, raise, prov)
      case Bind(l, r) =>
        val l_ty = typeTerm(l)
        val newCtx = ctx.nest // so the pattern's context don't merge with the outer context!
        val r_ty = typePattern(r)(newCtx, raise)
        ctx ++= newCtx.env
        con(l_ty, r_ty, r_ty)
      case Test(l, r) =>
        val l_ty = typeTerm(l)
        val newCtx = ctx.nest
        val r_ty = typePattern(r)(newCtx, raise) // TODO make these bindings flow
        con(l_ty, r_ty, TopType)
        BoolType
      case With(t, rcd) =>
        val t_ty = typeTerm(t)
        val rcd_ty = typeTerm(rcd)
        (t_ty without rcd.fields.iterator.map(_._1).toSortedSet) & (rcd_ty, prov)
      case CaseOf(s, cs) =>
        val s_ty = typeTerm(s)
        val (tys, cs_ty_ls) = typeArms(s |>? {
          case v: Var => v
          case Asc(v: Var, _) => v
        }, cs)
        val req = tys.foldRight(BotType: SimpleType) {
          case ((a_ty, tv), req) => a_ty & tv | req & a_ty.neg()
        }
        val cs_ty = freshVar(prov, lbs = cs_ty_ls)
        con(s_ty, req, cs_ty)
      case iff @ If(cond, body) =>
        println(PrettyPrintHelper.inspect(iff))
        body match {
          // handle if-then-else separately
          case Ls(IfThen(Var("True"), trueArm), IfThen(Var("False"), falseArm)) =>
            val cond_ty = typeTerm(cond, "if-then-else condition type")
            con(cond_ty, BoolType, cond_ty)
            val ret_ty = freshVar(prov.copy(desc = "if-then-else return type"))
            con(typeTerm(trueArm, "if-then-else true condition"), ret_ty, ret_ty)
            con(typeTerm(falseArm, "if-then-else false condition"), ret_ty, ret_ty)
          // TODO handle let deconstruction differently
          case _ =>
            // find constructor for each arm
            val adtData = body.flatMap(_.topLevelCtors).map { case Var(name) =>
              ctx.tyDefs.getOrElse(name, lastWords(s"could not find type defintion ${name}"))
                .adtData.getOrElse(lastWords(s"could not find adt data for type defintion ${name}"))
            }
            // find and retrieve the common adt for all the constructors
            val adtName = adtData match {
              case Nil => lastWords("match case without any arms")
              case adt :: Nil => adt._1
              case head :: tail =>
                val adtName = head._1
                tail.foreach(v => if (v._1 =/= adtName) lastWords(s"incompatible adt data for ${head} and ${v}"))
                adtName
            }
            println(s"ADT name: $adtName")

            val adtDef = ctx.tyDefs.getOrElse(adtName.name, lastWords(s"Could not find ${adtName} in context"))
            // TODO should the type variables be cloned here
            val newTargs = adtDef.targs.map(tv => freshVar(tv.prov, tv.nameHint))
            val adt_ty = TypeRef(adtName, newTargs)(TypeProvenance(cond.toLoc, "match-case condition type"))
            println(s"ADT type: $adt_ty")
            
            val cond_ty = typeTerm(cond)
            val ret_ty = freshVar(prov.copy(desc = "match-case return type"))
            con(cond_ty, adt_ty, adt_ty)

            // the assumed shape of an IfBody is a List[IfThen, IfThen, IfElse] with an optional IfElse at the end
            body.zipWithIndex.foreach {
              // case x -> expr catch all with a new variable in the context
              case (IfThen(v@Var(name), rhs), _) =>
                // update context with variables
                val newCtx = ctx.nest
                newCtx += name -> VarSymbol(cond_ty, v)
                con(typeTerm(rhs), ret_ty, ret_ty)
              // case Left x -> expr or Left 2 -> expr the type variable for the adt is constrained
              // with the argument to the constructor Left in this case
              case (IfThen(App(Var(ctorNme), Tup(fields)), rhs), index) =>
                println(s"Typing case $index ($ctorNme)")
                val ctorType = ctx.get(ctorNme) match {
                  case S(VarSymbol(PolymorphicType(_, res), _)) => res
                  case _ => die
                }
                println(s"ctor type: ${ctorType}")
                def destructCtor(ctorType: ST, acc: Ls[ST]): Ls[ST] -> Ls[TV] = ctorType.unwrapProxies match {
                  case FunctionType(fieldTy, rest) =>
                     destructCtor(rest, fieldTy :: acc)
                  case TypeRef(defn, targs) =>
                    assert(defn === adtName)
                    acc.reverse -> targs.map {
                      case tv: TV => tv
                      case _ => die
                    }
                  case _ => die
                }
                val (originalFieldTypes, ctorTargs) = destructCtor(ctorType, Nil)
                assert(ctorTargs.sizeCompare(newTargs) === 0)
                val mapping = ctorTargs.zip(newTargs).toMap[ST, ST]
                val fieldTypes = originalFieldTypes.map { ft => subst(ft, mapping) }
                println(s"fieldTypes: $fieldTypes")
                val adtArgIndex = adtData(index)._2
                val nestCtx = ctx.nest
                fields.map(_._2.value).zip(adtArgIndex).zipWithIndex.foreach {
                  // in case of Left x also add x to nested scope
                  case ((argTerm: Var, adtArgIndex), fieldIdx) =>
                    println(s"Typing field $argTerm ($adtArgIndex)")
                    // val adtTvar = newTargs(adtArgIndex)
                    val fieldType = fieldTypes(fieldIdx)
                    println(s"Field $argTerm : $fieldType")
                    nestCtx += argTerm.name -> VarSymbol(fieldType, argTerm)
                    // con(adtTvar, typeTerm(argTerm), adtTvar)
                  case ((argTerm, adtArgIndex), fieldIdx) =>
                    val adtTvar = newTargs(adtArgIndex)
                    // con(adtTvar, typeTerm(argTerm), adtTvar)
                    ???
                }
                // con(typeTerm(rhs), ret_ty, ret_ty)
                // implicit val ctx: Ctx = nestCtx
                nestCtx |> { implicit ctx =>
                  con(typeTerm(rhs), ret_ty, ret_ty)
                }
              case (IfElse(expr), _) => con(typeTerm(expr), ret_ty, ret_ty)
              case ifbody => lastWords(s"Cannot handle case expression ${ifbody}")
            }
            ret_ty
        }
      case New(S((nmedTy, trm)), TypingUnit(Nil)) =>
        typeTerm(App(Var(nmedTy.base.name).withLocOf(nmedTy), trm))
      case New(base, args) => ???
      case TyApp(_, _) => ??? // TODO
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
      typeTerms(Tup(S(trm) -> Fld(false, false, trm) :: Nil) :: sts, rcd, fields)
    case Blk(sts0) :: sts1 => typeTerms(sts0 ::: sts1, rcd, fields)
    case Tup(Nil) :: sts => typeTerms(sts, rcd, fields)
    case Tup((no, Fld(tmut, _, trm)) :: ofs) :: sts =>
      val ty = {
        trm match  {
          case Bra(false, t) if ctx.inPattern => // we use syntax `(x: (p))` to type `p` as a pattern and not a type...
            typePattern(t)
          case _ => ctx.copy(inPattern = ctx.inPattern && no.isEmpty) |> { implicit ctx => // TODO change this?
            if (ofs.isEmpty) typeTerm(Bra(rcd, trm))
            // ^ This is to type { a: ... } as { a: { ... } } to facilitate object literal definitions;
            //   not sure that's a good idea...
            else typeTerm(trm)
          }
        }
      }
      val res_ty = no |> {
        case S(nme) if ctx.inPattern =>
          // TODO in 'opaque' definitions we should give the exact specified type and not something more precise
          // as in `(x: Int) => ...` should not try to refine the type of `x` further
          
          val prov = tp(trm.toLoc, "parameter type")
          val t_ty =
            // TODO in positive position, this should create a new VarType instead! (i.e., an existential)
            new TypeVariable(lvl, Nil, Nil)(prov)//.tap(ctx += nme -> _)
          
          // constrain(ty, t_ty)(raise, prov)
          constrain(t_ty, ty)(raise, prov, ctx)
          ctx += nme.name -> VarSymbol(t_ty, nme)
          
          t_ty
          // ty
          // ComposedType(false, t_ty, ty)(prov)
          // ComposedType(true, t_ty, ty)(prov) // loops!
          
        case S(nme) =>
          ctx += nme.name -> VarSymbol(ty, nme)
          ty
        case _ =>
          ty
      }
      typeTerms(Tup(ofs) :: sts, rcd, (no, res_ty) :: fields)
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
  
  case class UnificationStore() {
  /** Unification happens because of previous type variable. Dir indicates
   * if the st is an lb of the prev (true) or an ub of prev (false)
     */
    case class UnificationReason(st: ST, prev: ST, dir: Bool) {
      override def toString: String = dir match {
        case true => s"${st.unwrapProvs} <: ${prev}"
        case false => s"${st.unwrapProvs} :> ${prev}"
      }
      def toDiagnostic(implicit ctx: Ctx): Ls[Message -> Opt[Loc]] = {
        val stUnder = st.unwrapProvs
        val prevUnder = prev.unwrapProvs
        if (dir) {
          msg"${stUnder.toString} flows into ${prevUnder.toString}" -> N ::
            firstAndLastUseLocation(prev) ::: firstAndLastUseLocation(st)
        } else {
          msg"${prevUnder.toString} flows into ${stUnder.toString}" -> N ::
            firstAndLastUseLocation(st) ::: firstAndLastUseLocation(prev)
        }
      }
    }

    /** A unification can create an message showing relevant locations */
    def createErrorMessage(u: Unification)(implicit ctx: Ctx): Ls[Message -> Opt[Loc]] = {
      def helper(a: ST, b: ST, u: Unification): Ls[Message -> Opt[Loc]] = {
        val tvars = u.unifyingTypeVars.filter{case (_, (l, r)) => l >= 2 || r >= 2}.keys
        val tvarMessage: Ls[Message -> Opt[Loc]] = if (tvars.nonEmpty) {
          msg"The following tvars cannot be resolved ${tvars.mkString(", ")}" -> N :: tvars.flatMap(firstAndLastUseLocation).toList
        } else {
          Nil
        }
        msg"[UNIFICATION ERROR ${u.level.toString}] ${a.expPos} and ${b.expPos} cannot be unified but flows into the same location" -> N ::
          firstAndLastUseLocation(a) ::: firstAndLastUseLocation(b) ::: tvarMessage
      }
      u match {
        case CommonLower(common, a, b) => helper(a, b, u)
        case CommonUpper(common, a, b) => helper(a, b, u)
        case TypeRefArg(a, b, _, _, _, _) => helper(a, b, u)
        case TupleField(a, b, _, _, _) => helper(a, b, u)
        case FunctionArg(a, b, _, _) => helper(a, b, u)
        case FunctionResult(a, b, _, _) => helper(a, b, u)
        case Connector(a, b, uforB, uforA) => helper(a, b, u)
        // these unifications cannot produce an error by themselves
        case LowerBound(tv, st) => firstAndLastUseLocation(st)
        case UpperBound(tv, st) => firstAndLastUseLocation(st)
      }
    }

    def unifyTypes(a: ST, b: ST, u: Unification)(implicit cache: MutSet[(ST, ST)], ctx: Ctx, raise: Raise): Unit = {
      val st1 = a.unwrapProvs
      val st2 = b.unwrapProvs

      if (cache((st1, st2))) return
      else cache += ((st1, st2))

      (st1, st2) match {
        case (c1: ClassTag, c2: ClassTag) =>
          if (c1 =/= c2) {
            // report error
            println(s"[ERROR ${u.level}] ${c1} != ${c2} unifying because ${u}")
            // TODO: type variables are unwrapped so no prov info is available
            raise(WarningReport(createErrorMessage(u)))
          }
        case (tr1: TypeRef, tr2: TypeRef) =>
          if (tr1.defn === tr2.defn && tr1.targs.length === tr2.targs.length) {
            tr1.targs.zip(tr2.targs).zipWithIndex.foreach {
              case ((arg1, arg2), i) => unifyTypes(arg1, arg2, TypeRefArg(arg1, arg2, tr1.defn, i, tr1, tr2))
            }
          } else {
            // report error
            println(s"[ERROR ${u.level}] ${tr1} != ${tr2} unifying because ${u}")
            raise(WarningReport(createErrorMessage(u)))
          }
        case (tup1: TupleType, tup2: TupleType)
          if ((tup1.implicitTuple && tup2.implicitTuple) ||
              (tup1.implicitTuple && tup2.fields.length == 1) ||
              (tup2.implicitTuple && tup1.fields.length == 1)
             ) =>
          // implicit tuple can be considered as transparent
          // replace previous unification reasons types of
          // the fields of these tuple types
          val st1 = tup1.fields.head._2.ub
          val st2 = tup2.fields.head._2.ub
          val newU = u.replaceTypes(st1, st2)
          unifyTypes(st1, st2, newU)
        case (tup1: TupleType, tup2: TupleType) =>
          if (tup1.fields.length === tup2.fields.length) {
            tup1.fields.map(_._2.ub).zip(tup2.fields.map(_._2.ub)).zipWithIndex.foreach {
              case ((t1, t2), i) => unifyTypes(t1, t2, TupleField(t1, t2, i, tup1, tup2))
            }
          } else {
            // report error
            println(s"[ERROR ${u.level}] ${tup1} != ${tup2} unifying because ${u}")
            raise(WarningReport(createErrorMessage(u)))
          }
        case (f1@FunctionType(arg1, res1), f2@FunctionType(arg2, res2)) =>
          unifyTypes(arg1, arg2, FunctionArg(arg1, arg2, f1, f2))
          unifyTypes(res1, res2, FunctionResult(res1, res2, f1, f2))
        case (tv1: TypeVariable, tv2: TypeVariable) if tv1 === tv2 =>
          () // report error
        // pass prov wrapped types so that locations get stored in unification
        case (tv1: TypeVariable, tv2: TypeVariable) => unifyWithTypeVar(tv1, b, u)
        case (tv1: TypeVariable, _) => unifyWithTypeVar(tv1, b, u)
        case (_, tv2: TypeVariable) => unifyWithTypeVar(tv2, a, u)
        case (_, _) =>
          // report error
          println(s"[ERROR ${u.level}] ${st1} != ${st2} unifying because ${u}")
          raise(WarningReport(createErrorMessage(u)))
      }
    }

    def unifyWithTypeVar(tv: TV, st: ST, u: Unification)(implicit cache: MutSet[(ST, ST)], ctx: Ctx, raise: Raise) = {
      tv.unification.foreach(prevU => (prevU, u) match {
        case (LowerBound(_, lb), _: LowerBound) => unifyTypes(lb, st, CommonUpper(tv, lb, st))
        case (UpperBound(_, ub), _: UpperBound) => unifyTypes(ub, st, CommonLower(tv, ub, st))
        // Information is lost int <: a <: b, b does not know it was unified with int through a
        case (LowerBound(_, lb), _: UpperBound) => unifyTypes(lb, st, Connector(lb, st, u, prevU))
        case (UpperBound(_, ub), _: LowerBound) => unifyTypes(ub, st, Connector(ub, st, u, prevU))
        case _ => unifyTypes(prevU.unifiedWith, st, Connector(prevU.unifiedWith, st, u, prevU))
      })
      tv.unification = u :: tv.unification
    }
  }
  
  /** Unifies type bounds to find unification errors. These errors are not
    * detected by mlscript type system because it has sub-typing.
    *
    * @param st
    * @param ctx
    * @param raise
    */
  def unifyType()(implicit ctx: Ctx, raise: Raise) = {
    val unificationStore = UnificationStore()
    val tvars = TypeVariable.createdTypeVars.reverse
    implicit val cache: MutSet[(ST, ST)] = MutSet()
//    println(s"[UNIFICATION] unifying the following types: ${tvars}")
//    tvars.foreach(unificationStore.traverseTypeBounds(_, Nil))
    tvars.foreach(tv => {
      tv.upperBounds.foreach(ub => unificationStore.unifyTypes(tv, ub, UpperBound(tv, ub)))
      tv.lowerBounds.foreach(lb => unificationStore.unifyTypes(tv, lb, LowerBound(tv, lb)))
    })
    tvars.foreach(tv => {
      println(s"unified ${tv}")
      tv.unification.foreach(u => println(s" ${u}"))
    })
  }
  
  /** Convert an inferred SimpleType into the immutable Type representation. */
  def expandType(st: SimpleType, stopAtTyVars: Bool = false)(implicit ctx: Ctx): Type = {
    val expandType = ()
    
    import Set.{empty => semp}
    
    var bounds: Ls[TypeVar -> Bounds] = Nil
    
    val seenVars = mutable.Set.empty[TV]
    
    def field(ft: FieldType): Field = ft match {
      case FieldType(S(l: TV), u: TV) if l === u =>
        val res = go(u)
        Field(S(res), res) // TODO improve Field
      case f =>
        Field(f.lb.map(go), go(f.ub))
    }
    
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
        case TupleType(fs) => Tuple(fs.mapValues(field))
        case ArrayType(FieldType(None, ub)) => AppliedType(TypeName("Array"), go(ub) :: Nil)
        case ArrayType(f) =>
          val f2 = field(f)
          AppliedType(TypeName("MutArray"), Bounds(f2.in.getOrElse(Bot), f2.out) :: Nil)
        case SpliceType(elems) => Splice(elems.map { 
              case L(l) => L(go(l)) 
              case R(v) => R(Field(v.lb.map(go(_)), go(v.ub))) })
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
