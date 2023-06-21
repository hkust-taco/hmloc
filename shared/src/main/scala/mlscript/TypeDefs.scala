package mlscript

import scala.collection.mutable
import scala.collection.mutable.{Map => MutMap, Set => MutSet}
import scala.collection.immutable.{SortedSet, SortedMap}
import scala.util.chaining._
import scala.annotation.tailrec
import mlscript.utils._, shorthands._
import mlscript.Message._

class TypeDefs extends UnificationSolver { self: Typer =>
  import TypeProvenance.{apply => tp}
  
  
  /**
   * TypeDef holds information about declarations like classes, interfaces, and type aliases
   *
   * @param kind tells if it's a class, interface or alias
   * @param nme name of the defined type
   * @param tparamsargs list of type parameter names and their corresponding type variable names used in the definition of the type
   * @param tvars
   * @param bodyTy type of the body, this means the fields of a class or interface or the type that is being aliased
   * @param mthDecls method type declarations in a class or interface, not relevant for type alias
   * @param mthDefs method definitions in a class or interface, not relevant for type alias
   * @param baseClasses base class if the class or interface inherits from any
   * @param toLoc source location related information
   * @param positionals positional term parameters of the class
   */
  case class TypeDef(
    kind: TypeDefKind,
    nme: TypeName,
    tparamsargs: List[(TypeName, TypeVariable)],
    tvars: Iterable[TypeVariable], // "implicit" type variables. instantiate every time a `TypeRef` is expanded
    bodyTy: SimpleType,
    baseClasses: Set[TypeName],
    toLoc: Opt[Loc],
    positionals: Ls[Str],
    // maps a class to it's adt by name and maps params to adt param by position
    // for e.g. in type 'a, 'b either = Left of 'a | Right of 'b
    // Right will have an adtData = S((TypeName("either"), List(1)))
    // indicating that it's adt is either and it's param is the 1th param of either
    adtData: Opt[(TypeName, Ls[Int])] = N
  ) {
    def allBaseClasses(ctx: Ctx)(implicit traversed: Set[TypeName]): Set[TypeName] =
      baseClasses.map(v => TypeName(v.name)) ++
        baseClasses.iterator.filterNot(traversed).flatMap(v =>
          ctx.tyDefs.get(v.name).fold(Set.empty[TypeName])(_.allBaseClasses(ctx)(traversed + v)))
    val (tparams: List[TypeName], targs: List[TypeVariable]) = tparamsargs.unzip
    val thisTv: TypeVariable = freshVar(noProv, S("this"), Nil, TypeRef(nme, targs)(noProv) :: Nil)(1)
    var tvarVariances: Opt[VarianceStore] = N
    def getVariancesOrDefault: collection.Map[TV, VarianceInfo] =
      tvarVariances.getOrElse(Map.empty[TV, VarianceInfo].withDefaultValue(VarianceInfo.in))
  }
  
  def tparamField(clsNme: TypeName, tparamNme: TypeName): Var =
    Var(clsNme.name + "#" + tparamNme.name)
  
  def clsNameToNomTag(td: TypeDef)(prov: TypeProvenance, ctx: Ctx): ClassTag = {
    require(td.kind is Cls)
    ClassTag(Var(td.nme.name), ctx.allBaseClassesOf(td.nme.name))(prov)
  }
  
  def baseClassesOf(tyd: mlscript.TypeDef): Set[TypeName] =
    if (tyd.kind === Als) Set.empty else baseClassesOf(tyd.body)
  
  private def baseClassesOf(ty: Type): Set[TypeName] = ty match {
      case Inter(l, r) => baseClassesOf(l) ++ baseClassesOf(r)
      case TypeName(nme) => Set.single(TypeName(nme))
      case AppliedType(b, _) => baseClassesOf(b)
      case Record(_) => Set.empty
      case _: Union => Set.empty
      case _ => Set.empty // TODO TupleType?
    }
  
  
  /** Only supports getting the fields of a valid base class type.
   * Notably, does not traverse type variables. 
   * Note: this does not retrieve the positional fields implicitly defined by tuples */
  def fieldsOf(ty: SimpleType, paramTags: Bool)(implicit ctx: Ctx): Map[Var, ST] =
  // trace(s"Fields of $ty {${travsersed.mkString(",")}}")
  {
    ty match {
      case tr @ TypeRef(td, targs) =>
        fieldsOf(tr.expandWith(paramTags), paramTags)
      case ComposedType(false, l, r) =>
        mergeMap(fieldsOf(l, paramTags), fieldsOf(r, paramTags))(_ & _)
      case RecordType(fs) => fs.toMap
      case p: ProxyType => fieldsOf(p.underlying, paramTags)
      case TypeBounds(lb, ub) => fieldsOf(ub, paramTags)
      case _: ObjectTag | _: FunctionType | _: TupleType | _: TypeVariable
        | _: ExtrType | _: ComposedType => Map.empty
    }
  }

  /** Add type definitions to context. Raises error if a type defintion
    * has already been defined or it is malformed.
    *
    * @param newDefs0
    *   defintions created in current block
    * @param ctx
    *   previous context
    * @param raise
    *   method to raise errors
    * @return
    */
  def processTypeDefs(newDefs0: List[mlscript.TypeDef])(implicit ctx: Ctx, raise: Raise): Ctx = {
    
    var allDefs = ctx.tyDefs
    val allEnv = ctx.env.clone
    val newDefsInfo = newDefs0.iterator.map { case td => td.nme.name -> (td.kind, td.tparams.size) }.toMap
    val newDefs = newDefs0.flatMap { td0 =>
      val n = td0.nme.name
      val td = td0
      // val n = td0.nme.name.capitalize
      // val td = if (td0.nme.name.isCapitalized) td0
      // else {
      //   err(msg"Type names must start with a capital letter", td0.nme.toLoc)
      //   td0.copy(nme = td0.nme.copy(n).withLocOf(td0.nme)).withLocOf(td0)
      // }
      if (primitiveTypes.contains(n)) {
        err(msg"Type name '$n' is reserved.", td.nme.toLoc)
      }
      td.tparams.groupBy(_.name).foreach { case s -> tps if tps.sizeIs > 1 => err(
          msg"Multiple declarations of type parameter ${s} in ${td.kind.str} definition" -> td.toLoc
            :: tps.map(tp => msg"Declared at" -> tp.toLoc))
        case _ =>
      }
      allDefs.get(n) match {
        case S(other) =>
          err(msg"Type '$n' is already defined.", td.nme.toLoc)
          N
        case N =>
          val dummyTargs = td.tparams.map(p =>
            freshVar(originProv(p.toLoc, s"${td.kind.str} type parameter", p.name), S(p.name))(ctx.lvl + 1))
          val tparamsargs = td.tparams.lazyZip(dummyTargs)
          val (bodyTy, tvars) = 
            typeType2(td.body, simplify = false)(ctx.copy(lvl = 0), raise, tparamsargs.map(_.name -> _).toMap, newDefsInfo)
          val td1 = TypeDef(td.kind, td.nme, tparamsargs.toList, tvars, bodyTy, baseClassesOf(td), td.toLoc, td.positionals.map(_.name), td.adtData)
          allDefs += n -> td1
          S(td1)
      }
    }
    import ctx.{tyDefs => oldDefs}
    
    /* Type the bodies of type definitions, ensuring the correctness of parent types
     * and the regularity of the definitions, then register the constructors and types in the context. */
    def typeTypeDefs(implicit ctx: Ctx): Ctx =
      ctx.copy(tyDefs = oldDefs ++ newDefs.flatMap { td =>
        implicit val prov: TypeProvenance = tp(td.toLoc, "type definition")
        val n = td.nme
        
        def checkCycle(ty: SimpleType)(implicit travsersed: Set[TypeName \/ TV]): Bool =
            // trace(s"Cycle? $ty {${travsersed.mkString(",")}}") {
            ty match {
          case TypeRef(tn, _) if travsersed(L(tn)) =>
            err(msg"illegal cycle involving type ${tn}", prov.loco)
            false
          case tr @ TypeRef(tn, targs) => checkCycle(tr.expand)(travsersed + L(tn))
          case ComposedType(_, l, r) => checkCycle(l) && checkCycle(r)
          case p: ProxyType => checkCycle(p.underlying)
          case TypeBounds(lb, ub) => checkCycle(lb) && checkCycle(ub)
          case tv: TypeVariable => travsersed(R(tv)) || {
            val t2 = travsersed + R(tv)
            tv.lowerBounds.forall(checkCycle(_)(t2)) && tv.upperBounds.forall(checkCycle(_)(t2))
          }
          case _: ExtrType | _: ObjectTag | _: FunctionType | _: RecordType | _: TupleType => true
        }
        // }()

        val rightParents = td.kind match {
          case Als => checkCycle(td.bodyTy)(Set.single(L(td.nme)))
          case k: ObjDefKind =>
            val parentsClasses = MutSet.empty[TypeRef]
            def checkParents(ty: SimpleType): Bool = ty match {
              // case ClassTag(Var("string"), _) => true // Q: always?
              case _: ObjectTag => true // Q: always? // FIXME actually no
              case tr @ TypeRef(tn2, _) =>
                val td2 = ctx.tyDefs(tn2.name)
                td2.kind match {
                  case Cls =>
                    if (td.kind is Cls) {
                      parentsClasses.isEmpty || {
                        err(msg"${td.kind.str} $n cannot inherit from class ${tn2
                            } as it already inherits from class ${parentsClasses.head.defn}",
                          prov.loco)
                        false
                      } tap (_ => parentsClasses += tr)
                    } else
                      checkParents(tr.expand)
                  case Als =>
                    err(msg"cannot inherit from a type alias", prov.loco)
                    false
                }
              case ComposedType(false, l, r) => checkParents(l) && checkParents(r)
              case ComposedType(true, l, r) =>
                err(msg"cannot inherit from a type union", prov.loco)
                false
              case tv: TypeVariable =>
                err(msg"cannot inherit from a type variable", prov.loco)
                false
              case _: FunctionType =>
                err(msg"cannot inherit from a function type", prov.loco)
                false
              case _: TupleType =>
                err(msg"cannot inherit from a tuple type", prov.loco)
                false
              case _: TypeBounds =>
                err(msg"cannot inherit from type bounds", prov.loco)
                false
              case _: RecordType | _: ExtrType => true
              case p: ProxyType => checkParents(p.underlying)
            }

            checkParents(td.bodyTy) && checkCycle(td.bodyTy)(Set.single(L(td.nme)))
        }

        def checkRegular(ty: SimpleType)(implicit reached: Map[Str, Ls[SimpleType]]): Bool = ty match {
          case tr @ TypeRef(defn, targs) => reached.get(defn.name) match {
            case None => checkRegular(tr.expandWith(false))(reached + (defn.name -> targs))
            case Some(tys) =>
              // Note: this check *has* to be relatively syntactic because
              //    the termination of constraint solving relies on recursive type occurrences
              //    obtained from unrolling a recursive type to be *equal* to the original type
              //    and to have the same has hashCode (see: the use of a cache MutSet)
              if (defn === td.nme && tys =/= targs) {
                err(msg"Type definition is not regular: it occurs within itself as ${
                  expandType(tr).show
                }, but is defined as ${
                  expandType(TypeRef(defn, td.targs)(noProv)).show
                }", td.toLoc)(raise)
                false
              } else true
          }
          case _ => ty.children(includeBounds = false).forall(checkRegular)
        }
        
        // Note: this will end up going through some types several times... We could make sure to
        //    only go through each type once, but the error messages would be worse.
        if (rightParents && checkRegular(td.bodyTy)(Map(n.name -> td.targs)))
          td.nme.name -> td :: Nil
        else Nil
      })

    typeTypeDefs(ctx.copy(env = allEnv, tyDefs = allDefs))
  }
  
  /**
    * Finds the variances of all type variables in the given type definitions with the given
    * context using a fixed point computation. The algorithm starts with each type variable
    * as bivariant by default and each type defintion position as covariant and
    * then keeps updating the position variance based on the types it encounters.
    * 
    * It uses the results to update variance info in the type defintions
    *
    * @param tyDefs
    * @param ctx
    */
  def computeVariances(tyDefs: List[TypeDef], ctx: Ctx): Unit = {
    println(s"VARIANCE ANALYSIS")
    var varianceUpdated: Bool = false;
    
    /** Update variance information for all type variables belonging
      * to a type definition.
      *
      * @param ty
      *   type tree to check variance for
      * @param curVariance
      *   variance of current position where the type tree has been found
      * @param tyDef
      *   type definition which is currently being processed
      * @param visited
      *   set of type variables visited along with the variance
      *   true polarity if covariant position visit
      *   false polarity if contravariant position visit
      *   both if invariant position visit
      */
    def updateVariance(ty: SimpleType, curVariance: VarianceInfo)(implicit tyDef: TypeDef, visited: MutSet[Bool -> TypeVariable]): Unit = {
      def fieldVarianceHelper(fieldTy: ST): Unit = updateVariance(fieldTy, curVariance)

      trace(s"upd[$curVariance] $ty") {
        ty match {
          case ProxyType(underlying) => updateVariance(underlying, curVariance)
          case ClassTag(_, _) => ()
          case ExtrType(pol) => ()
          case t: TypeVariable =>
            // update the variance information for the type variable
            val tvv = tyDef.tvarVariances.getOrElse(die)
            val oldVariance = tvv.getOrElseUpdate(t, VarianceInfo.bi)
            val newVariance = oldVariance && curVariance
            if (newVariance =/= oldVariance) {
              tvv(t) = newVariance
              println(s"UPDATE ${tyDef.nme.name}.$t from $oldVariance to $newVariance")
              varianceUpdated = true
            }
            val (visitLB, visitUB) = (
              !curVariance.isContravariant && !visited(true -> t),
              !curVariance.isCovariant && !visited(false -> t),
            )
            if (visitLB) visited += true -> t
            if (visitUB) visited += false -> t
            if (visitLB) t.lowerBounds.foreach(lb => updateVariance(lb, VarianceInfo.co))
            if (visitUB) t.upperBounds.foreach(ub => updateVariance(ub, VarianceInfo.contra))
          case RecordType(fields) => fields.foreach {
            case (_ , fieldTy) => fieldVarianceHelper(fieldTy)
          }
          case TypeRef(defn, targs) =>
            // it's possible that the type definition may not exist in the
            // context because it is malformed or incorrect. Do nothing in
            // such cases
            ctx.tyDefs.get(defn.name).foreach(typeRefDef => {
              // variance for all type parameters of type definitions has been preset
              // do nothing if variance for the parameter does not exist
              targs.zip(typeRefDef.tparamsargs).foreach { case (targ, (_, tvar)) =>
                typeRefDef.tvarVariances.getOrElse(die).get(tvar).foreach {
                  case in @ VarianceInfo(false, false) => updateVariance(targ, in)
                  case VarianceInfo(true, false) => updateVariance(targ, curVariance)
                  case VarianceInfo(false, true) => updateVariance(targ, curVariance.flip)
                  case VarianceInfo(true, true) => ()
                }
              }
            })
          case ComposedType(pol, lhs, rhs) =>
            updateVariance(lhs, curVariance)
            updateVariance(rhs, curVariance)
          case TypeBounds(lb, ub) =>
            updateVariance(lb, VarianceInfo.contra)
            updateVariance(ub, VarianceInfo.co)
          case TupleType(fields) => fields.foreach {
              case (_ , fieldTy) => fieldVarianceHelper(fieldTy)
            }
          case FunctionType(lhs, rhs) =>
            updateVariance(lhs, curVariance.flip)
            updateVariance(rhs, curVariance)
        }
      }()
    }
    
    // set default value for all type variables as bivariant
    // this prevents errors when printing type defintions in
    // DiffTests for type variables that are not used at all
    // and hence are not set in the variance info map
    tyDefs.foreach { t =>
      if (t.tvarVariances.isEmpty) {
        // * ^ This may not be empty if the type def was (erroneously) defined several types in the same block
        t.tvarVariances = S(MutMap.empty)
        t.tparamsargs.foreach { case (_, tvar) => t.tvarVariances.getOrElse(die).put(tvar, VarianceInfo.bi) }
      }
    }
    
    var i = 1
    do trace(s"⬤ ITERATION $i") {
      val visitedSet: MutSet[Bool -> TypeVariable] = MutSet()
      varianceUpdated = false;
      tyDefs.foreach {
        case t @ TypeDef(k, nme, _, _, body, _, _, _, _) =>
          trace(s"${k.str} ${nme.name}  ${
                t.tvarVariances.getOrElse(die).iterator.map(kv => s"${kv._2} ${kv._1}").mkString("  ")}") {
            updateVariance(body, VarianceInfo.co)(t, visitedSet)
          }()
      }
      i += 1
    }() while (varianceUpdated)
    println(s"DONE")
  }
  
  case class VarianceInfo(isCovariant: Bool, isContravariant: Bool) {
    
    /** Combine two pieces of variance information together
     */
    def &&(that: VarianceInfo): VarianceInfo =
      VarianceInfo(isCovariant && that.isCovariant, isContravariant && that.isContravariant)
    
    /*  Flip the current variance if it encounters a contravariant position
     */
    def flip: VarianceInfo = VarianceInfo(isContravariant, isCovariant)
    
    override def toString: Str = show
    
    def show: Str = this match {
      case (VarianceInfo(true, true)) => "±"
      case (VarianceInfo(false, true)) => "-"
      case (VarianceInfo(true, false)) => "+"
      case (VarianceInfo(false, false)) => "="
    }
  }
  
  object VarianceInfo {
    val bi: VarianceInfo = VarianceInfo(true, true)
    val co: VarianceInfo = VarianceInfo(true, false)
    val contra: VarianceInfo = VarianceInfo(false, true)
    val in: VarianceInfo = VarianceInfo(false, false)
  }
  
  type VarianceStore = MutMap[TypeVariable, VarianceInfo]
}
