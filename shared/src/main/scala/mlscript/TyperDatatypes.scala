package mlscript

import scala.collection.mutable
import scala.collection.mutable.{Map => MutMap, Set => MutSet}
import scala.collection.immutable.{SortedSet, SortedMap}
import scala.util.chaining._
import scala.annotation.tailrec
import mlscript.utils._, shorthands._
import mlscript.Message._
import sourcecode._

abstract class TyperDatatypes extends TyperHelpers { self: Typer =>
  
  type TN = TypeName
  
  // The data types used for type inference:
  case class TypeProvenance(loco: Opt[Loc], desc: Str, originName: Opt[Str] = N, isType: Bool = false)(implicit val file: FileName, val line: Line) {
    val isOrigin: Bool = originName.isDefined
    def & (that: TypeProvenance): TypeProvenance = this // arbitrary; maybe should do better
    override def toString: Str = (if (isOrigin) "o: " else "") + "‹"+loco.fold(desc)(desc+":"+_)+s"›[${file.value}:${line.value}]"
  }

  case class NestingInfo(info: Str = "<nesting info here>", reversed: Bool = false) {
    override def toString: Str = info
  }
  class NestedTypeProvenance(var chain: Ls[SimpleType], var nestingInfo: NestingInfo = NestingInfo()) extends TypeProvenance(N, "<nested>") {
    override def toString: Str = "<nested> " + chain.mkString(" -> ") + " <nested>"
    
    def updateInfo(newInfo: Str): NestedTypeProvenance = {
      nestingInfo = nestingInfo.copy(info = newInfo)
      this
    }
  }

  object NestedTypeProvenance {
    def apply(chain: Ls[SimpleType], nestingInfo: NestingInfo = NestingInfo()): NestedTypeProvenance = {
      new NestedTypeProvenance(chain, nestingInfo)
    }
  }

  type TP = TypeProvenance
  
  sealed abstract class TypeInfo

  case class VarSymbol(ty: TypeScheme, definingVar: Var) extends TypeInfo
  
  /** A type that potentially contains universally quantified type variables,
    * and which can be isntantiated to a given level. */
  sealed abstract class TypeScheme {
    def uninstantiatedBody: SimpleType
    def instantiate(implicit lvl: Int): SimpleType
  }
  
  /** A type with universally quantified type variables
    * (by convention, those variables of level greater than `level` are considered quantified). */
  case class PolymorphicType(level: Int, body: SimpleType) extends TypeScheme {
    val prov: TypeProvenance = body.prov
    def uninstantiatedBody: SimpleType = body
    def instantiate(implicit lvl: Int): SimpleType = freshenAbove(level, body)
    def rigidify(implicit lvl: Int): SimpleType = freshenAbove(level, body, rigidify = true)
  }
  
  /** `body.get._1`: implicit `this` parameter
    * `body.get._2`: actual body of the method
    * `body` being `None` indicates an error:
    *   - when this MethodType is computed from `MethodSet#processInheritedMethods`,
    *     it means two or more parent classes defined or declared the method
    *     and the current class did not override it;
    *   - when this MethodType is obtained from the environment, it means the method is ambiguous,
    *     which happens when two or more unrelated classes define or declare a method with the same name.
    *     So note that in this case, it will have more than one parent.
    *   Note: This is some fairly brittle and error-prone logic, which would gain to be refactored.
    *     Especially aggravating is the fact that `toPT`/`bodyPT` return `errorType` when `body` is `None`,
    *     whereas this case should probably be checked and carefully considered in each call site.
    * `isInherited`: whether the method declaration comes from the intersection of multiple inherited declarations
   */
  case class MethodType(
    level: Int,
    body: Opt[(SimpleType, SimpleType)],
    parents: List[TypeName],
    isInherited: Bool,
  )(val prov: TypeProvenance) {
    def &(that: MethodType): MethodType = {
      require(this.level === that.level)
      MethodType(level, mergeOptions(this.body, that.body)((b1, b2) => (b1._1 & b2._1, b1._2 & b2._2)),
        (this.parents ::: that.parents).distinct, isInherited = true)(prov)
    }
    val toPT: PolymorphicType =
      body.fold(PolymorphicType(0, errType))(b => PolymorphicType(level, FunctionType(singleTup(b._1), b._2)(prov)))
    val bodyPT: PolymorphicType =
      body.fold(PolymorphicType(0, errType))(b => PolymorphicType(level, ProvType(b._2)(prov)))
  }
  
  /** A type without universally quantified type variables. */
  sealed abstract class SimpleType extends TypeScheme with SimpleTypeImpl {
    val prov: TypeProvenance
    def level: Int
    def uninstantiatedBody: SimpleType = this
    def instantiate(implicit lvl: Int) = this
    constructedTypes += 1
  }
  type ST = SimpleType
  
  sealed abstract class BaseTypeOrTag extends SimpleType
  sealed abstract class BaseType extends BaseTypeOrTag {
    def toRecord: RecordType = RecordType.empty
  }
  sealed abstract class MiscBaseType extends BaseType
  sealed trait Factorizable extends SimpleType
  
  case class FunctionType(lhs: SimpleType, rhs: SimpleType)(val prov: TypeProvenance) extends MiscBaseType {
    lazy val level: Int = lhs.level max rhs.level
    override def toString = s"(${lhs match {
      case TupleType((N, f) :: Nil) => f.toString
      case lhs => lhs
    }} -> $rhs)"
  }
  
  case class RecordType(fields: List[(Var, FieldType)])(val prov: TypeProvenance) extends SimpleType {
    // TODO: assert no repeated fields
    lazy val level: Int = fields.iterator.map(_._2.level).maxOption.getOrElse(0)
    def toInter: SimpleType =
      fields.map(f => RecordType(f :: Nil)(prov)).foldLeft(TopType: ST)(((l, r) => ComposedType(false, l, r)(noProv)))
    def mergeAllFields(fs: Iterable[Var -> FieldType]): RecordType = {
      val res = mutable.SortedMap.empty[Var, FieldType]
      fs.foreach(f => res.get(f._1) match {
        case N => res(f._1) = f._2
        case S(ty) => res(f._1) = ty && f._2
      })
      RecordType(res.toList)(prov)
    }
    def addFields(fs: Ls[Var -> FieldType]): RecordType = {
      val shadowing = fs.iterator.map(_._1).toSet
      RecordType(fields.filterNot(f => shadowing(f._1)) ++ fs)(prov)
    }
    def sorted: RecordType = RecordType(fields.sortBy(_._1))(prov)
    override def toString = s"{${fields.map(f => s"${f._1}: ${f._2}").mkString(", ")}}"
  }
  object RecordType {
    def empty: RecordType = RecordType(Nil)(noProv)
    def mk(fields: List[(Var, FieldType)])(prov: TypeProvenance = noProv): SimpleType =
      if (fields.isEmpty) ExtrType(false)(prov) else RecordType(fields)(prov)
  }
  
  sealed abstract class ArrayBase extends MiscBaseType {
    def inner: FieldType
  }

  case class ArrayType(val inner: FieldType)(val prov: TypeProvenance) extends ArrayBase {
    def level: Int = inner.level
    override def toString = s"Array‹$inner›"
  }

  case class TupleType(fields: List[Opt[Var] -> FieldType])(val prov: TypeProvenance) extends ArrayBase {
    lazy val inner: FieldType = fields.map(_._2).reduceLeftOption(_ || _).getOrElse(BotType.toUpper(noProv))
    lazy val level: Int = fields.iterator.map(_._2.level).maxOption.getOrElse(0)
    lazy val toArray: ArrayType = ArrayType(inner)(prov)  // upcast to array
    var implicitTuple: Bool = false
    override lazy val toRecord: RecordType =
      RecordType(
        fields.zipWithIndex.map { case ((_, t), i) => (Var("_"+(i+1)), t) }
        // Note: In line with TypeScript, tuple field names are pure type system fictions,
        //    with no runtime existence. Therefore, they should not be included in the record type
        //    corresponding to this tuple type.
        //    i.e., no `::: fields.collect { case (S(n), t) => (n, t) }`
      )(prov)
    override def toString =
      s"(${fields.map(f => s"${f._1.fold("")(_.name+": ")}${f._2},").mkString(" ")})"
    // override def toString = s"(${fields.map(f => s"${f._1.fold("")(_+": ")}${f._2},").mkString(" ")})"
  }

  case class SpliceType(elems: Ls[Either[SimpleType, FieldType]])(val prov: TypeProvenance) extends ArrayBase {
    lazy val level: Int = elems.map{ case L(l) => l.level case R(r) => r.level }.max
    lazy val inner: FieldType = elems.map {
      case L(l) => l match { case a: ArrayBase => a.inner case _ => ??? }
      case R(r) => r
    }.reduceLeft(_ || _)

    def updateElems(f: SimpleType => SimpleType, g: SimpleType => SimpleType, 
      h: SimpleType => SimpleType,newProv: TypeProvenance = prov): SpliceType =
      SpliceType(elems.map{case L(l) => L(f(l)) case R(r) => R(r.update(g, h))})(newProv)
  }
  
  /** Polarity `pol` being `true` means Bot; `false` means Top. These are extrema of the subtyping lattice. */
  case class ExtrType(pol: Bool)(val prov: TypeProvenance) extends SimpleType {
    def level: Int = 0
    override def toString = if (pol) "⊥" else "⊤"
  }
  /** Polarity `pol` being `true` means union; `false` means intersection. */
  case class ComposedType(pol: Bool, lhs: SimpleType, rhs: SimpleType)(val prov: TypeProvenance) extends SimpleType {
    def level: Int = lhs.level max rhs.level
    override def toString = s"($lhs ${if (pol) "|" else "&"} $rhs)"
  }
  case class NegType(negated: SimpleType)(val prov: TypeProvenance) extends SimpleType {
    def level: Int = negated.level
    override def toString = s"~(${negated})"
  }
  
  /** Represents a type `base` from which we have removed the fields in `names`. */
  case class Without(base: SimpleType, names: SortedSet[Var])(val prov: TypeProvenance) extends MiscBaseType {
    def level: Int = base.level
    override def toString = s"${base}\\${names.mkString("-")}"
  }
  
  /** A proxy type is a derived type form storing some additional information,
   * but which can always be converted into an underlying simple type. */
  sealed abstract class ProxyType extends SimpleType {
    def level: Int = underlying.level
    def underlying: SimpleType
    override def toString = s"[$underlying]"
  }
  object ProxyType {
    def unapply(proxy: ProxyType): S[ST] =
      S(proxy.underlying)
  }
  
  /** The sole purpose of ProvType is to store additional type provenance info. */
  case class ProvType(underlying: SimpleType)(val prov: TypeProvenance) extends ProxyType {
    override def toString = if (prov is NestedTypeProvenance) s"[$underlying] prov: $prov" else s"[$underlying]"
    // override def toString = s"$underlying[${prov.desc.take(5)}]"
    // override def toString = s"$underlying[${prov.toString.take(5)}]"
    // override def toString = s"$underlying@${prov}"
    // override def toString = showProvOver(true)(""+underlying)
    
    // TOOD override equals/hashCode? — could affect hash consing...
    // override def equals(that: Any): Bool = super.equals(that) || underlying.equals(that)
    // override def equals(that: Any): Bool = unwrapProxies.equals(that)
  }
  
  /** A proxy type, `S with {x: T; ...}` is equivalent to `S\x\... & {x: T; ...}`. */
  case class WithType(base: SimpleType, rcd: RecordType)(val prov: TypeProvenance) extends ProxyType {
    lazy val underlying: ST =
      base.without(rcd.fields.iterator.map(_._1).toSortedSet) & rcd
    override def toString = s"${base} w/ ${rcd}"
  }
  
  type TR = TypeRef
  case class TypeRef(defn: TypeName, targs: Ls[SimpleType])(val prov: TypeProvenance) extends SimpleType {
    def level: Int = targs.iterator.map(_.level).maxOption.getOrElse(0)
    def expand(implicit ctx: Ctx): SimpleType = expandWith(paramTags = true)
    /**
      * Expands a type reference to actual typedef kind it refers to
      *
      * @param paramTags
      * @param ctx
      * @return
      */
    def expandWith(paramTags: Bool)(implicit ctx: Ctx): SimpleType = {
      val td = ctx.tyDefs(defn.name)
      require(targs.size === td.tparamsargs.size)
      lazy val tparamTags =
        if (paramTags) RecordType.mk(td.tparamsargs.map { case (tp, tv) =>
            val tvv = td.getVariancesOrDefault
            tparamField(defn, tp) -> FieldType(
              Some(if (tvv(tv).isCovariant) BotType else tv),
              if (tvv(tv).isContravariant) TopType else tv)(prov)
          })(noProv)
        else TopType
      // substitute the arguments of type def
      // with the arguments given to the type ref
      subst(td.kind match {
        case Als => td.bodyTy
        case Nms => throw new NotImplementedError("Namespaces are not supported yet.")
        case Cls => clsNameToNomTag(td)(prov, ctx) & td.bodyTy & tparamTags
        case Trt => trtNameToNomTag(td)(prov, ctx) & td.bodyTy & tparamTags
      }, td.targs.lazyZip(targs).toMap) //.withProv(prov)
    }
    private var tag: Opt[Opt[ClassTag]] = N
    def mkTag(implicit ctx: Ctx): Opt[ClassTag] = tag.getOrElse {
      val res = ctx.tyDefs.get(defn.name) match {
        case S(td: TypeDef) if td.kind is Cls => S(clsNameToNomTag(td)(noProv, ctx))
        case _ => N
      }
      tag = S(res)
      res
    }
    def mapTargs[R](pol: Opt[Bool])(f: (Opt[Bool], ST) => R)(implicit ctx: Ctx): Ls[R] = {
      val td = ctx.tyDefs(defn.name)
      td.tvarVariances.fold(targs.map(f(N, _))) { tvv =>
        assert(td.tparamsargs.sizeCompare(targs) === 0)
        (td.tparamsargs lazyZip targs).map { case ((_, tv), ta) =>
          tvv(tv) match {
            case VarianceInfo(true, true) =>
              f(N, TypeBounds(BotType, TopType)(noProv))
            case VarianceInfo(co, contra) =>
              f(if (co) pol else if (contra) pol.map(!_) else N, ta)
          }
      }}
    }
    override def toString = showProvOver(false) {
      val displayName =
        if (primitiveTypes.contains(defn.name)) defn.name.capitalize else defn.name
      if (targs.isEmpty) displayName else s"$displayName[${targs.mkString(",")}]"
    }
  }
  
  sealed trait ObjectTag extends BaseTypeOrTag with Ordered[ObjectTag] {
    val id: SimpleTerm
    def compare(that: ObjectTag): Int = this.id compare that.id
  }
  
  case class ClassTag(id: SimpleTerm, parents: Set[TypeName])(val prov: TypeProvenance) extends BaseType with ObjectTag {
    lazy val parentsST = parents.iterator.map(tn => Var(tn.name)).toSet[SimpleTerm]
    def glb(that: ClassTag): Opt[ClassTag] =
      if (that.id === this.id) S(this)
      else if (that.parentsST.contains(this.id)) S(that)
      else if (this.parentsST.contains(that.id)) S(this)
      else N
    def lub(that: ClassTag): Set[ClassTag] = // TODO rm? it's unused
      if (that.id === this.id) Set.single(that)
      else if (that.parentsST.contains(this.id)) Set.single(this)
      else if (this.parentsST.contains(that.id)) Set.single(that)
      // else this.parentsST.union(that.parentsST)
      else Set(this, that)
    def level: Int = 0
    override def toString = showProvOver(false)(id.idStr+s"<${parents.map(_.show).mkString(",")}>")
  }
  
  case class TraitTag(id: SimpleTerm)(val prov: TypeProvenance) extends BaseTypeOrTag with ObjectTag with Factorizable {
    def level: Int = 0
    override def toString = id.idStr
  }
  
  /** `TypeBounds(lb, ub)` represents an unknown type between bounds `lb` and `ub`.
    * The only way to give something such a type is to make the type part of a def or method signature,
    * as it will be replaced by a fresh bounded type variable upon subsumption checking (cf rigidification). */
  case class TypeBounds(lb: SimpleType, ub: SimpleType)(val prov: TypeProvenance) extends SimpleType {
    def level: Int = lb.level max ub.level
    override def toString = s"$lb..$ub"
  }
  object TypeBounds {
    final def mk(lb: SimpleType, ub: SimpleType, prov: TypeProvenance = noProv)(implicit ctx: Ctx): SimpleType =
      if ((lb is ub) || lb === ub || lb <:< ub && ub <:< lb) lb else (lb, ub) match {
        case (TypeBounds(lb, _), ub) => mk(lb, ub, prov)
        case (lb, TypeBounds(_, ub)) => mk(lb, ub, prov)
        case _ => TypeBounds(lb, ub)(prov)
      }
  }
  
  case class FieldType(lb: Option[SimpleType], ub: SimpleType)(val prov: TypeProvenance) {
    def level: Int = lb.map(_.level).getOrElse(ub.level) max ub.level
    def <:< (that: FieldType)(implicit ctx: Ctx, cache: MutMap[ST -> ST, Bool] = MutMap.empty): Bool =
      (that.lb.getOrElse(BotType) <:< this.lb.getOrElse(BotType)) && (this.ub <:< that.ub)
    def && (that: FieldType, prov: TypeProvenance = noProv): FieldType =
      FieldType(lb.fold(that.lb)(l => Some(that.lb.fold(l)(l | _))), ub & that.ub)(prov)
    def || (that: FieldType, prov: TypeProvenance = noProv): FieldType =
      FieldType(for {l <- lb; r <- that.lb} yield (l & r), ub | that.ub)(prov)
    def update(lb: SimpleType => SimpleType, ub: SimpleType => SimpleType): FieldType =
      FieldType(this.lb.map(lb), ub(this.ub))(prov)
    override def toString =
      lb.fold(s"$ub")(lb => s"mut ${if (lb === BotType) "" else lb}..$ub")
  }


  /** Unification meta data records why a unification happened
    *
    * It is named from the perspective of the type variable storing the unification.
    * As in LowerBound(tv, st) stored in a type variable is meant to be read as -
    * tv has lower bound st
    */
  sealed abstract class Unification {
    override def toString: Str = this match {
      case LowerBound(tv, st) => s"${tv} :> ${st}"
      case UpperBound(tv, st) => s"${tv} <: ${st}"
      case CommonLower(common, a, b) => s"${a} & ${b} :> ${common}"
      case CommonUpper(common, a, b) => s"${a} | ${b} <: ${common}"
      case TypeRefArg(a, b, name, index, tr1, tr2) => s"${a} = ${b} are ${name}(${index}) arg type"
      case TupleField(a, b, index, tr1, tr2) => s"${a} = ${b} are Tup(${index}) field type"
      case FunctionArg(a, b, fr1, fr2) => s"${a} = ${b} are arg type in ${fr1} = ${fr2}"
      case FunctionResult(a, b, fr1, fr2) => s"${a} = ${b} are result type in ${fr1} = ${fr2}"
      case Connector(a, b, uforB, uforA) => s"${a} = ${b} because ${uforA} and ${uforB}"
    }
    def unifiedWith: ST = this match {
      case LowerBound(tv, st) => st
      case UpperBound(tv, st) => st
      // always prefer to unify with a type variable
//      case CommonLower(_, a, b) => if (a.isInstanceOf[TypeVariable]) a else b
//      case CommonUpper(_, a, b) => if (a.isInstanceOf[TypeVariable]) a else b
//      case TypeRefArg(a, b, _, _, _, _) => if (a.isInstanceOf[TypeVariable]) a else b
//      case TupleField(a, b, _, _, _) => if (a.isInstanceOf[TypeVariable]) a else b
//      case FunctionArg(a, b, _, _) => if (a.isInstanceOf[TypeVariable]) a else b
//      case FunctionResult(a, b, _, _) => if (a.isInstanceOf[TypeVariable]) a else b
//      case Connector(a, b, _, _) => if (a.isInstanceOf[TypeVariable]) a else b
      case CommonLower(_, a, b) => b
      case CommonUpper(_, a, b) => b
      case TypeRefArg(a, b, _, _, _, _) => b
      case TupleField(a, b, _, _, _) => b
      case FunctionArg(a, b, _, _) => b
      case FunctionResult(a, b, _, _) => b
      case Connector(a, b, _, _) => b
    }

    /** Unifying type variables that have unified two lb or ub types
      *
      * int | bool <: a = 1 => a is a unifying type
      * int | b <: a and => a is a unifying type
      *       b <: bool  => b is a unifying type through a
      */
    def unifyingTypeVars: MutMap[ST, (Int, Int)] = {
      def updateCounter(st: ST, delta: (Int, Int))(implicit counter: MutMap[ST, (Int, Int)]): Unit = {
        st.unwrapProvs match {
          case tv: TypeVariable =>
            // store type variable with all it's provenances
            counter.updateWith(st) {
              case Some((l, u)) => S(l + delta._1, u + delta._2)
              case None => S(delta)
            }
          case _ => ()
        }
      }

      def rec(u: Unification)(implicit counter: MutMap[ST, (Int, Int)]): Unit = u match {
        case LowerBound(tv, st) =>
          updateCounter(tv, (1, 0))
          updateCounter(st, (0, 1))
        case UpperBound(tv, st) =>
          updateCounter(tv, (0, 1))
          updateCounter(st, (1, 0))
        case CommonLower(common, a, b) =>
          updateCounter(common, (0, 2))
          updateCounter(a, (1, 0))
          updateCounter(b, (1, 0))
        case CommonUpper(common, a, b) =>
          updateCounter(common, (2, 0))
          updateCounter(a, (0, 1))
          updateCounter(b, (0, 1))
        case Connector(_, _, uforB, uforA) =>
          rec(uforA)
          rec(uforB)
        case _ => ()
      }

      val counter: MutMap[ST, (Int, Int)] = MutMap()
      rec(this)(counter)
      counter
    }

    /** The level of a unification is the number of tv's it has zig zagged through
      *
      * For e.g.
      *
      * int <: a <: bool = 0
      * int | bool <: a = 1
      * int | b <: a and
      *       b <: bool = 2
      *
      * This is equal to the number of tvs that have unified two lbs or two ubs
      * during a single unification
      */
    def level: Int = unifyingTypeVars.count{ case (_, (l, r)) => l >= 2 || r >= 2}

    /** Replace the unified types in a unification reason
      *
      * Main usage is to update unification reason when unified tuples are paired
      */
    def replaceTypes(newA: ST, newB: ST): Unification = {
      this match {
        case u: CommonLower => u.copy(a = newA, b = newB)
        case u: CommonUpper => u.copy(a = newA, b = newB)
        case u: TypeRefArg => u.copy(a = newA, b = newB)
        case u: TupleField => u.copy(a = newA, b = newB)
        case u: FunctionArg => u.copy(a = newA, b = newB)
        case u: FunctionResult => u.copy(a = newA, b = newB)
        case u: Connector => u.copy(a = newA, b = newB)
        // replace should not be called with any other unification
        case _ => ???
      }
    }

    /** A unification can create an message showing relevant locations */
    def createErrorMessage(implicit ctx: Ctx): Ls[Message -> Opt[Loc]] = {
      def helper(a: ST, b: ST, u: Unification): Ls[Message -> Opt[Loc]] = {
        val tvars = u.unifyingTypeVars.filter{case (_, (l, r)) => l >= 2 || r >= 2}.keys
        val tvarMessage: Ls[Message -> Opt[Loc]] = if (tvars.nonEmpty) {
          msg"The following tvars cannot be resolved ${tvars.mkString(", ")}" -> N :: tvars.flatMap(firstAndLastUseLocation(_)).toList
        } else {
          Nil
        }
        msg"${a.toString} and ${b.toString} cannot be unified but they flow into the same locations" -> N ::
          firstAndLastUseLocation(a) ::: firstAndLastUseLocation(b) ::: tvarMessage
      }
      this match {
        case CommonLower(common, a, b) => helper(a, b, this)
        case CommonUpper(common, a, b) => helper(a, b, this)
        case TypeRefArg(a, b, _, _, _, _) => helper(a, b, this)
        case TupleField(a, b, _, _, _) => helper(a, b, this)
        case FunctionArg(a, b, _, _) => helper(a, b, this)
        case FunctionResult(a, b, _, _) => helper(a, b, this)
        case Connector(a, b, uforB, uforA) => helper(a, b, this)
        // these unifications cannot produce an error by themselves
        case LowerBound(tv, st) => firstAndLastUseLocation(st)
        case UpperBound(tv, st) => firstAndLastUseLocation(st)
      }
    }
  }
  final case class LowerBound(tv: TV, st: ST) extends Unification
  final case class UpperBound(tv: TV, st: ST) extends Unification
  final case class CommonLower(common: TV, a: ST, b: ST) extends Unification
  final case class CommonUpper(common: TV, a: ST, b: ST) extends Unification
  final case class TypeRefArg(a: ST, b: ST, name: TypeName, index: Int, tr1: TypeRef, tr2: TypeRef) extends Unification
  final case class TupleField(a: ST, b: ST, index: Int, tr1: TupleType, tr2: TupleType) extends Unification
  final case class FunctionArg(a: ST, b: ST, fr1: FunctionType, fr2: FunctionType) extends Unification
  final case class FunctionResult(a: ST, b: ST, fr1: FunctionType, fr2: FunctionType) extends Unification

  /** Unifying a and b with metadata about sequence of unification that led to this unification */
  final case class Connector(a: ST, b: ST, uforB: Unification, uforA: Unification) extends Unification

  /** A type variable living at a certain polymorphism level `level`, with mutable bounds.
    * Invariant: Types appearing in the bounds never have a level higher than this variable's `level`. */
  final class TypeVariable(
      val level: Int,
      var lowerBounds: List[SimpleType],
      var upperBounds: List[SimpleType],
      val nameHint: Opt[Str] = N
  )(val prov: TypeProvenance) extends SimpleType with CompactTypeOrVariable with Ordered[TypeVariable] with Factorizable {
    private[mlscript] val uid: Int = { freshCount += 1; freshCount - 1 }
    lazy val asTypeVar = new TypeVar(L(uid), nameHint)
    var unification: Ls[Unification] = Nil
    def compare(that: TV): Int = this.uid compare that.uid
    
    def isRecursive_$(implicit ctx: Ctx) : Bool = (lbRecOccs_$, ubRecOccs_$) match {
      case (S(N | S(true)), _) | (_, S(N | S(false))) => true
      case _ => false
    } 
    /** None: not recursive in this bound; Some(Some(pol)): polarly-recursive; Some(None): nonpolarly-recursive.
      * Note that if we have something like 'a :> Bot <: 'a -> Top, 'a is not truly recursive
      *   and its bounds can actually be inlined. */
    private final def lbRecOccs_$(implicit ctx: Ctx): Opt[Opt[Bool]] =
      TupleType(lowerBounds.map(N -> _.toUpper(noProv)))(noProv).getVarsPol(S(true)).get(this)
    private final def ubRecOccs_$(implicit ctx: Ctx): Opt[Opt[Bool]] =
      TupleType(upperBounds.map(N -> _.toUpper(noProv)))(noProv).getVarsPol(S(false)).get(this)
    
    override def toString: String = showProvOver(false)(nameHint.getOrElse("α") + uid + "'" * level)
  }
  type TV = TypeVariable
  private var freshCount = 0
  def freshVar(p: TypeProvenance, nameHint: Opt[Str] = N, lbs: Ls[ST] = Nil, ubs: Ls[ST] = Nil)
        (implicit lvl: Int): TypeVariable = {
          val tvar = new TypeVariable(lvl, lbs, ubs, nameHint)(p)
          // only collect type variables if the flag is activated
          if (TypeVariable.collectTypeVars) {
            TypeVariable.createdTypeVars = tvar :: TypeVariable.createdTypeVars
          }
          tvar
        }
  def resetState(): Unit = {
    freshCount = 0
  }
  trait CompactTypeOrVariable
  type PolarVariable = (TypeVariable, Boolean)
  object TypeVariable {
    var collectTypeVars: Bool = false
    var createdTypeVars: Ls[TypeVariable] = Nil
    def clearCollectedTypeVars(): Unit = createdTypeVars = Nil
  }
  
  case class NegVar(tv: TV) extends ProxyType with Factorizable {
    lazy val underlying: SimpleType = tv.neg()
    val prov = noProv
  }
  case class NegTrait(tt: TraitTag) extends ProxyType with Factorizable {
    lazy val underlying: SimpleType = tt.neg()
    val prov = noProv
  }
  
}
