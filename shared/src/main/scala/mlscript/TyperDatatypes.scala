package mlscript

import scala.collection.mutable
import scala.collection.mutable.{Map => MutMap, Set => MutSet}
import scala.collection.immutable.{HashMap, SortedMap, SortedSet}
import scala.util.chaining._
import scala.annotation.tailrec
import mlscript.utils._
import shorthands._
import mlscript.Message._
import sourcecode._

import scala.::

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
    override def toString = s"(${lhs} -> $rhs)"
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
    override def toString = if (prov is NestedTypeProvenance) s"{$underlying} prov: $prov" else s"[$underlying]"
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
        if (primitiveTypes.contains(defn.name)) defn.name else defn.name
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

  /** Reads as a is unified with b because of the following reason.
    * Reason is a list of unifications that show how the type travels
    * from a to b. Indicating that a <: b
    *
    * The first reason is closest to b and the last reason is closest to a.
    */
  case class Unification(a: ST, b: ST, reason: Ls[UnificationReason], desc: Str = "") {
    override def toString: Str = reason match {
      case Nil => s"${a} = ${b}"
      case _ => s"${a} = ${b} because ${reason.mkString(",")}"
    }
    def unificationLevel: Int = {
      reason.sliding(2).count {
        case Seq(LB(_, tv1, _), LB(_, tv2, _)) if tv1 === tv2 => true
        case Seq(UB(tv1, _, _), UB(tv2, _, _)) if tv1 === tv2 => true
        case Seq(UB(_, tv1, _), LB(_, tv2, _)) if tv1 === tv2 => true
        case Seq(LB(_, tv1, _), UB(_, tv2, _)) if (tv1: ST) === tv2 => true
        case _ => false
      }
    }

    def unificationChain: Ls[UnificationReason -> Bool] = reason match {
      case Nil => Nil
      case head :: _ => {
        var flow = head match {
          case _: LB => true // flow is from left to right
          case _: UB => false // flow is from right to left
        }

        head -> flow :: reason.sliding(2).collect {
          case Seq(LB(st1, tv1, _), u@LB(st2, tv2, _)) if tv1 === tv2 || st1 === st2 =>
            flow = !flow
            u -> flow
          case Seq(UB(tv1, st1, _), u@UB(tv2, st2, _)) if tv1 === tv2 || st1 === st2 =>
            flow = !flow
            u -> flow
          case Seq(UB(tv1, st1, _), u@LB(st2, tv2, _)) if tv1 == st2 || tv2 == st1 =>
            flow = !flow
            u -> flow
          case Seq(LB(st1, tv1, _), u@UB(tv2, st2, _)) if tv1 == st2 || tv2 == st1 =>
            flow = !flow
            u -> flow
        }.toList
      }
    }

    def unificationSequenceTVars: Set[TV] = {
      val tvSet: MutSet[TV] = MutSet()
      unificationChain.map { case (ur, _) =>
        ur.a.unwrapProvs match { case tv: TypeVariable => tvSet += tv case _ => ()}
        ur.b.unwrapProvs match { case tv: TypeVariable => tvSet += tv case _ => ()}
      }
      tvSet.toSet
    }

    def unificationSequence: Ls[ST -> Bool] = reason match {
      case Nil => Nil
      case head :: _ =>
        var flow: Bool = head match {
          case _: LB => true // flow is from left to right
          case _: UB => false // flow is from right to left
        }

        val (start, end) = head match {
          case UB(tv, st, _) => (tv, st)
          case LB(st, tv, _) => (tv, st)
        }

        start -> flow :: reason.sliding(2).collect {
          case Seq(LB(_, tv1, _), LB(_, tv2, _)) if tv1 == tv2 =>
            flow = !flow
            tv1 -> flow
          case Seq(UB(tv1, _, _), UB(tv2, _, _)) if tv1 == tv2 =>
            flow = !flow
            tv1 -> flow
          case Seq(UB(_, tv1, _), LB(_, tv2, _)) if tv1 == tv2 =>
            flow = !flow
            tv1 -> flow
          case Seq(LB(_, tv1, _), UB(_, tv2, _)) if tv1 == tv2 =>
            flow = !flow
            tv1 -> flow
        }.toList ::: end -> false :: Nil // last type locations are always shown from consumption to introduction
    }

    // check if any pair of reasons represents a through flow
    // ignore such reasons because they are an artifact of
    // symmetric bounds constraining
    def throughFlow: Bool = reason match {
      case Nil => false
      case _ :: Nil => false
      case _ => reason.sliding(2).exists {
        case Seq(LB(_, tv1, _), LB(tv2, _, _)) => tv1.unwrapProvs == tv2.unwrapProvs
        case Seq(UB(_, tv1, _), UB(tv2, _, _)) => tv1.unwrapProvs == tv2.unwrapProvs
        case Seq(LB(_, tv1, _), UB(tv2, _, _)) => tv1.unwrapProvs == tv2.unwrapProvs
        case Seq(UB(tv2, _, _), LB(_, tv1, _)) => tv1.unwrapProvs == tv2.unwrapProvs
        case Seq(UB(_, tv1, _), LB(tv2, _, _)) => tv1.unwrapProvs == tv2.unwrapProvs
        case Seq(LB(tv2, _, _), UB(_, tv1, _)) => tv1.unwrapProvs == tv2.unwrapProvs
        case _ => false
      }
    }
  }

  abstract class UnificationReason {
    var nested: Opt[Unification] = N
    val a: ST = this match {
      case LB(st, tv, provs) => st
      case UB(tv, st, provs) => tv
    }
    val b: ST = this match {
      case LB(st, tv, provs) => tv
      case UB(tv, st, provs) => st
    }
    override def toString: Str = this match {
      case LB(st, tv, _) => s"lb(${st.toString} <: ${tv.toString})"
      case UB(tv, st, _) => s"ub(${tv.toString} <: ${st.toString})"
    }

    // indicates which type was unified with which one
    // that is what the next unification should be
    def unifiedWith: ST = this match {
      case LB(st, _, _) => st
      case UB(_, st, _) => st
    }
    // indicates which type was unified with which one
    // that is what the next unification should be
    def unifiedAgainst: ST = this match {
      case LB(_, tv, _) => tv
      case UB(tv, _, _) => tv
    }

    def getProvs: Ls[TP] = this match {
      case LB(_, _, provs) => provs
      case UB(_, _, provs) => provs
    }

    def getCleanProvs: Ls[TP] = this match {
      // first location binds tighter so only use second prov if it's not same as first
      case LB(_, _, provs) => provs match {
        case head :: _ => head :: provs.sliding(2).collect {
          case Seq(TypeProvenance(S(loc1), _, _, _), tp@TypeProvenance(S(loc2), _, _, _)) if loc1 != loc2 => tp
        }.toList
        case _ => provs
      }
      // second location binds tighter
      case UB(_, _, provs) => provs match {
        case ::(head, _) => head :: provs.sliding(2).collect {
          case Seq(TypeProvenance(S(loc1), _, _, _), tp@TypeProvenance(S(loc2), _, _, _)) if loc1 != loc2 => tp
        }.toList
        case Nil => Nil
      }
    }
  }

  final case class LB(st: ST, tv: ST, provs: Ls[TP]) extends UnificationReason
  final case class UB(tv: ST, st: ST, provs: Ls[TP]) extends UnificationReason

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
    var new_unification: HashMap[ST, UnificationReason] = HashMap()
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
