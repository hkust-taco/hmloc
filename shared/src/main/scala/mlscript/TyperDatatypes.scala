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

  case class RecordType(fields: List[(Var, ST)])(val prov: TypeProvenance) extends SimpleType {
    // TODO: assert no repeated fields
    lazy val level: Int = fields.iterator.map(_._2.level).maxOption.getOrElse(0)
    def sorted: RecordType = RecordType(fields.sortBy(_._1))(prov)
    override def toString = s"{${fields.map(f => s"${f._1}: ${f._2}").mkString(", ")}}"
  }
  object RecordType {
    def empty: RecordType = RecordType(Nil)(noProv)
    def mk(fields: List[(Var, ST)])(prov: TypeProvenance = noProv): SimpleType =
      if (fields.isEmpty) ExtrType(false)(prov) else RecordType(fields)(prov)
  }

  sealed abstract class ArrayBase extends MiscBaseType {
    def inner: ST
  }

  case class ArrayType(val inner: ST)(val prov: TypeProvenance) extends ArrayBase {
    def level: Int = inner.level
    override def toString = s"Array‹$inner›"
  }

  case class TupleType(fields: List[Opt[Var] -> ST])(val prov: TypeProvenance) extends ArrayBase {
    lazy val inner: ST = fields.map(_._2).reduceLeftOption(_ | _).getOrElse(BotType)
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
            tparamField(defn, tp) -> tv
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
    var uni: Ls[Unification] = Ls()
    def compare(that: TV): Int = this.uid compare that.uid
    def isRecursive_$(implicit ctx: Ctx) : Bool = (lbRecOccs_$, ubRecOccs_$) match {
      case (S(N | S(true)), _) | (_, S(N | S(false))) => true
      case _ => false
    }
    /** None: not recursive in this bound; Some(Some(pol)): polarly-recursive; Some(None): nonpolarly-recursive.
      * Note that if we have something like 'a :> Bot <: 'a -> Top, 'a is not truly recursive
      *   and its bounds can actually be inlined. */
    private final def lbRecOccs_$(implicit ctx: Ctx): Opt[Opt[Bool]] =
      TupleType(lowerBounds.map(N -> _))(noProv).getVarsPol(S(true)).get(this)
    private final def ubRecOccs_$(implicit ctx: Ctx): Opt[Opt[Bool]] =
      TupleType(upperBounds.map(N -> _))(noProv).getVarsPol(S(false)).get(this)

    override def toString: String = showProvOver(false)(nameHint.getOrElse("α") + uid + "'" * level)
  }
  type TV = TypeVariable
  private var freshCount = 0
  def freshVar(p: TypeProvenance, nameHint: Opt[Str] = N, lbs: Ls[ST] = Nil, ubs: Ls[ST] = Nil)
        (implicit lvl: Int): TypeVariable = new TypeVariable(lvl, lbs, ubs, nameHint)(p)
  def resetState(): Unit = {
    freshCount = 0
  }
  trait CompactTypeOrVariable
  type PolarVariable = (TypeVariable, Boolean)
  object TypeVariable {
    var collectTypeVars: Bool = false
  }
}
