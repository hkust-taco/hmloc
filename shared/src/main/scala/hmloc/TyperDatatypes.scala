package hmloc

import hmloc.utils._
import hmloc.utils.shorthands._
import sourcecode._

abstract class TyperDatatypes extends TyperHelpers { self: Typer =>

  type TN = TypeName
  type TP = TypeProvenance

  case class TypeProvenance(loco: Opt[Loc], desc: Str, originName: Opt[Str] = N, isType: Bool = false)(implicit val file: FileName, val line: Line) {
    val isOrigin: Bool = originName.isDefined
    override def toString: Str = (if (isOrigin) "o: " else "") + "‹"+loco.fold(desc)(desc+":"+_)+s"›[${file.value}:${line.value}] src: ${loco.fold("NA")(_.showLocationInSource)}"
  }

  case class VarSymbol(ty: TypeScheme, definingVar: Var)

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

  case class FunctionType(lhs: SimpleType, rhs: SimpleType)(val prov: TypeProvenance) extends SimpleType {
    lazy val level: Int = lhs.level max rhs.level
    override def toString = s"($lhs -> $rhs)"
  }

  case class TupleType(fields: List[Opt[Var] -> ST])(val prov: TypeProvenance) extends SimpleType {
    lazy val level: Int = fields.iterator.map(_._2.level).maxOption.getOrElse(0)
    override def toString =
      s"(${fields.map(f => s"${f._1.fold("")(_.name+": ")}${f._2},").mkString(" ")})"
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

  /** The sole purpose of ProvType is to store additional type provenance info. */
  case class ProvType(underlying: SimpleType)(val prov: TypeProvenance) extends SimpleType {
    override def toString = s"[$underlying]"
    def level: Int = underlying.level
  }

  object ProvType {
    def unapply(prov: ProvType): S[ST] = S(prov.underlying)
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
      // substitute the arguments of type def
      // with the arguments given to the type ref
      subst(td.bodyTy, td.targs.lazyZip(targs).toMap)
    }

    def mapTargs[R](pol: Opt[Bool])(f: (Opt[Bool], ST) => R)(implicit ctx: Ctx): Ls[R] = {
      val td = ctx.tyDefs(defn.name)
      td.tvarVariances.fold(targs.map(f(N, _))) { tvv =>
        assert(td.tparamsargs.sizeCompare(targs) === 0)
        (td.tparamsargs lazyZip targs).map { case ((_, tv), ta) =>
          tvv(tv) match {
            // Note: This should return a TypeBound(BotType, TopType) when using constrained types
            case VarianceInfo(true, true) =>
              f(N, TopType)
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

  case class RigidTypeVariable(id: SimpleTerm)(val prov: TypeProvenance) extends SimpleType {
    def compare(that: RigidTypeVariable): Int = this.id compare that.id
    def level: Int = 0
    override def toString = id.idStr
  }

  /** A type variable living at a certain polymorphism level `level`, with mutable bounds.
    * Invariant: Types appearing in the bounds never have a level higher than this variable's `level`. */
  final class TypeVariable(
      var level: Int,
      var lowerBounds: List[SimpleType],
      var upperBounds: List[SimpleType],
      val nameHint: Opt[Str] = N
  )(val prov: TypeProvenance) extends SimpleType with Ordered[TypeVariable] {
    private[hmloc] val uid: Int = { freshCount += 1; freshCount - 1 }
    lazy val asTypeVar = new TypeVar(L(uid), nameHint)
    var uni: Ls[Unification] = Ls()
    var uniConcreteTypes: Set[ST] = Set.empty

    def unifiedWith: Ls[ST] = uni.map(u => if (u.a.unwrapProvs == this) u.b.unwrapProvs else u.a.unwrapProvs)
    def compare(that: TV): Int = this.uid compare that.uid
    override def toString: String = showProvOver(false)(nameHint.getOrElse("α") + uid + "'" * level)
  }
  type TV = TypeVariable
  private var freshCount = 0
  def freshVar(p: TypeProvenance, nameHint: Opt[Str] = N, lbs: Ls[ST] = Nil, ubs: Ls[ST] = Nil)
        (implicit lvl: Int): TypeVariable = new TypeVariable(lvl, lbs, ubs, nameHint)(p)
}
