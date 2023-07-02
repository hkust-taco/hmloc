package mlscript

import mlscript.utils._
import mlscript.utils.shorthands._

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import scala.collection.mutable.{Map => MutMap, Set => MutSet}

/** Inessential methods used to help debugging. */
abstract class TyperHelpers { Typer: Typer =>
  
  protected var constrainCalls = 0
  protected var annoyingCalls = 0
  protected var subtypingCalls = 0
  protected var constructedTypes = 0
  def stats: (Int, Int, Int, Int) =
    (constrainCalls, annoyingCalls, subtypingCalls, constructedTypes)
  def resetStats(): Unit = {
    constrainCalls = 0
    annoyingCalls  = 0
    subtypingCalls = 0
    constructedTypes = 0
  }
  
  private val noPostTrace: Any => String = _ => ""
  
  protected var indent = 0
  def trace[T](pre: => String)(thunk: => T)(post: T => String = noPostTrace): T = {
    println(pre)
    indent += 1
    val res = try thunk finally indent -= 1
    if (post isnt noPostTrace) println(post(res))
    res
  }
  
  def emitDbg(str: String): Unit = scala.Predef.println(str)
  
  // Shadow Predef functions with debugging-flag-enabled ones:
  
  def println(msg: => Any): Unit = if (dbg) emitDbg("| " * indent + msg)
  
  /** A more advanced println version to show where things are printed from. */
  // def println(msg: => Any)(implicit file: sourcecode.FileName, line: sourcecode.Line): Unit =
  //   if (dbg) {
  //     emitDbg((if (showPrintPrefix) {
  //       val prefix = s"[${file.value}:${line.value}]"
  //       prefix + " " * (30 - prefix.length)
  //     } else "") + "| " * indent + msg)
  //   }
  // val showPrintPrefix =
  //   // false
  //   true

  def recordUnion(fs1: Ls[Var -> ST], fs2: Ls[Var -> ST]): Ls[Var -> ST] = {
    val fs2m = fs2.toMap
    fs1.flatMap { case (k, v) => fs2m.get(k).map(v2 => k -> (v | v2)) }
  }

  def subst(ts: PolymorphicType, map: Map[SimpleType, SimpleType]): PolymorphicType =
    PolymorphicType(ts.level, subst(ts.body, map))

  /**
    * Apply a mapping to a simple type. If st is an unconstrained type variable
    * it is returned as is. If it has constrains clone the type variable and
    * apply subst to it's bounds.
    *
    * @param st
    * @param map
    * @param substInMap recursively apply subst to result of mapping if exists
    * @param cache tracks substituted type variables
    * @return
    */
  def subst(st: SimpleType, map: Map[SimpleType, SimpleType], substInMap: Bool = false)
        (implicit cache: MutMap[TypeVariable, SimpleType] = MutMap.empty): SimpleType =
            // trace(s"subst($st)") {
    map.get(st) match {
      case S(res) => if (substInMap) subst(res, map, substInMap) else res
      case N =>
        st match {
          case tv: TypeVariable if tv.lowerBounds.isEmpty && tv.upperBounds.isEmpty =>
            cache += tv -> tv
            tv
          case tv: TypeVariable => cache.getOrElse(tv, {
            val v = freshVar(tv.prov, tv.nameHint)(tv.level)
            cache += tv -> v
            v.lowerBounds = tv.lowerBounds.map(subst(_, map, substInMap))
            v.upperBounds = tv.upperBounds.map(subst(_, map, substInMap))
            v
          })
          case _ => st.map(subst(_, map, substInMap))
        }
    }
    // }(r => s"= $r")
  

  def tupleIntersection(fs1: Ls[Opt[Var] -> ST], fs2: Ls[Opt[Var] -> ST]): Ls[Opt[Var] -> ST] = {
    require(fs1.size === fs2.size)
    (fs1 lazyZip fs2).map {
      case ((S(n1), t1), (S(n2), t2)) if n1 =/= n2 => (N, t1 & t2)
      case ((no1, t1), (no2, t2)) => (no1 orElse no2, t1 & t2)
    }
  }
  def tupleUnion(fs1: Ls[Opt[Var] -> ST], fs2: Ls[Opt[Var] -> ST]): Ls[Opt[Var] -> ST] = {
    require(fs1.size === fs2.size)
    (fs1 lazyZip fs2).map {
      case ((S(n1), t1), (S(n2), t2)) => (Option.when(n1 === n2)(n1), t1 | t2)
      case ((no1, t1), (no2, t2)) => (N, t1 | t2)
    }
  }

  trait SimpleTypeImpl {
    self: SimpleType =>

    implicit val ord: Ordering[ST] = Ordering.by(_.toString)

    def showProvOver(enabled: Bool)(str: Str): Str =
      if (enabled) str + prov.toString
      else str

    // Note: we implement hashCode and equals manually because:
    //  1. On one hand, we want a ProvType to compare equal to its underlying type,
    //      which is necessary for recursive types to associate type provenances to
    //      their recursive uses without making the constraint solver diverge; and
    //  2. Additionally, caching hashCode should have performace benefits
    //      — though I'm not sure whether it's best as a `lazy val` or a `val`.
    override lazy val hashCode: Int = this match {
      case tv: TypeVariable => tv.uid
      case ProvType(und) => und.hashCode
      case p: Product => scala.runtime.ScalaRunTime._hashCode(p)
    }

    override def equals(that: Any): Bool =
    // trace(s"$this == $that") {
      this match {
        case ProvType(und) => (und: Any) === that
        case tv1: TV => that match {
          case tv2: Typer#TV => tv1.uid === tv2.uid
          case ProvType(und) => this === und
          case _ => false
        }
        case p1: Product => that match {
          case that: ST => that match {
            case ProvType(und) => this === und
            case tv: TV => false
            case p2: Product =>
              p1.canEqual(p2) && p1.productArity === p2.productArity && {
                val it1 = p1.productIterator
                val it2 = p2.productIterator
                while (it1.hasNext && it2.hasNext) {
                  if (it1.next() =/= it2.next()) return false
                }
                return !it1.hasNext && !it2.hasNext
              }
          }
          case _ => false
        }
      }
    // }(r => s"= $r")

    def map(f: SimpleType => SimpleType): SimpleType = this match {
      case FunctionType(lhs, rhs) => FunctionType(f(lhs), f(rhs))(prov)
      case RecordType(fields) => RecordType(fields.mapValues(f(_)))(prov)
      case TupleType(fields) => TupleType(fields.mapValues(f(_)))(prov)
      case ComposedType(pol, lhs, rhs) => ComposedType(pol, f(lhs), f(rhs))(prov)
      case ProvType(underlying) => ProvType(f(underlying))(prov)
      case TypeRef(defn, targs) => TypeRef(defn, targs.map(f(_)))(prov)
      case _: TypeVariable | _: RigidTypeVariable | _: ExtrType => this
    }

    def |(that: SimpleType, prov: TypeProvenance = noProv, swapped: Bool = false): SimpleType = (this, that) match {
      case (TopType, _) => TopType
      case (BotType, _) => that

      // These were wrong! During constraint solving it's important to keep them!
      // case (_: RecordType, _: PrimType | _: FunctionType) => TopType
      // case (_: FunctionType, _: PrimType | _: RecordType) => TopType

      case (_: RecordType, _: FunctionType) => TopType
      case (RecordType(fs1), RecordType(fs2)) =>
        RecordType(recordUnion(fs1, fs2))(prov)
      case (t0@TupleType(fs0), t1@TupleType(fs1))
        // If the sizes are different, to merge these we'd have to return
        //  the awkward `t0.toArray & t0.toRecord | t1.toArray & t1.toRecord`
        if fs0.sizeCompare(fs1) === 0 =>
        TupleType(tupleUnion(fs0, fs1))(t0.prov)
      case _ if !swapped => that | (this, prov, swapped = true)
      case (`that`, _) => this
      case _ => ComposedType(true, that, this)(prov)
    }

    def &(that: SimpleType, prov: TypeProvenance = noProv, swapped: Bool = false): SimpleType =
      (this, that) match {
        case (TopType | RecordType(Nil), _) => that
        case (BotType, _) => BotType
        // Unnecessary and can complicate constraint solving quite a lot:
        // case (ComposedType(true, l, r), _) => l & that | r & that
        case (FunctionType(l1, r1), FunctionType(l2, r2)) =>
          FunctionType(l1 | l2, r1 & r2)(prov)
        case (RecordType(fs1), RecordType(fs2)) =>
          RecordType(mergeSortedMap(fs1, fs2)(_ & _).toList)(prov)
        case (t0@TupleType(fs0), t1@TupleType(fs1)) =>
          if (fs0.sizeCompare(fs1) =/= 0) BotType
          else TupleType(tupleIntersection(fs0, fs1))(t0.prov)
        case _ if !swapped => that & (this, prov, swapped = true)
        case (`that`, _) => this
        case _ => ComposedType(false, that, this)(prov)
      }

    def withProv(p: TypeProvenance): ST = mkProxy(this, p)

    def unwrapProvs: SimpleType = this match {
      case ProvType(und) => und.unwrapProvs
      case _ => this
    }

    def children(includeBounds: Bool): List[SimpleType] = this match {
      case tv: TypeVariable => if (includeBounds) tv.uniConcreteTypes.toList else Nil
      case FunctionType(l, r) => l :: r :: Nil
      case ComposedType(_, l, r) => l :: r :: Nil
      case RecordType(fs) => fs.flatMap(f => f._2 :: Nil)
      case TupleType(fs) => fs.flatMap(f => f._2 :: Nil)
      case ExtrType(_) => Nil
      case ProvType(und) => und :: Nil
      case _: RigidTypeVariable => Nil
      case TypeRef(d, ts) => ts
    }

    def getVars: SortedSet[TypeVariable] = {
      val res = MutSet.empty[TypeVariable]

      @tailrec def rec(queue: List[SimpleType]): Unit = queue match {
        case (tv: TypeVariable) :: tys =>
          if (res(tv)) rec(tys)
          else {
            res += tv; rec(tv.children(includeBounds = true) ::: tys)
          }
        case ty :: tys => rec(ty.children(includeBounds = true) ::: tys)
        case Nil => ()
      }

      rec(this :: Nil)
      SortedSet.from(res)(Ordering.by(_.uid))
    }

    def showUnified: String =
      getVars.iterator.filter(tv => tv.uniConcreteTypes.nonEmpty)
        .map(tv => "\n\t\t" + tv.toString + " = " + tv.uniConcreteTypes.mkString(", ")).mkString

    def expOcamlTy()(implicit ctx: Ctx, showTV: Set[TV]): Type = {
      expandUnifiedType(this, stopAtTyVars = true, showTV, true)
    }

    /** List of valid locations a type has been used at by collecting location from provenances.
      * Tightest location last
      */
    def typeUseLocations: Ls[TypeProvenance] = this match {
      case pv: ProvType => pv.prov.loco match {
        case None => pv.underlying.typeUseLocations
        case Some(_) => pv.prov :: pv.underlying.typeUseLocations
      }
      case st => st.prov.loco match {
        case None => Nil
        case Some(_) => st.prov :: Nil
      }
    }

    /** List of valid locations a type has been used. Consecutive duplicate locations are removed.
      * Lost location is where the type is introduced.
      */
    def uniqueTypeUseLocations: Ls[TypeProvenance] = {
      val stUseLocation = this.typeUseLocations
      stUseLocation.headOption.map(head => head :: stUseLocation.sliding(2).collect {
        case Seq(TypeProvenance(loc1, _, _, _), t@TypeProvenance(loc2, _, _, _)) if loc1 =/= loc2 => t
      }.toList).getOrElse(Nil)
    }
  }
}
