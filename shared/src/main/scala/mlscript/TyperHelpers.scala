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
  
  /** Substitutes only at the syntactic level, without updating type variables nor traversing their bounds. */
  def substSyntax(st: SimpleType)(map: PartialFunction[SimpleType, SimpleType]): SimpleType =
    // trace(s"substSyntax $st") {
      map.applyOrElse[ST, ST](st, _.map(substSyntax(_)(map)))
    // }(r => s"=> $r")
  
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
  
  private def cleanupUnion(tys: Ls[ST])(implicit ctx: Ctx): Ls[ST] = {
    var res: Ls[ST] = Nil
    tys.reverseIterator.foreach { ty =>
      if (!res.exists(ty <:< _)) res ::= ty
    }
    res
  }

  def factorizeImpl(cs: Ls[Ls[ST]]): ST = trace(s"factorize? ${cs.map(_.mkString(" & ")).mkString(" | ")}") {
    def rebuild(cs: Ls[Ls[ST]]): ST =
      cs.iterator.map(_.foldLeft(TopType: ST)(_ & _)).foldLeft(BotType: ST)(_ | _)
    if (cs.sizeCompare(1) <= 0) return rebuild(cs)
    val factors = MutMap.empty[Factorizable, Int]
    cs.foreach { c =>
      c.foreach {
        case tv: TV =>
          factors(tv) = factors.getOrElse(tv, 0) + 1
        case _ =>
      }
    }
    println(s"Factors ${factors.mkString(", ")}")
    factors.maxByOption(_._2) match {
      // case S((fact, n)) =>
      case S((fact, n)) if n > 1 =>
        val (factored, rest) =
          cs.partitionMap(c => if (c.contains(fact)) L(c) else R(c))
        println(s"Factor $fact -> ${factored.mkString(", ")}")
        assert(factored.size === n, factored -> n)
        val factoredFactored = fact & factorizeImpl(factored.map(_.filterNot(_ === fact)))
        val restFactored =
          if (factors.sizeCompare(1) > 0 && factors.exists(f => (f._1 isnt fact) && f._2 > 1))
            factorizeImpl(rest)
          else rebuild(rest)
        restFactored | factoredFactored
      case _ => rebuild(cs)
    }
  }(r => s"yes: $r")

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
      case TypeBounds(lb, ub) => TypeBounds(f(lb), f(ub))(prov)
      case FunctionType(lhs, rhs) => FunctionType(f(lhs), f(rhs))(prov)
      case RecordType(fields) => RecordType(fields.mapValues(f(_)))(prov)
      case TupleType(fields) => TupleType(fields.mapValues(f(_)))(prov)
      case ComposedType(pol, lhs, rhs) => ComposedType(pol, f(lhs), f(rhs))(prov)
      case ProvType(underlying) => ProvType(f(underlying))(prov)
      case ProxyType(underlying) => f(underlying) // TODO different?
      case TypeRef(defn, targs) => TypeRef(defn, targs.map(f(_)))(prov)
      case _: TypeVariable | _: ObjectTag | _: ExtrType => this
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
        case (_: ClassTag, _: FunctionType) => BotType
        case (FunctionType(l1, r1), FunctionType(l2, r2)) =>
          FunctionType(l1 | l2, r1 & r2)(prov)
        case (RecordType(fs1), RecordType(fs2)) =>
          RecordType(mergeSortedMap(fs1, fs2)(_ & _).toList)(prov)
        case (t0@TupleType(fs0), t1@TupleType(fs1)) =>
          if (fs0.sizeCompare(fs1) =/= 0) BotType
          else TupleType(tupleIntersection(fs0, fs1))(t0.prov)
        case (TypeBounds(l0, u0), TypeBounds(l1, u1)) =>
          TypeBounds(l0 | l1, u0 & u1)(prov)
        case _ if !swapped => that & (this, prov, swapped = true)
        case (`that`, _) => this
        case _ => ComposedType(false, that, this)(prov)
      }

    def neg(prov: TypeProvenance = noProv, force: Bool = false): SimpleType = this match {
      case ExtrType(b) => ExtrType(!b)(noProv)
      case ComposedType(true, l, r) => l.neg() & r.neg()
      case ComposedType(false, l, r) if force => l.neg() | r.neg()
    }

    def >:<(that: SimpleType)(implicit ctx: Ctx): Bool =
      (this is that) || this <:< that && that <:< this

    // TODO for composed types and negs, should better first normalize the inequation
    def <:<(that: SimpleType)(implicit ctx: Ctx, cache: MutMap[ST -> ST, Bool] = MutMap.empty): Bool = {
      // trace(s"? $this <: $that") {
      // trace(s"? $this <: $that   assuming:  ${
      //     cache.iterator.map(kv => "" + kv._1._1 + (if (kv._2) " <: " else " <? ") + kv._1._2).mkString(" ; ")}") {
      subtypingCalls += 1

      def assume[R](k: MutMap[ST -> ST, Bool] => R): R = k(cache.map(kv => kv._1 -> true))

      (this === that) || ((this, that) match {
        case (RecordType(Nil), _) => TopType <:< that
        case (_, RecordType(Nil)) => this <:< TopType
        case (pt1@ClassTag(id1, ps1), pt2@ClassTag(id2, ps2)) => (id1 === id2) || pt1.parentsST(id2)
        case (TypeBounds(lb, ub), _) => ub <:< that
        case (_, TypeBounds(lb, ub)) => this <:< lb
        case (FunctionType(l1, r1), FunctionType(l2, r2)) => assume { implicit cache =>
          l2 <:< l1 && r1 <:< r2
        }
        case (ComposedType(true, l, r), _) => l <:< that && r <:< that
        case (_, ComposedType(false, l, r)) => this <:< l && this <:< r
        case (ComposedType(false, l, r), _) => l <:< that || r <:< that
        case (_, ComposedType(true, l, r)) => this <:< l || this <:< r
        case (RecordType(fs1), RecordType(fs2)) => assume { implicit cache =>
          fs2.forall(f => fs1.find(_._1 === f._1).exists(_._2 <:< f._2))
        }
        case (_: TypeVariable, _) | (_, _: TypeVariable)
          if cache.contains(this -> that)
        => cache(this -> that)
        case (tv: TypeVariable, _) =>
          cache(this -> that) = false
          val tmp = tv.upperBounds.exists(_ <:< that)
          if (tmp) cache(this -> that) = true
          tmp
        case (_, tv: TypeVariable) =>
          cache(this -> that) = false
          val tmp = tv.lowerBounds.exists(this <:< _)
          if (tmp) cache(this -> that) = true
          tmp
        case (ProxyType(und), _) => und <:< that
        case (_, ProxyType(und)) => this <:< und
        case (_, ExtrType(false)) => true
        case (ExtrType(true), _) => true
        case (_, ExtrType(true)) | (ExtrType(false), _) => false // not sure whether LHS <: Bot (or Top <: RHS)
        case (tr: TypeRef, _)
          if (primitiveTypes contains tr.defn.name) && !tr.defn.name.isCapitalized => tr.expand <:< that
        case (_, tr: TypeRef)
          if (primitiveTypes contains tr.defn.name) && !tr.defn.name.isCapitalized => this <:< tr.expand
        case (tr: TypeRef, _) =>
          ctx.tyDefs.get(tr.defn.name) match {
            case S(td) if td.kind is Cls => clsNameToNomTag(td)(noProv, ctx) <:< that
            case _ => false
          }
        case (_, _: TypeRef) =>
          false // TODO try to expand them (this requires populating the cache because of recursive types)
        case (_: FunctionType, _) | (_, _: FunctionType) => false
        case (_: RecordType, _: ObjectTag) | (_: ObjectTag, _: RecordType) => false
        // case _ => lastWords(s"TODO $this $that ${getClass} ${that.getClass()}")
      })
      // }(r => s"! $r")
    }

    def isTop: Bool = (TopType <:< this) (Ctx.empty)

    def isBot: Bool = (this <:< BotType) (Ctx.empty)

    def unwrapAll(implicit ctx: Ctx): SimpleType = unwrapProxies match {
      case tr: TypeRef => tr.expand.unwrapAll
      case u => u
    }

    def withProv(p: TypeProvenance): ST = mkProxy(this, p)

    def abs(that: SimpleType)(prov: TypeProvenance): SimpleType =
      FunctionType(this, that)(prov)

    def unwrapProxies: SimpleType = this match {
      case ProxyType(und) => und.unwrapProxies
      case _ => this
    }

    def unwrapProvs: SimpleType = this match {
      case ProvType(und) => und.unwrapProvs
      case _ => this
    }

    def components(union: Bool): Ls[ST] = this match {
      case ExtrType(`union`) => Nil
      case ComposedType(`union`, l, r) => l.components(union) ::: r.components(union)
      case ProvType(und) => und.components(union)
      case _ => this :: Nil
    }

    def children(includeBounds: Bool): List[SimpleType] = this match {
      case tv: TypeVariable => if (includeBounds) tv.uniConcreteTypes.toList else Nil
      case FunctionType(l, r) => l :: r :: Nil
      case ComposedType(_, l, r) => l :: r :: Nil
      case RecordType(fs) => fs.flatMap(f => f._2 :: Nil)
      case TupleType(fs) => fs.flatMap(f => f._2 :: Nil)
      case ExtrType(_) => Nil
      case ProxyType(und) => und :: Nil
      case _: ObjectTag => Nil
      case TypeRef(d, ts) => ts
      case TypeBounds(lb, ub) => lb :: ub :: Nil
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

    def showBounds: String =
      getVars.iterator.filter(tv => (tv.upperBounds ++ tv.lowerBounds).nonEmpty).map(tv =>
        "\n\t\t" + tv.toString
          + (if (tv.lowerBounds.isEmpty) "" else " :> " + tv.lowerBounds.mkString(" | "))
          + (if (tv.upperBounds.isEmpty) "" else " <: " + tv.upperBounds.mkString(" & "))
      ).mkString

    def showUnified: String =
      getVars.iterator.filter(tv => tv.uniConcreteTypes.nonEmpty)
        .map(tv => "\n\t\t" + tv.toString + " = " + tv.uniConcreteTypes.mkString(", ")).mkString

    def exp(pol: Opt[Bool], ty: ST)(implicit ctx: Ctx): Type = (
      ty
        // |> (_.normalize(false))
        // |> (simplifyType(_, pol, removePolarVars = false, inlineBounds = false))
        // |> (shallowCopy)
        // |> (subst(_, Map.empty)) // * Make a copy of the type and its TV graph – although we won't show the TV bounds, we still care about the bounds as they affect class type reconstruction in normalizeTypes_!
        // |> (normalizeTypes_!(_, pol)(ctx))
        |> (expandType(_, stopAtTyVars = true))
      )

    def expOcamlTy()(implicit ctx: Ctx, showTV: Set[TV]): Type = {
      expandType(this, stopAtTyVars = true, showTV, true)
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

  def shallowCopy(st: ST)(implicit cache: MutMap[TV, TV] = MutMap.empty): ST = st match {
    case tv: TV => cache.getOrElseUpdate(tv, freshVar(tv.prov, tv.nameHint, Nil, Nil)(tv.level))
    case _ => st.map(shallowCopy)
  }
  
  def merge(pol: Bool, ts: Ls[ST]): ST =
    if (pol) ts.foldLeft(BotType: ST)(_ | _)
    else ts.foldLeft(TopType: ST)(_ & _)
  
  
  class Traverser(implicit ctx: Ctx) {
    def apply(pol: Opt[Bool])(st: ST): Unit = st match {
      case tv: TypeVariable =>
        if (pol =/= S(false)) tv.lowerBounds.foreach(apply(S(true)))
        if (pol =/= S(true)) tv.upperBounds.foreach(apply(S(false)))
      case FunctionType(l, r) => apply(pol.map(!_))(l); apply(pol)(r)
      case ComposedType(_, l, r) => apply(pol)(l); apply(pol)(r)
      case RecordType(fs) => fs.unzip._2.foreach(applyField(pol))
      case TupleType(fs) => fs.unzip._2.foreach(applyField(pol))
      case ExtrType(_) => ()
      case ProxyType(und) => apply(pol)(und)
      case _: ObjectTag => ()
      case tr: TypeRef => tr.mapTargs(pol)(apply(_)(_)); ()
      case TypeBounds(lb, ub) => apply(S(false))(lb); apply(S(true))(ub)
    }
    def applyField(pol: Opt[Bool])(fld: ST): Unit = apply(pol)(fld)
  }
}
