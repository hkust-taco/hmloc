package mlscript

import mlscript.Message.MessageContext
import mlscript.utils._
import mlscript.utils.shorthands._

import scala.collection.mutable.{Set => MutSet}

trait UnificationSolver extends TyperDatatypes {
  self: Typer =>

  implicit val cache: MutSet[(ST, ST)] = MutSet()
  var unifyMode: Bool = false

  // entry point
  def unifyTypes(a: ST, b: ST)(implicit cache: MutSet[(ST, ST)], ctx: Ctx, raise: Raise): Unit = {
    if (unifyMode) {
      // start by traversing bounds
      traverseBounds(a, b, a.typeUseLocations reverse_::: b.typeUseLocations)
    }
  }

  // reason already has reason for a and b to be unified
  // this unification unifies types and create errors if they fail
  def unifyTypes(a: ST, b: ST, reason: Ls[UnificationReason], skipCache: Bool = false)
                (implicit cache: MutSet[(ST, ST)], ctx: Ctx, raise: Raise): Unit =
    trace( s"U ${a} = ${b} because ${reason.mkString(", ")} ${if (skipCache) "skipCache"}") {
    val st1 = a.unwrapProvs
    val st2 = b.unwrapProvs

    def createUnification(desc: Str = ""): Unification = Unification(a, b, reason, desc)
    def createProvs(a: ST, b: ST): Ls[TP] = a.typeUseLocations reverse_::: b.typeUseLocations

    // unification doesn't have an ordering
    if (!skipCache) {
      if (cache((st1, st2)) || cache(st2, st1)) return
      else {
        cache += ((st1, st2))
        cache += ((st2, st1))
      }
    }

    (st1, st2) match {
      case (tr1: TypeRef, tr2: TypeRef) =>
        if (tr1.defn === tr2.defn && tr1.targs.length === tr2.targs.length) {
          tr1.targs.zip(tr2.targs).zipWithIndex.foreach {
            case ((arg1, arg2), i) =>
              traverseBounds(arg1, arg2, createProvs(arg1, arg2), S(createUnification(s"${i} index of type ref ${tr1.defn}")))
          }
        } else {
          reportUnificationError(createUnification())
        }
      case (tup1: TupleType, tup2: TupleType) =>
        if (tup1.fields.length === tup2.fields.length) {
          tup1.fields.map(_._2.ub).zip(tup2.fields.map(_._2.ub)).zipWithIndex.foreach {
            case ((t1, t2), i) =>
              traverseBounds(t1, t2, createProvs(t1, t2), S(createUnification(s"${i} index of tuple")))
          }
        } else {
          reportUnificationError(createUnification())
        }
      case (FunctionType(arg1, res1), FunctionType(arg2, res2)) =>
        traverseBounds(arg2, arg1, createProvs(arg2, arg1), S(createUnification("function argument")))
        traverseBounds(res1, res2, createProvs(res1, res2), S(createUnification("function result")))
      case (tv1: TypeVariable, tv2: TypeVariable) if tv1 === tv2 =>
        () // TODO report error for recursive type
      case (tv: TypeVariable, st) =>
        if (tv.new_unification.nonEmpty) println(s"U   ${st} with")
        tv.new_unification.foreach {
          case ((prev, prevReason)) =>
            println(s"    ${tv} = ${prev} for ${prevReason}")
            unifyTypes(prev, st, prevReason :: reason)
        }
      case (st, tv: TypeVariable) =>
        if (tv.new_unification.nonEmpty) println(s"U   ${st} with")
        tv.new_unification.foreach {
          case ((prev, prevReason)) =>
            println(s"    ${tv} = ${prev} for ${prevReason}")
            unifyTypes(prev, st, prevReason :: reason.reverse)
        }
      case (_, _) =>
        // report error
        reportUnificationError(createUnification())
    }
  }()

  // provs already has provs for a and b
  // this unification traverses bounds to reach a diverging or converging unification
  def traverseBounds(a: ST, b: ST, provs: Ls[TypeProvenance] = Nil, nested: Opt[Unification] = N)
                (implicit cache: MutSet[(ST, ST)], ctx: Ctx, raise: Raise): Unit =
    trace( s"UT ${a} <: ${b} len: ${provs.length} ${nested.mkStringOr("", "nested: ")}") {
    val st1 = a.unwrapProvs
    val st2 = b.unwrapProvs

    // unification doesn't have an ordering
    if (cache((st1, st2)) || cache(st2, st1)) return
    else {
      cache += ((st1, st2))
      cache += ((st2, st1))
    }

    (st1, st2) match {
      case (tv1: TypeVariable, tv2: TypeVariable) if tv1 === tv2 =>
        () // TODO report error for recursive type
      case (tv: TypeVariable, st) =>
        if (tv.lowerBounds.nonEmpty) println(s"UT  ${st} with")
        // lb <: tv <: st pass through tv and maintain constraining relation
        // from constraining lb already has all it's provs upto the type variable
        // so now we have provs for lb ---- tv ---- st in this order
        tv.lowerBounds.foreach(lb => {
          println(s"UT  ${lb} <: ${tv} <: ${st}")
          traverseBounds(lb, b, lb.typeUseLocations reverse_::: provs, nested)
        })

        if (tv.upperBounds.nonEmpty) println(s"UT  ${st} with")
        val ur = UB(tv, b, provs)
        ur.nested = nested
        val reason = ur :: Nil
        // tv <: ub
        // tv <: st
        // stop traversing and unify
        tv.upperBounds.foreach(ub => {
          tv.new_unification.get(ub).foreach { case (prevReason) =>
            println(s"UT  ${tv} <: ${ub}  for ${prevReason}")
            unifyTypes(ub, b, prevReason :: reason)
          }
        })
        println(s"UT  ${tv} += ${(b, reason)}")
        tv.new_unification += ((b, ur))
      case (st, tv: TypeVariable) =>
        if (tv.upperBounds.nonEmpty) println(s"UT  ${a} with")
        // st <: tv <: ub pass through tv and maintain constraining relation
        tv.upperBounds.foreach(ub => {
          println(s"UT  ${a} <: ${tv} <: ${ub}")
          traverseBounds(a, ub, provs ::: ub.typeUseLocations, nested)
        })

        if (tv.lowerBounds.nonEmpty) println(s"UT  ${a} with")
        val ur = LB(a, tv, provs)
        ur.nested = nested
        val reason = ur :: Nil
        // lb <: tv
        // st <: tv
        // stop traversing and unify
        tv.lowerBounds.foreach(lb => {
          tv.new_unification.get(lb).foreach { case (prevReason) =>
            println(s"UT  ${tv} :> ${lb.unwrapProvs} for ${prevReason}")
            unifyTypes(lb, a, prevReason :: reason)
          }
        })

        println(s"UT  ${tv} += ${(a, reason.reverse)}")  // reason is always from tv to st
        tv.new_unification += ((a, ur))
      case _ =>
        val ur = LB(a, b, provs)
        ur.nested = nested
        // skip cache because it hit the traverse bound cache
        // but will actually unified in the next function call
        unifyTypes(a, b, ur :: Nil, true)
    }
  }()

  def showUnificationDebugInfo(): Unit = {
    TypeVariable.createdTypeVars.foreach(tv => {
      println(s"unified ${tv} with:")
      tv.new_unification.foreach { case (st, reason) => println(s"  ${st}: ${reason}") }
    })
  }

  def toSequenceString(u: Unification)(implicit ctx: Ctx): Message = u.unificationChain match {
    case Nil => msg""
    case head :: tail => val first = head match {
      case (LB(st, tv, _), true) => msg"${st.expPos} ---> ${tv.expPos} "
      case (LB(st, tv, _), false) => msg"${tv.expPos} <--- ${st.expPos} "
      case (UB(tv, st, _), false) => msg"${st.expPos} <--- ${tv.expPos} "
      case (UB(tv, st, _), true) => msg"${tv.expPos} ---> ${st.expPos} "
    }

    tail.foldLeft(first) {
      case (prev, (LB(st, tv, _), true)) => prev + msg"---> ${tv.expPos} "
      case (prev, (LB(st, tv, _), false)) => prev + msg"<--- ${st.expPos} "
      case (prev, (UB(tv, st, _), false)) => prev + msg"<--- ${tv.expPos} "
      case (prev, (UB(tv, st, _), true)) => prev + msg"---> ${st.expPos} "
    }
  }

  def collisionErrorMessage(u: Unification)(implicit ctx: Ctx): Ls[Message -> Opt[Loc]] = {
    def makeMessage(st: ST, provs: Ls[TP]): Ls[Message -> Opt[Loc]] = provs.map {
      case TypeProvenance(loco, desc, _, false) => msg"this ${desc} has type `${st.expPos}`" -> loco
      case TypeProvenance(loco, desc, _, true) => msg"`${st.expPos}` comes from this type expression" -> loco
    }

    // respect contravariant type flow
    val (lprov, rprov) = if (u.desc == "function argument") {
      makeMessage(u.b, u.b.uniqueTypeUseLocations.reverse) -> makeMessage(u.a, u.a.uniqueTypeUseLocations)
    } else {
      makeMessage(u.a, u.a.uniqueTypeUseLocations.reverse) -> makeMessage(u.b, u.b.uniqueTypeUseLocations)
    }

    u.reason match {
      case head :: _ => head.nested.fold(lprov ::: rprov)(u => lprov ::: collisionErrorMessage(u) ::: rprov)
      case _ => lprov ::: rprov
    }
  }

  def reportUnificationError(u: Unification)(implicit raise: Raise, ctx: Ctx): Unit = {
    println(s"UERR  ${u.toString}")
    val msgdoesnotmatch = (a: ST, b: ST) => msg"Type `${a.expPos}` does not match `${b.expPos}`"
    u.unificationChain match {
      case Nil => ()
      case _ :: Nil =>
       raise(UnificationReport(msgdoesnotmatch(u.a, u.b) -> N :: collisionErrorMessage(u), false))
      case chain@(head :: next :: _) =>
        val messages = createErrorMessage(head, next, true) ::: chain.sliding(2).collect {
          case Seq(fst, snd) => createErrorMessage(fst, snd)
        }.toList.flatten

        raise(UnificationReport(
          msgdoesnotmatch(u.a, u.b) -> N ::
           msg"       " + toSequenceString(u) -> N :: messages
        , true))
    }
  }

  def createErrorMessage(firstUR: UnificationReason -> Bool, secondUR: UnificationReason -> Bool, showFirst: Bool = false)
                        (implicit ctx: Ctx): Ls[Message -> Opt[Loc]] = {
    val diagistypeof = (st: ST, tp: TP) => msg"this ${tp.desc} has type `${st.expPos}`" -> tp.loco
    val diagishere = (st: ST, tp: TP) => msg"`${st.expPos}` comes from this type expression" -> tp.loco
    val msgistypeof = (st: ST, tp: TP) => if (tp.isType) {
      msg"`${st.expPos}` comes from this type expression"
    } else {
      msg"this ${tp.desc} has type `${st.expPos}`"
    }
    val msgflowitfrom = (st: ST) => msg" and it flows from `${st.expPos}`"
    val msgflowitinto = (st: ST) => msg" and it flows into `${st.expPos}`"
    // take elements from first list upto and include first common element
    // a: 1, 2, 3, 4
    // b: 5, 6, 3, 4
    // result: 1, 2, 3
    def takeUpToFirstCommon(aprovs: Ls[TP], bprovs: Ls[TP]): Ls[TP] = {
      val common = aprovs.toSet.intersect(bprovs.toSet)
      aprovs.span(!common.contains(_)) match {
        // show common location only if it's the first unification
        case ((head, firstCommon :: _)) => head ::: (if (showFirst) firstCommon :: Nil else Nil)
        case ((head, Nil)) => head
      }
    }
    def makeMessagesST(st: ST, ls: Ls[TP]): Ls[Message -> Opt[Loc]] = ls.map{
      case tp@TypeProvenance(_, _, _, false) => diagistypeof(st, tp)
      case tp@TypeProvenance(_, _, _, true) => diagishere(st, tp)
    }
    // showFirst indicates if the merged location belongs to the type variable or the st
    // when showing first we show location of type variable
    // otherwise the location of type variable is skipped as it must have been shown in previous
    // unification reason message. So the directly the location of st is shown
    def makeMessagesUR(ur: UnificationReason -> Bool, provs: Ls[TP]): Ls[Message -> Opt[Loc]] = {
      // first ur should include the last location which is the common location between the two urs
      // so the last message should include the type b as the main type

      // true direction flow (showFirst = true)
      // lb: st 0 -> st 1 -> st into tv 2 -> tv 3
      // ub: tv 0 -> tv 1 -> tv into st 2 -> st 3
      // true direction flow (showFirst = false)
      // lb: st 0 -> st 1 -> st into tv 2 -> tv 3
      // ub: tv 0 -> tv 1 -> tv into st 2 -> st 3
      if (ur._2) {
        val a = ur._1.a
        val b = ur._1.b
        provs.reverse match {
          case last :: Nil => msgistypeof(a, last) + msgflowitinto(b) -> last.loco :: Nil
          case last :: sndLast :: tail => diagistypeof(b, last) :: msgistypeof(a, sndLast) + msgflowitinto(b) -> sndLast.loco :: makeMessagesST(a, tail)
          case _ => ???
        }
      }.reverse
      // false direction flow (showFirst = true)
      // lb: st 1 -> tv from st 2 -> tv 3 -> tv 4 (rev)
      // ub: tv 1 -> st from tv 2 -> st 3 -> st 4 (rev)
      // false direction flow (showFirst = false)
      // lb: st 1 -> tv from st 2 -> tv 3 (rev)
      // ub: tv 1 -> st from tv 2 -> st 3 (rev)
      else {
        val a = ur._1.a
        val b = ur._1.b
        provs match {
          case fst :: Nil => msgistypeof(a, fst) + msgflowitinto(b) -> fst.loco :: Nil
          case fst :: snd :: tail => diagistypeof(a, fst) :: msgistypeof(b, snd) + msgflowitfrom(a) -> snd.loco :: makeMessagesST(b, tail)
          case _ => ???
        }
      }.reverse
    }

    // if a is same that means common elements are on left side
    // other wise b is same and common elements are on right side
    val leftSame = firstUR._1.a == secondUR._1.a
    println(s"${firstUR} -> ${secondUR} showFirst: ${showFirst} leftSame: ${leftSame}")

    val aprovs = firstUR._1.getCleanProvs
    val bprovs = secondUR._1.getCleanProvs

    // first should show all locations
    if (showFirst) {
      val provs = if (leftSame) {
        takeUpToFirstCommon(aprovs.reverse, bprovs.reverse).reverse
      } else {
        takeUpToFirstCommon(aprovs, bprovs)
      }
      println(s"show ${provs.length} locations for ${firstUR}")
      makeMessagesUR(firstUR, provs)
    }
    // second should check previous to see if terminal location is common and not show it again
    else {
      val provs = if (leftSame) {
        takeUpToFirstCommon(bprovs.reverse, aprovs.reverse).reverse
      } else {
        takeUpToFirstCommon(bprovs, aprovs)
      }
      println(s"show ${provs.length} locations for ${secondUR}")
      makeMessagesUR(secondUR, provs)
    }
  }
}
