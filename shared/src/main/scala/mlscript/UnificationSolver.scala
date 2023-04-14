package mlscript

import mlscript.Message.MessageContext
import mlscript.utils._
import mlscript.utils.shorthands._

import scala.collection.mutable.{Set => MutSet, Map => MutMap, Queue => MutQueue}

trait UnificationSolver extends TyperDatatypes {
  self: Typer =>

  val errorCache: MutMap[(ST, ST), Unification] = MutMap()
  val cache: MutSet[(ST, ST)] = MutSet()
  var unifyMode: Bool = false

  // Unify all type variables crated by accessing them from the hook
  def unifyTypes()(implicit ctx: Ctx, raise: Raise): Unit = {
    errorCache.clear()
    cache.clear()

    // register all bounds as unifications
    TypeVariable.createdTypeVars.foreach(tv => {
      println(s"$tv is ${tv.prov.desc} and it's bounds are")
      tv.lowerBounds.foreach(lb => {
        val reason = LB(lb, tv, lb.typeUseLocations.reverse)
        println(s"U $tv <: $lb with ${reason.provs.length} provs")
        tv.new_unification += ((lb, reason))
        println(s"U ${tv} += ${(lb, reason)}")
      })
      tv.upperBounds.foreach(ub => {
        val reason = UB(tv, ub, ub.typeUseLocations)
        println(s"U $tv :> $ub with ${reason.provs.length} provs")
        tv.new_unification += ((ub, reason))
        println(s"U ${tv} += ${(ub, reason)}")
      })
    })

    TypeVariable.createdTypeVars.foreach(tv => {
      for {
        (tv1, ur1) <- tv.new_unification
        (tv2, ur2) <- tv.new_unification
        if tv1 != tv2
      } {
        unifyTypes(tv1, tv2, ur1 :: ur2 :: Nil, MutSet())
      }
    })

    errorCache.values
      .toSortedSet(Ordering.by(u => u.a.toString ++ u.b.toString))
        .foreach(reportUnificationError)
    errorCache.clear()
    cache.clear()
  }

  // reason already has reason for a and b to be unified
  // this unification unifies types and create errors if they fail
  def unifyTypes(a: ST, b: ST, reason: Ls[UnificationReason], cache: MutSet[(ST, ST)])
                (implicit ctx: Ctx, raise: Raise): Unit =
    trace(s"U ${a} = ${b} because ${reason.mkString(", ")}") {
      val st1 = a.unwrapProvs
      val st2 = b.unwrapProvs

      def createUnification(desc: Str = ""): Unification = Unification(a, b, reason, desc)

      // check if unification reason as through flow
      // however allow through flow of length 2 because
      // it can be a collision error
      if (reason.length > 2 && createUnification().throughFlow) return

      // unification doesn't have an ordering
      if (cache((st1, st2)) || cache(st2, st1)) {
        println(s"U Cached ${st1} = ${st2}")
        return
      }

      cache += ((a, b))
      cache += ((b, a))

      (st1, st2) match {
        case (tr1: TypeRef, tr2: TypeRef)
          if tr1.defn === tr2.defn &&
            tr1.targs.length === tr2.targs.length &&
            tr1.targs.zip(tr2.targs).forall {
              case (arg1: TypeVariable, arg2) => true
              case (arg1, arg2: TypeVariable) => true
              case (arg1, arg2) => arg1 === arg2
            }
        => ()
        case (tup1: TupleType, tup2: TupleType)
          if tup1.fields.length === tup2.fields.length &&
            tup1.fields.zip(tup2.fields).forall {
              case (N -> FieldType(N, fld1: TypeVariable), _) => true
              case (_, N -> FieldType(N, fld2: TypeVariable)) => true
              case (fld1, fld2) => fld1 === fld2
            }
        => ()
        // cannot check functional equality
        case (FunctionType(arg1, res1), FunctionType(arg2, res2)) => ()
        case (tv1: TypeVariable, tv2: TypeVariable) if tv1 === tv2 =>
          () // TODO report error for recursive type
        case (tv: TypeVariable, st) =>
          if (tv.new_unification.nonEmpty) println(s"U   ${st} with")
          tv.new_unification.foreach {
            case ((prev, prevReason)) =>
              println(s"    ${tv} = ${prev} for ${prevReason}")
              unifyTypes(prev, st, prevReason :: reason, cache)
          }
        case (st, tv: TypeVariable) =>
          if (tv.new_unification.nonEmpty) println(s"U   ${st} with")
          tv.new_unification.foreach {
            case ((prev, prevReason)) =>
              println(s"    ${tv} = ${prev} for ${prevReason}")
              unifyTypes(prev, st, prevReason :: reason.reverse, cache)
          }
        case (_, _) =>
          // report error
          registerUnificationError(createUnification())
      }
    }()

  def showUnificationDebugInfo(): Unit = {
    TypeVariable.createdTypeVars.foreach(tv => {
      println(s"unified ${tv} with:")
      tv.new_unification.foreach { case (st, reason) => println(s"  ${st}: ${reason}") }
    })
  }

  def reportUnificationDebugInfo()(implicit ctx: Ctx, raise: Raise): Unit = {
    TypeVariable.createdTypeVars.foreach(tv => {
      tv.new_unification.foreach { case (st, reason) =>
        val head = msg"${tv.expPos} is unified with ${st.expPos} because ${reason.toString})" -> N
        val locs = reason.getProvs.map {
          case nested: NestedTypeProvenance => msg"<nested> ${nested.nestingInfo.toString} len: ${nested.chain.length.toString}" -> N
          case tp => msg"${tp.desc}" -> tp.loco
        }
        raise(WarningReport(head :: locs))
      }
    })
  }

  def toSequenceString(u: Unification)(implicit ctx: Ctx, showTV: Set[TV]): Message = u.unificationChain match {
    case Nil => msg""
    case head :: tail => val first = head match {
      case (LB(st, tv, _), true) => msg"(${st.expOcamlTy()}) ---> (${tv.expOcamlTy()}) "
      case (LB(st, tv, _), false) => msg"(${tv.expOcamlTy()}) <--- (${st.expOcamlTy()}) "
      case (UB(tv, st, _), false) => msg"(${st.expOcamlTy()}) <--- (${tv.expOcamlTy()}) "
      case (UB(tv, st, _), true) => msg"(${tv.expOcamlTy()}) ---> (${st.expOcamlTy()}) "
    }

      tail.foldLeft(first) {
        case (prev, (LB(st, tv, _), true)) => prev + msg"---> (${tv.expOcamlTy()}) "
        case (prev, (LB(st, tv, _), false)) => prev + msg"<--- (${st.expOcamlTy()}) "
        case (prev, (UB(tv, st, _), false)) => prev + msg"<--- (${tv.expOcamlTy()}) "
        case (prev, (UB(tv, st, _), true)) => prev + msg"---> (${st.expOcamlTy()}) "
      }
  }

  def collisionErrorMessage(u: Unification)(implicit ctx: Ctx): Ls[Message -> Opt[Loc]] = {
    implicit val showTV: Set[TV] = Set()

    val firstthismsg = (st: ST, prov: TP) => prov match {
      case TypeProvenance(loco, desc, _, false) => msg"this ${desc} has type `${st.expOcamlTy()}`" -> loco
      case TypeProvenance(loco, _, _, true) => msg"`${st.expOcamlTy()}` comes from this type expression" -> loco
    }
    val firstsomsg = (st: ST, prov: TP) => prov match {
      case TypeProvenance(loco, desc, _, false) => msg"so this ${desc} has type `${st.expOcamlTy()}`" -> loco
      case TypeProvenance(loco, _, _, true) => msg"`${st.expOcamlTy()}` comes from this type expression" -> loco
    }
    val sndbutmsg = (st: ST, prov: TP) => prov match {
      case TypeProvenance(loco, desc, _, false) => msg"but this ${desc} has type `${st.expOcamlTy()}`" -> loco
      case TypeProvenance(loco, _, _, true) => msg"but `${st.expOcamlTy()}` comes from this type expression" -> loco
    }
    val sndbecausemsg = (st: ST, prov: TP) => prov match {
      case TypeProvenance(loco, desc, _, false) => msg"because this ${desc} has type `${st.expOcamlTy()}`" -> loco
      case TypeProvenance(loco, _, _, true) => msg"because `${st.expOcamlTy()}` comes from this type expression" -> loco
    }
    val commonmsg = (a: ST, b: ST, prov: TP, firstLoc: Bool) => {
      val prefix = if (firstLoc) "" else "so "
      prov match {
        case TypeProvenance(loco, desc, _, false) => msg"${prefix}this ${desc} has type `${a.expOcamlTy()}`. However it flows into `${b.expOcamlTy()}`" -> loco
        case TypeProvenance(loco, _, _, true) => msg"`${a.expOcamlTy()}` comes from this type expression. However it flows into `${b.expOcamlTy()}`" -> loco
      }
    }

    def makeMessageFirstType(st: ST, provs: Ls[TP]) = firstthismsg(st, provs.head) :: provs.tail.map(firstsomsg(st, _))
    def makeMessageSecondType(st: ST, provs: Ls[TP]) = provs.map(sndbecausemsg(st, _))

    // TODO respect contravariant type flow
    val (lprov, rprov) = (u.a.uniqueTypeUseLocations, u.b.uniqueTypeUseLocations)

    // take elements from second list only including the last common element
    // a: 3, 4, 2, 1
    // b: 3, 4, 5, 6
    // result: 4, 5, 6
    val common = lprov.toSet.intersect(rprov.toSet)

    // for a: 3, 4, 2, 1
    // show 2, 1
    val fstmsg = lprov.span(common) match {
      case (Nil, provs) => makeMessageFirstType(u.a, provs.reverse)
      case (_ :+ last, Nil) => commonmsg(u.a, u.b, last, true) :: Nil
      case (_ :+ last, provs) => makeMessageFirstType(u.a, provs.reverse) ::: commonmsg(u.a, u.b, last, false) :: Nil
    }

    val sndmsg = rprov.span(common) match {
      case (Nil, provs) => msg"TODO No common prov" -> N :: makeMessageSecondType(u.b, provs)
      case (_, provs) => makeMessageSecondType(u.b, provs)
    }

    fstmsg ::: sndmsg
  }

  def reportUnificationError(u: Unification)(implicit raise: Raise, ctx: Ctx): Unit = {
    println(s"UERR REPORT ${u.toString}")
    implicit val showTV: Set[TV] = u.unificationSequenceTVars
    val msgdoesnotmatch = (a: ST, b: ST) => msg"Type `${a.expOcamlTy()(ctx, Set())}` does not match `${b.expOcamlTy()(ctx, Set())}`"
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

  def registerNewUnificationError(u: NewUnification): Unit = {
    println(s"UERR $u")
    newErrorCache += u
  }

  def registerUnificationError(u: Unification): Unit = {
    val (a, b, level, length) = (u.a, u.b, u.unificationLevel, u.reason.length)

    errorCache.get((a, b)).fold {
      println(s"UERR CACHE [$level] [$length] ${u.toString}")
      errorCache += (((a, b), u))
      errorCache += (((b, a), u))
      ()
    } {
      case cachedU if level < cachedU.unificationLevel =>
        errorCache += (((a, b), u))
        errorCache += (((b, a), u))
        println(s"UERR UPDATE [$level] ${u.toString}")
      case cachedU if level == cachedU.unificationLevel && length < cachedU.reason.length =>
        errorCache += (((a, b), u))
        errorCache += (((b, a), u))
        println(s"UERR UPDATE [$level] [$length] ${u.toString}")
      case _ =>
        println(s"UERR IGNORE ${u.toString}")
    }
  }

  def createErrorMessage(firstUR: UnificationReason -> Bool, secondUR: UnificationReason -> Bool, showFirst: Bool = false)
                        (implicit ctx: Ctx, showTV: Set[TV]): Ls[Message -> Opt[Loc]] = {
    val diagistypeof = (st: ST, tp: TP, marker: Bool) => if (marker) {
      st match {
        case tv: TV => msg"(${tv.expOcamlTy()}) is assumed as the type of this ${tp.desc}" -> tp.loco
        case _ => msg"(${st.expOcamlTy()}) is assumed as the type of this ${tp.desc}" -> tp.loco
      }
      } else {
        msg"so this ${tp.desc} has type `${st.expOcamlTy()}`" -> tp.loco
      }
    val diagishere = (st: ST, tp: TP) => msg"(${st.expOcamlTy()}) comes from this type expression" -> tp.loco
    val msgistypeof = (st: ST, tp: TP, marker: Bool) => if (tp.isType) {
      msg"(${st.expOcamlTy()}) comes from this type expression"
    } else {
      if (marker) {
        st match {
          case tv: TV => msg"(${tv.expOcamlTy()}) is the assumed type of this ${tp.desc}"
          case _ => msg"(${st.expOcamlTy()}) is the type of this ${tp.desc}"
        }
      } else {
        msg"so this ${tp.desc} has type `${st.expOcamlTy()}`"
      }
    }
    val msgflowintoit = (a: ST, b: ST) => msg". However `${a.expOcamlTy()}` flows into `${b.expOcamlTy()}`"
    val msgitflowinto = (st: ST) => msg" and it flows into `${st.expOcamlTy()}`"

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

    def makeMessagesST(st: ST, ls: Ls[TP]): Ls[Message -> Opt[Loc]] =
      ls.zipWithIndex.map {
        case (tp@TypeProvenance(_, _, _, false), i) => diagistypeof(st, tp, showFirst && i == ls.length - 1)
        case (tp@TypeProvenance(_, _, _, true), i) => diagishere(st, tp)
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
        val a = ur._1.a.unwrapProvs
        val b = ur._1.b.unwrapProvs
        provs.reverse match {
          case last :: Nil => msgistypeof(a, last, true) + msgitflowinto(b) -> last.loco :: Nil
          case last :: sndLast :: Nil => diagistypeof(b, last, true) :: msgistypeof(a, sndLast, showFirst) + msgitflowinto(b) -> sndLast.loco :: Nil
          case last :: sndLast :: tail => diagistypeof(b, last, true) :: msgistypeof(a, sndLast, false) + msgitflowinto(b) -> sndLast.loco :: makeMessagesST(a, tail)
          case Nil => msg"TODO: Exception empty list for ${ur.toString()}" -> N :: Nil
        }
      }.reverse
      // false direction flow (showFirst = true)
      // lb: st 1 -> tv from st 2 -> tv 3 -> tv 4 (rev)
      // ub: tv 1 -> st from tv 2 -> st 3 -> st 4 (rev)
      // false direction flow (showFirst = false)
      // lb: st 1 -> tv however into st 2 -> tv 3 (rev)
      // ub: tv 1 -> st however into tv 2 -> st 3 (rev)
      else {
        val a = ur._1.a.unwrapProvs
        val b = ur._1.b.unwrapProvs
        provs match {
          case fst :: Nil => msgistypeof(a, fst, true) + msgitflowinto(b) -> fst.loco :: Nil
          case fst :: snd :: Nil => diagistypeof(a, fst, true) :: msgistypeof(b, snd, showFirst) + msgflowintoit(a, b) -> snd.loco :: Nil
          case fst :: snd :: tail => diagistypeof(a, fst, true) :: msgistypeof(b, snd, false) + msgflowintoit(a, b) -> snd.loco :: makeMessagesST(b, tail)
          case Nil => msg"Exception empty list for ${ur.toString()}" -> N :: Nil
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

  class UnificationState (
    cache: MutSet[(ST, ST, Int)] = MutSet(),
    val queue: MutQueue[NewUnification] = MutQueue(),
    val error: MutSet[NewUnification] = MutSet(),
    // max queue length, total unifications solved
  ) extends Iterator[NewUnification] {
    var stats: (Int, Int) = (0, 0)
    def enqueueUnification(u: NewUnification): Unit = if (!cached(u)) {
      // TODO: fix this so that recursion can be stopped
      if (u.level >= 4) {
        println(s"  | U X $u.a ~ $u.b")
        return
      }
      println(s"  | U Q $u.a ~ $u.b")
      queue += u
      cache(u)
      stats = (math.max(stats._1, queue.length), stats._2 + 1)
    }
    def cached(u: NewUnification): Bool =
      cache((u.a.unwrapProvs, u.b.unwrapProvs, u.level)) || cache((u.b.unwrapProvs, u.a.unwrapProvs, u.level))
    def cache(u: NewUnification): Unit = cache += ((u.a, u.b, u.level))
    def addError(u: NewUnification): Unit = {
      println(s"UERR $u")
      error += u
    }
    def clear(): Unit = {
      cache.clear()
      queue.clear()
      error.clear()
      stats = (0, 0)
    }

    def reportStats: Str = s"U max: ${stats._1}, total: ${stats._2}"
    override def hasNext: Bool = queue.nonEmpty
    override def next(): NewUnification = queue.dequeue()

    def unify(): Unit = foreach { case u@NewUnification(a, b, flow, level) =>
      println(s"U $u")
      val st1 = a.unwrapProvs
      val st2 = b.unwrapProvs

      (st1, st2) match {
        case (tr1: TypeRef, tr2: TypeRef) if tr1.defn === tr2.defn && tr1.targs.length === tr2.targs.length =>
          tr1.targs.zip(tr2.targs).foreach { case (arg1, arg2) =>
            enqueueUnification(NewUnification(arg1, arg2, Constructor(arg1, arg2, tr1, tr2, flow), level + 1))
          }
        case (_: TypeRef, _: TypeRef) => addError(u)
        case (tup1: TupleType, tup2: TupleType) if tup1.fields.length === tup2.fields.length =>
            tup1.fields.map(_._2.ub).zip(tup2.fields.map(_._2.ub)).foreach {
              case (arg1, arg2) => enqueueUnification(NewUnification(arg1, arg2, Constructor(arg1, arg2, tup1, tup2, flow), level + 1))
            }
        case (_: TupleType, _: TupleType) => addError(u)
        case (FunctionType(arg1, res1), FunctionType(arg2, res2)) =>
          enqueueUnification(NewUnification(arg1, arg2, Constructor(arg1, arg2, st1, st2, flow), level + 1))
          enqueueUnification(NewUnification(res1, res2, Constructor(res1, res2, st1, st2, flow), level + 1))
        case (_: FunctionType, _: FunctionType) => addError(u)
        case (tv1: TypeVariable, tv2: TypeVariable) if tv1 === tv2 =>
          // because bounds are added to both sides any a1 <: a2 and a2 :> a1
          // all type variables will become equal to each other
          // TODO: find a way to not catch those or catch recursive types differently
          // registerUnificationError(u)
          ()
        case (tv: TypeVariable, st) =>
          tv.lbs.foreach(c => enqueueUnification(NewUnification(c.a, st, flow.addLeft(c), level)))
          tv.ubs.foreach(c => enqueueUnification(NewUnification(c.b, st, flow.addLeft(c), level)))
        case (st, tv: TypeVariable) =>
          tv.lbs.foreach(c => enqueueUnification(NewUnification(st, c.a, flow.addRight(c), level)))
          tv.ubs.foreach(c => enqueueUnification(NewUnification(st, c.b, flow.addRight(c), level)))
        case (st1, st2) if st1 != st2 => addError(u)
        case _ => ()
      }
    }
  }

  val newErrorCache: MutSet[NewUnification] = MutSet()
  val uniState = new UnificationState()

  // Unify all type variables crated by accessing them from the hook
  def newUnifyTypes()(implicit ctx: Ctx, raise: Raise): Unit = {
    errorCache.clear()
    cache.clear()
    uniState.clear()

    // register all bounds as unifications
    TypeVariable.createdTypeVars.foreach(tv => {
      tv.lowerBounds.foreach(tv.lbs ::= Constraint(_, tv))
      tv.upperBounds.foreach(tv.ubs ::= Constraint(tv, _))
    })

    TypeVariable.createdTypeVars.foreach(tv => {
      tv.lbs.foreach{ case c@Constraint(a, b) => uniState.enqueueUnification(NewUnification(a, b, c))}
      tv.ubs.foreach{ case c@Constraint(a, b) => uniState.enqueueUnification(NewUnification(a, b, c))}
    })

    uniState.unify()

    errorCache.values
      .toSortedSet(Ordering.by(u => u.a.toString ++ u.b.toString))
      .foreach(reportUnificationError)
    errorCache.clear()
    cache.clear()
  }

  def newUnifyTypes(u: NewUnification, cache: MutSet[(ST, ST)]): Unit =
    trace(s"U $u") {
      val (a, b, flow, level) = NewUnification.unapply(u).get

      val st1 = a.unwrapProvs
      val st2 = b.unwrapProvs

      // unification doesn't have an ordering
      /** Cache unified types and their nesting level. Nesting level is needed because
        * the type arguments are directly constrained but we also want to unify them
        * through constructor data flow. So we need to consider the level in cache.
        */
      if (cache((st1, st2)) || cache(st2, st1)) {
        println(s"U Cached $st1 = $st2")
        return
      }

      cache += ((a, b))
      cache += ((b, a))

      (st1, st2) match {
        case (tr1: TypeRef, tr2: TypeRef) if tr1.defn === tr2.defn && tr1.targs.length === tr2.targs.length =>
          tr1.targs.zip(tr2.targs).foreach { case (arg1, arg2) =>
            newUnifyTypes(NewUnification(arg1, arg2, Constructor(arg1, arg2, tr1, tr2, flow), level + 1), cache)
          }
        case (_: TypeRef, _: TypeRef) => registerNewUnificationError(u)
        case (tup1: TupleType, tup2: TupleType) if tup1.fields.length === tup2.fields.length =>
            tup1.fields.map(_._2.ub).zip(tup2.fields.map(_._2.ub)).foreach {
              case (arg1, arg2) => newUnifyTypes(NewUnification(arg1, arg2, Constructor(arg1, arg2, tup1, tup2, flow), level + 1), cache)
            }
        case (_: TupleType, _: TupleType) => registerNewUnificationError(u)
        case (FunctionType(arg1, res1), FunctionType(arg2, res2)) =>
          newUnifyTypes(NewUnification(arg2, arg1, Constructor(arg2, arg1, st2, st1, flow), level + 1), cache)
          newUnifyTypes(NewUnification(res1, res2, Constructor(res1, res2, st1, st2, flow), level + 1), cache)
        case (_: FunctionType, _: FunctionType) => registerNewUnificationError(u)
        case (tv1: TypeVariable, tv2: TypeVariable) if tv1 === tv2 =>
          // because bounds are added to both sides any a1 <: a2 and a2 :> a1
          // all type variables will become equal to each other
          // TODO: find a way to not catch those or catch recursive types differently
          // registerUnificationError(u)
          ()
        case (tv: TypeVariable, st) =>
          tv.lbs.foreach(c => newUnifyTypes(NewUnification(c.a, st, flow.addLeft(c), level), cache))
          tv.ubs.foreach(c => newUnifyTypes(NewUnification(c.b, st, flow.addLeft(c), level), cache))
        case (st, tv: TypeVariable) =>
          tv.lbs.foreach(c => newUnifyTypes(NewUnification(st, c.a, flow.addRight(c), level), cache))
          tv.ubs.foreach(c => newUnifyTypes(NewUnification(st, c.b, flow.addRight(c), level), cache))
        case _ => registerNewUnificationError(u)
      }
    }()

  def outputUnificationErrors(): Ls[Str] = {
    if (uniState.error.nonEmpty) {
      uniState.reportStats :: s"UERR ${uniState.error.size} errors" :: uniState.error.map(_.toString()).toList
    } else {
      Ls()
    }
  }

  def reportNewUnificationErrors(implicit raise: Raise, ctx: Ctx): Unit =
    uniState.error.iterator.foreach(u => raise(u.createErrorMessage()))

  case class NewUnification(a: ST, b: ST, flow: DataFlow, level: Int = 0) {
    override def toString: Str = s"L: $level [$a ~ $b, $flow]"

    def sequenceTVs: Set[TV] = {
      val tvSet: MutSet[TV] = MutSet()
      flow.constraintSequence().map { case (Constraint(a, b), _) =>
        a.unwrapProvs match { case tv: TypeVariable => tvSet += tv case _ => ()}
        b.unwrapProvs match { case tv: TypeVariable => tvSet += tv case _ => ()}
      }
      tvSet.toSet
    }

    def constraintSequence: Ls[(Constraint, Int)] = flow.constraintSequence()

    def createErrorMessage(level: Int = 0)(implicit ctx: Ctx): UniErrReport = {
      println(s"UERR REPORT $toString")
      implicit val showTV: Set[TV] = sequenceTVs
      val mainMsg = msg"[ERROR] Type `${a.expOcamlTy()(ctx, Set())}` does not match `${b.expOcamlTy()(ctx, Set())}`"
      def constraintToMessage(c: Constraint, last: Bool = false): Ls[(Message, Ls[Loc], Bool, Int, Bool)] = {
        val (a, b) = Constraint.unapply(c).get
        val flow = c.flow
        val locs = c.getCleanProvs.collect {
          case TypeProvenance(S(loc), _, _, _) => loc
        }
        def msg(a: ST): Message = a.unwrapProvs match {
          case tv: TV => msg"(${tv.expOcamlTy()}) is assumed here"
          case st => msg"(${st.expOcamlTy()}) is here"
        }

        // for the last constraint display both the types
        // from the sequence of locations show the last one for b
        if (last) {
          if (locs.isEmpty) {
            throw new Exception("No locs for relation")
          } else if (locs.length == 1) {
             (msg(a), locs, flow, level, false) :: (msg(b), locs, flow, level, true) :: Nil
          } else {
            (msg(a), locs.init, flow, level, false) :: (msg(b), locs.last :: Nil, flow, level, true) :: Nil
          }
        } else {
          (msg(a), locs, flow, level, false) :: Nil
        }
      }
      val report = flow match {
        case c@Constraint(a, b) => UniErrReport(mainMsg, constraintToMessage(c, true).map(L(_)))
        case Constructor(a, b, ctora, ctorb, flow) =>
          NewUnification(ctora, ctorb, flow, level + 1).createErrorMessage(level + 1)(ctx)
        case Sequence(flow) =>
          val msgs = flow.iterator.sliding(2).collect {
            case Seq(c: Constraint) => constraintToMessage(c, true).map(L(_))
            case Seq(Constructor(_, _, ctora, ctorb, flow)) =>
              R(NewUnification(ctora, ctorb, flow, level + 1).createErrorMessage(level + 1)(ctx)) :: Nil
            case Seq(c: Constraint, _: Constraint) => constraintToMessage(c).map(L(_))
            case Seq(c: Constraint, _: Constructor) => constraintToMessage(c, true).map(L(_))
            case Seq(Constructor(_, _, ctora, ctorb, flow), _) => R(NewUnification(ctora, ctorb, flow, level + 1).createErrorMessage(level + 1)(ctx)) :: Nil
          }.flatten.toList ::: (if (flow.length != 1) flow.last match {
            case c: Constraint => constraintToMessage(c, true).map(L(_))
            case Constructor(_, _, ctora, ctorb, flow) =>
              R(NewUnification(ctora, ctorb, flow, level + 1).createErrorMessage(level + 1)(ctx)) :: Nil
          } else { Nil })
          UniErrReport(mainMsg, msgs, level)
      }
      report
    }
  }

  class DataFlow {
    override def toString: Str = this match {
      case c@Constraint(a, b) => if (c.flow) s"$a <: $b" else s"$a :> $b"
      case Sequence(flow) => flow.mkString(", ")
      case Constructor(a, b, ctora, ctorb, flow) => s"[$a - $ctora ~ $ctorb - $b, $flow]"
    }
    def getA: ST = this match {
      case Constraint(a, b) => a
      case Sequence(flow) => flow.head.getA
      case Constructor(a, b, ctora, ctorb, flow) => a
    }
    def getB: ST = this match {
      case Constraint(a, b) => b
      case Sequence(flow) => flow.head.getB
      case Constructor(a, b, ctora, ctorb, flow) => b
    }
    def constraintSequence(level: Int = 0): Ls[(Constraint, Int)] = this match {
      case c: Constraint => (c -> level) :: Nil
      case Constructor(a, b, ctora, ctorb, flow) =>
        flow.addLeft(Constraint(a, ctora))
        flow.addRight(Constraint(b, ctorb))
        flow.constraintSequence(level + 1)
      case Sequence(flow) => flow.iterator.flatMap(_.constraintSequence(level)).toList
    }

    def addLeft(c: Constraint): Sequence = this match {
      case c1: Constraint =>
        if (c.b.unwrapProvs == c1.getA.unwrapProvs) Sequence(MutQueue(c, c1))
        else Sequence(MutQueue(c.rev(), c1))
      case Sequence(flow) =>
        if (c.b.unwrapProvs == flow.head.getA.unwrapProvs) Sequence(c +: flow)
        else Sequence(c.rev() +: flow)
      case c1: Constructor =>
        if (c.b.unwrapProvs == c1.getA.unwrapProvs) Sequence(MutQueue(c, c1))
        else Sequence(MutQueue(c.rev(), c1))
    }

    def addRight(c: Constraint): Sequence = this match {
      case c1: Constraint =>
        if (c1.getB.unwrapProvs == c.a.unwrapProvs) Sequence(MutQueue(c1, c))
        else Sequence(MutQueue(c1, c.rev()))
      case Sequence(flow) => Sequence(flow :+ c)
        if (flow.last.getB.unwrapProvs == c.a.unwrapProvs) Sequence(flow :+ c)
        else Sequence(flow :+ c.rev())
      case c1: Constructor =>
        if (c1.getB.unwrapProvs == c.a.unwrapProvs) Sequence(MutQueue(c1, c))
        else Sequence(MutQueue(c1, c.rev()))
    }

    def nestingLevel: Int = this match {
      case c: Constraint => 0
      case Sequence(flow) => flow.map(_.nestingLevel).max
      case ctor: Constructor => 1 + ctor.flow.nestingLevel
    }

    def rev(): DataFlow = this match {
      case c@Constraint(a, b) =>
        val c1 = Constraint(b, a)
        c1.flow = !c.flow
        c1
      case Constructor(a, b, ctora, ctorb, flow) =>
        Constructor(b, a, ctorb, ctora, flow.rev())
      case Sequence(flow) => Sequence(flow.reverse.map(_.rev()))
    }
  }

  case class Constraint(a: ST, b: ST) extends DataFlow {
    // true flow from a to b
    var flow = true
    def getCleanProvs: Ls[TP] = {
      val provs = a.uniqueTypeUseLocations reverse_::: b.uniqueTypeUseLocations
      if (flow) {
        // first location binds tighter so only use second prov if it's not same as first
        provs match {
          case head :: _ => head :: provs.sliding(2).collect {
            case Seq(TypeProvenance(S(loc1), _, _, _), tp@TypeProvenance(S(loc2), _, _, _)) if loc1 != loc2 => tp
          }.toList
          case _ => provs
        }
      } else {
        // second location binds tighter
        provs match {
          case ::(head, _) => head :: provs.sliding(2).collect {
            case Seq(TypeProvenance(S(loc1), _, _, _), tp@TypeProvenance(S(loc2), _, _, _)) if loc1 != loc2 => tp
          }.toList
          case Nil => Nil
        }
      }
    }
  }
  case class Sequence(flow: MutQueue[DataFlow]) extends DataFlow
  case class Constructor(a: ST, b: ST, ctora: ST, ctorb: ST, flow: DataFlow) extends DataFlow
}
