package mlscript

import mlscript.Message.MessageContext
import mlscript.utils._
import mlscript.utils.shorthands._

import scala.collection.mutable.{Set => MutSet}

trait UnificationSolver extends TyperDatatypes {
  self: Typer =>

  implicit val cache: MutSet[(ST, ST)] = MutSet()

  def unifyTypes(a: ST, b: ST, u: Opt[Unification] = N)(implicit cache: MutSet[(ST, ST)], ctx: Ctx, raise: Raise): Unit =
  trace(s"U ${a} <: ${b}"){
    val reason = (a.unwrapProvs, b.unwrapProvs) match {
      case (tv: TypeVariable, st) => UB(tv, st, a.typeUseLocations reverse_::: b.typeUseLocations)
      case (st1, st2) => LB(st1, st2, a.typeUseLocations reverse_::: b.typeUseLocations)
    }
    reason.nested = u
    unifyTypes(a, b, reason :: Nil)
  }()

  def unifyTypes(a: ST, b: ST, reason: Ls[UnificationReason])
                (implicit cache: MutSet[(ST, ST)], ctx: Ctx, raise: Raise): Unit =
    trace( s"U ${a} <: ${b} because ${reason.mkString(", ")}") {
    val st1 = a.unwrapProvs
    val st2 = b.unwrapProvs

    def createUnification(desc: Str = ""): Unification = Unification(a, b, reason, desc)

    // unification doesn't have an ordering
    if (cache((st1, st2)) || cache(st2, st1)) return
    else {
      cache += ((st1, st2))
      cache += ((st2, st1))
    }

    (st1, st2) match {
      case (tr1: TypeRef, tr2: TypeRef) =>
        if (tr1.defn === tr2.defn && tr1.targs.length === tr2.targs.length) {
          tr1.targs.zip(tr2.targs).zipWithIndex.foreach {
            case ((arg1, arg2), i) =>
              unifyTypes(arg1, arg2, S(createUnification(s"${i} index of type ref ${tr1.defn}")))
          }
        } else {
          reportUnificationError(createUnification())
        }
      case (tup1: TupleType, tup2: TupleType) =>
        if (tup1.fields.length === tup2.fields.length) {
          tup1.fields.map(_._2.ub).zip(tup2.fields.map(_._2.ub)).zipWithIndex.foreach {
            case ((t1, t2), i) =>
              unifyTypes(t1, t2, S(createUnification(s"${i} index of tuple")))
          }
        } else {
          reportUnificationError(createUnification())
        }
      case (FunctionType(arg1, res1), FunctionType(arg2, res2)) =>
        unifyTypes(arg2, arg1, S(createUnification("function argument")))
        unifyTypes(res1, res2, S(createUnification("function result")))
      case (tv1: TypeVariable, tv2: TypeVariable) if tv1 === tv2 =>
        () // TODO report error for recursive type
      case (tv: TypeVariable, st) =>
        if (tv.new_unification.nonEmpty) println(s"U  ${st} with")
        // lb <: tv <: st pass through tv and maintain constraining relation
        tv.lowerBounds.foreach(lb => {
          tv.new_unification.get(lb).foreach { case (prevReason) =>
            val (head :: tail) = reason
            val newHead = LB(lb, b, lb.typeUseLocations reverse_::: head.getProvs)
            val newReason = newHead :: tail
            println(s"    ${tv} :> ${lb.unwrapProvs} for ${prevReason}")
            println(s"    with new reason ${newReason}")
            unifyTypes(lb.unwrapProvs, st, newReason)
          }
        })
        // tv <: ub
        // tv <: st
        // do not pass through
        tv.upperBounds.foreach(ub => {
          tv.new_unification.get(ub).foreach { case (prevReason) =>
            println(s"    ${tv} <: ${ub.unwrapProvs}  for ${prevReason}")
            unifyTypes(st, ub.unwrapProvs, reason reverse_::: prevReason)
          }
        })
        println(s"U ${tv} += ${(st, reason)}")
        tv.new_unification += ((st, reason))
      case (st, tv: TypeVariable) =>
        if (tv.new_unification.nonEmpty) println(s"U  ${st} with")
        // st <: tv <: ub pass through tv and maintain constraining relation
        tv.upperBounds.foreach(ub => {
          tv.new_unification.get(ub).foreach { case (prevReason) =>
            val (head :: tail) = reason
            val newHead = LB(a, ub, head.getProvs ::: ub.typeUseLocations)
            val newReason = newHead :: tail
            println(s"    ${tv} <: ${ub.unwrapProvs} for ${prevReason}")
            println(s"    with new reason ${newReason}")
            unifyTypes(st, ub.unwrapProvs, newReason)
          }
        })
        // lb <: tv
        // st <: tv
        // do not pass through
        tv.lowerBounds.foreach(lb => {
          tv.new_unification.get(lb).foreach { case (prevReason) =>
            println(s"   ${tv} :> ${lb.unwrapProvs} for ${prevReason}")
            unifyTypes(lb.unwrapProvs, st, prevReason ::: reason.reverse)
          }
        })

        println(s"U ${tv} += ${(st, reason.reverse)}")  // reason is always from tv to st
        tv.new_unification += ((st, reason))
      case (_, _) =>
        // report error
        reportUnificationError(createUnification())
    }
  }()

  def showUnificationDebugInfo(): Unit = {
    TypeVariable.createdTypeVars.foreach(tv => {
      println(s"unified ${tv} with:")
      tv.new_unification.foreach { case (st, reason) => {
        println(s"  ${st}: ${reason.mkString(",")}")
      }
      }
    })
  }

  def createUnificationErrorMessage(u: Unification)(implicit ctx: Ctx): Ls[Message -> Opt[Loc]] = u.reason match {
    case Nil => msg"Cannot unify ${u.a.expPos} and ${u.b.expPos}" -> N :: Nil
    case _ => msg"[level ${u.unificationLevel.toString}] Cannot unify ${u.a.expPos} and ${u.b.expPos} because ${u.reason.mkString(",")}" -> N ::
      toSequenceString(u) -> N :: Nil
  }

  def toSequenceString(u: Unification)(implicit ctx: Ctx): Message = u.unificationChain match {
    case Nil => msg""
    case head :: tail => val first = head match {
      case (LB(st, tv, _), true) => msg"${st.expPos} ---> ${tv.expPos} "
      case (LB(st, tv, _), false) => msg"${tv.expPos} <--- ${st.expPos} "
      case (UB(tv, st, _), false) => msg"${st.expPos} <--- ${tv.expPos} "
      case (UB(tv, st, _), true) => msg"${tv.expPos} ---> ${st.expPos} "
    }

    msg"${(head :: tail).mkString(", ")}   " + tail.foldLeft(first) {
      case (prev, (LB(st, tv, _), true)) => prev + msg"---> ${tv.expPos} "
      case (prev, (LB(st, tv, _), false)) => prev + msg"<--- ${st.expPos} "
      case (prev, (UB(tv, st, _), false)) => prev + msg"<--- ${tv.expPos} "
      case (prev, (UB(tv, st, _), true)) => prev + msg"---> ${st.expPos} "
    }
  }

  def reportUnificationError(u: Unification)(implicit raise: Raise, ctx: Ctx) = {
    u.unificationChain match {
      case Nil => ()
      case _ :: Nil =>
        val message =
          msg"[level ${u.unificationLevel.toString}] Cannot unify ${u.a.expPos} and ${u.b.expPos} because ${u.reason.mkString(",")}" -> N ::
          toSequenceString(u) -> N :: Nil
        val report = ErrorReport(message)
        raise(report)
      case chain@(head :: next :: _) =>
        val messages = createErrorMessage(head, next, true) ::: chain.sliding(2).collect {
          case Seq(fst, snd) => createErrorMessage(fst, snd)
        }.toList.flatten
        val message =
          msg"[level ${u.unificationLevel.toString}] Cannot unify ${u.a.expPos} and ${u.b.expPos} because ${u.reason.mkString(",")}" -> N ::
            toSequenceString(u) -> N :: Nil
        val report = ErrorReport(message ::: messages)
        raise(report)
    }
  }
}
