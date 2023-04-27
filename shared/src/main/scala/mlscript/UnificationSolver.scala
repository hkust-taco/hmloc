package mlscript

import mlscript.Message.MessageContext
import mlscript.utils._
import mlscript.utils.shorthands._

import scala.collection.mutable
import scala.collection.mutable.{Map => MutMap, Queue => MutQueue, Set => MutSet}

trait UnificationSolver extends TyperDatatypes {
  self: Typer =>

  val cache: MutSet[(ST, ST)] = MutSet()
  var unifyMode: Bool = false

  def registerNewUnificationError(u: NewUnification): Unit = {
    println(s"UERR $u")
    newErrorCache += u
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

  def reportNewUnificationErrors(implicit ctx: Ctx, raise: Raise): Unit =
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

    def createSequenceString(level: Int = 0)(implicit ctx: Ctx): Str = {
      def  constraintToSequence(c: Constraint, last: Bool = false): Ls[(ST, Opt[Bool])] = {
        val (a, b) = Constraint.unapply(c).get
        val flow = c.flow
        // for the last constraint display both the types
        // don't show arrow for last type
        if (last) {
          (a, S(flow)) :: (b, N) :: Nil
        } else {
          (a, S(flow)) :: Nil
        }
      }

      val sequence = flow match {
        case c: Constraint => constraintToSequence(c)
        case Sequence(flow) => flow.iterator.zipWithIndex.collect {
          case (c: Constraint, idx) => constraintToSequence(c, idx == flow.length - 1)
        }.flatten.toList
        case _ => Nil
      }

      implicit val showTV: Set[TV] = sequenceTVs
      val sequenceMessage = sequence.map{
        case (st, S(true)) => msg"(${st.expOcamlTy()(ctx, showTV)}) ---> "
        case (st, S(false)) => msg"(${st.expOcamlTy()(ctx, showTV)}) <--- "
        case (st, N) => msg"(${st.expOcamlTy()(ctx, showTV)})"
      }

      val sctx = Message.mkCtx(sequenceMessage, "?")
      val sb = new mutable.StringBuilder();
      sequenceMessage.foreach(msg => sb ++= msg.showIn(sctx))
      sb.toString()
    }

    def createErrorMessage(level: Int = 0)(implicit ctx: Ctx): UniErrReport = {
      println(s"UERR REPORT $toString")
      implicit val showTV: Set[TV] = sequenceTVs
      val mainMsg = msg"Type `${a.expOcamlTy()(ctx, Set())}` does not match `${b.expOcamlTy()(ctx, Set())}`"
      val seqString = createSequenceString()
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
        case c@Constraint(a, b) => UniErrReport(mainMsg, "", constraintToMessage(c, true).map(L(_)))
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
          UniErrReport(mainMsg, seqString, msgs, level)
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
