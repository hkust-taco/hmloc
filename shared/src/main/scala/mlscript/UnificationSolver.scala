package mlscript

import mlscript.Message.MessageContext
import mlscript.utils._
import mlscript.utils.shorthands._

import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.collection.mutable.{Queue => MutQueue, Set => MutSet}

trait UnificationSolver extends TyperDatatypes {
  self: Typer =>

  val cache: MutSet[(ST, ST)] = MutSet()
  var unifyMode: Bool = false

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
        println(s"  | U X ${u.a} ~ ${u.b}")
        return
      }
      println(s"  | U Q ${u.a} ~ ${u.b}")
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

    def unify(): Unit = foreach { case u@NewUnification(_, level) =>
      println(s"U $u")
      val st1 = u.a.unwrapProvs
      val st2 = u.b.unwrapProvs

      (st1, st2) match {
        case (tr1: TypeRef, tr2: TypeRef) if tr1.defn === tr2.defn && tr1.targs.length === tr2.targs.length =>
          tr1.targs.zip(tr2.targs).foreach { case (arg1, arg2) =>
            enqueueUnification(NewUnification(Queue(Constructor(arg1, arg2, tr1, tr2, u)), level + 1))
          }
        case (_: TypeRef, _: TypeRef) => addError(u)
        case (tup1: TupleType, tup2: TupleType) if tup1.fields.length === tup2.fields.length =>
            tup1.fields.map(_._2.ub).zip(tup2.fields.map(_._2.ub)).foreach {
              case (arg1, arg2) => enqueueUnification(NewUnification(Queue(Constructor(arg1, arg2, tup1, tup2, u)), level + 1))
            }
        case (_: TupleType, _: TupleType) => addError(u)
        case (FunctionType(arg1, res1), FunctionType(arg2, res2)) =>
          enqueueUnification(NewUnification(Queue(Constructor(arg1, arg2, st1, st2, u)), level + 1))
          enqueueUnification(NewUnification(Queue(Constructor(res1, res2, st1, st2, u)), level + 1))
        case (_: FunctionType, _: FunctionType) => addError(u)
        case (tv1: TypeVariable, tv2: TypeVariable) if tv1 === tv2 =>
          // because bounds are added to both sides any a1 <: a2 and a2 :> a1
          // all type variables will become equal to each other
          // TODO: find a way to not catch those or catch recursive types differently
          // registerUnificationError(u)
          ()
        case (tv: TypeVariable, _) =>
          // u = tv ---- st2
          tv.uni.foreach(tvuni => {
            // tvuni = tv ---- b
            if (tvuni.a.unwrapProvs == tv) enqueueUnification(tvuni.rev.concat(u))
            // tvuni = a ---- tv
            else enqueueUnification(tvuni.concat(u))
          })

          tv.uni ::= u
        case (_, tv: TypeVariable) =>
          // u = st1 ---- tv
          tv.uni.foreach(tvuni => {
            // tvuni = tv ---- b
            if (tvuni.a.unwrapProvs == tv) enqueueUnification(u.concat(tvuni))
            // tvuni = a ---- tv
            else enqueueUnification(u.concat(tvuni.rev))
          })

          tv.uni ::= u
        case (st1, st2) if st1 != st2 => addError(u)
        case _ => ()
      }
    }
  }

  val newErrorCache: MutSet[NewUnification] = MutSet()
  val uniState = new UnificationState()

  // Unify all type variables crated by accessing them from the hook
  def newUnifyTypes()(implicit ctx: Ctx, raise: Raise): Unit = {
//    cache.clear()
//    uniState.clear()

//    // register all bounds as unifications
//    TypeVariable.createdTypeVars.foreach(tv => {
//      tv.lowerBounds.foreach(tv.lbs ::= Constraint(_, tv))
//      tv.upperBounds.foreach(tv.ubs ::= Constraint(tv, _))
//    })
//
//    TypeVariable.createdTypeVars.foreach(tv => {
//      tv.lbs.foreach{ c => uniState.enqueueUnification(NewUnification(Queue(c), 0))}
//      tv.ubs.foreach{ c => uniState.enqueueUnification(NewUnification(Queue(c), 0))}
//    })

    uniState.unify()
//    uniState.clear()
//    cache.clear()
  }

  def outputUnificationErrors(): Ls[Str] = {
    if (uniState.error.nonEmpty) {
      uniState.reportStats :: s"UERR ${uniState.error.size} errors" :: uniState.error.map(_.toString()).toList
    } else {
      Ls()
    }
  }

  def reportNewUnificationErrors(implicit ctx: Ctx, raise: Raise): Unit =
    uniState.error.iterator.foreach(u => raise(u.createErrorMessage()))

  case class NewUnification(flow: Queue[DataFlow], level: Int) {
    lazy val a: ST = flow.head.getStart
    lazy val b: ST = flow.last.getEnd

    override def toString: Str = s"L: $level [${a.unwrapProvs} ~ ${b.unwrapProvs}, ${flow.mkString(", ")}]"

    def sequenceTVs: Set[TV] = {
      val tvSet: MutSet[TV] = MutSet()
      constraintSequence.map { case (Constraint(a, b), _) =>
        a.unwrapProvs match { case tv: TypeVariable => tvSet += tv case _ => ()}
        b.unwrapProvs match { case tv: TypeVariable => tvSet += tv case _ => ()}
      }
      tvSet.toSet
    }

    lazy val constraintSequence: Ls[(Constraint, Int)] = flow.iterator.flatMap {
      case c: Constraint => (c, level) :: Nil
      case Constructor(a, b, ctora, ctorb, uni) =>
        uni.addLeft(Constraint(a, ctora)).addRight(Constraint(b, ctorb)).constraintSequence
    }.toList

    def createSequenceString(implicit ctx: Ctx): Str = {
      def  constraintToSequence(c: Constraint, last: Bool = false): Ls[(ST, Opt[Bool])] = {
        val (a, b) = Constraint.unapply(c).get
        val flow = c.dir
        // for the last constraint display both the types
        // don't show arrow for last type
        if (last) {
          (a, S(flow)) :: (b, N) :: Nil
        } else {
          (a, S(flow)) :: Nil
        }
      }

      def arrowString(c: Constraint): String = if (c.dir) " ---> " else " <--- "

      // Currently only show type sequence for types at currentl level of nesting
      val sequence: Ls[(ST, String)] = constraintSequence.iterator.zipWithIndex.sliding(2).collect {
        // ignore constraints of higher levels
        case Seq(((_, l1), _), ((_, l2), _)) if l1 < level && l2 < level => Nil
        // constraint before changing level should show both types and end with unification symbols
        case Seq(((c@Constraint(a, b), l1), _), ((Constraint(_, _), l2), _)) if l1 > l2 => (a, arrowString(c)) :: (b, " ~~~~ ") :: Nil
        // constraint after changing level should show both types if the next constraint is the last constraint
        case Seq(((Constraint(_, _), l1), _), ((c@Constraint(a, b), l2), idx))
          if l1 < l2 && idx == constraintSequence.length - 1 => (a, arrowString(c)) :: (b, "") :: Nil
        // constraint after changing level
        case Seq(((Constraint(_, _), l1), _), ((c@Constraint(a, _), l2), _)) if l1 < l2 => (a, arrowString(c)) :: Nil
        // if next constraint is last constraint show three types
        case Seq(((c@Constraint(a, _), _), _), ((c1@Constraint(b, d), _), idx)) if idx == constraintSequence.length - 1 =>
          (a, arrowString(c)) :: (b, arrowString(c1)) :: (d, "") :: Nil
        // if next constraint is not last constraint show only current constraints type
        case Seq(((c@Constraint(a, _), _), _), _) => (a, arrowString(c)) :: Nil
        // single constraint should show both types
        case Seq(((c@Constraint(a, b), _), _)) => (a, arrowString(c)) :: (b, "") :: Nil
        case _ => Nil
      }.flatten.toList

      implicit val showTV: Set[TV] = sequenceTVs
      val sequenceMessage = sequence.map { case (st, arrowString) => msg"(${st.expOcamlTy()(ctx, showTV)})${arrowString}" }
      val sctx = Message.mkCtx(sequenceMessage, "?")
      val sb = new mutable.StringBuilder();
      sequenceMessage.foreach(msg => sb ++= msg.showIn(sctx))
      sb.toString()
    }

    def addLeft(c: Constraint): NewUnification = {
      if (c.b.unwrapProvs == flow.head.getStart.unwrapProvs) NewUnification(c +: flow, level)
      else NewUnification(c.rev +: flow, level)
    }

    def addRight(c: Constraint): NewUnification = {
      if (flow.last.getEnd.unwrapProvs == c.a.unwrapProvs) NewUnification(flow :+ c, level)
      else NewUnification(flow :+ c.rev, level)
    }

    def concat(other: NewUnification): NewUnification = {
      assert(b.unwrapProvs == other.a.unwrapProvs, s"$b != ${other.a}")
      NewUnification(flow.enqueueAll(other.flow), level)
    }

    def nestingLevel: Int = flow.map {
      case _: Constraint => 0
      case ctor: Constructor => 1 + ctor.uni.nestingLevel
    }.max

    def rev: NewUnification = NewUnification(flow.map(_.rev).reverse, level)

    def createErrorMessage(level: Int = 0)(implicit ctx: Ctx): UniErrReport = {
      println(s"UERR REPORT $toString")
      implicit val showTV: Set[TV] = sequenceTVs
      val mainMsg = msg"Type `${a.expOcamlTy()(ctx, Set())}` does not match `${b.expOcamlTy()(ctx, Set())}`"
      val seqString = createSequenceString
      def constraintToMessage(c: Constraint, last: Bool = false): Ls[(Message, Ls[Loc], Bool, Int, Bool)] = {
        val (a, b) = Constraint.unapply(c).get
        val flow = c.dir
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

      val report = {
        val msgs = flow.iterator.sliding(2).collect {
          case Seq(c: Constraint) => constraintToMessage(c, true).map(L(_))
          case Seq(Constructor(_, _, _, _, uni)) =>
            R(NewUnification(uni.flow, level + 1).createErrorMessage(level + 1)(ctx)) :: Nil
          case Seq(c: Constraint, _: Constraint) => constraintToMessage(c).map(L(_))
          case Seq(c: Constraint, _: Constructor) => constraintToMessage(c, true).map(L(_))
          case Seq(Constructor(_, _, _, _, uni), _) => R(NewUnification(uni.flow, level + 1).createErrorMessage(level + 1)(ctx)) :: Nil
        }.flatten.toList ::: (if (flow.length != 1) flow.last match {
          case c: Constraint => constraintToMessage(c, true).map(L(_))
          case Constructor(_, _, _, _, uni) =>
            R(NewUnification(uni.flow, level + 1).createErrorMessage(level + 1)(ctx)) :: Nil
        } else { Nil })
        UniErrReport(mainMsg, seqString, msgs, level)
      }
      report
    }
  }

  object NewUnification {
    def fromLhsRhs(lhs: ST, rhs: ST): NewUnification = NewUnification(Queue(Constraint(lhs, rhs)), 0)
  }

  class DataFlow {
    def getStart: ST = this match {
      case c: Constraint => c.a
      case c: Constructor => c.a
    }

    def getEnd: ST = this match {
      case c: Constraint => c.b
      case c: Constructor => c.b
    }

    def rev: DataFlow = this match {
      case c@Constraint(a, b) =>
        val c1 = Constraint(b, a)
        c1.dir = !c.dir
        c1
      case Constructor(a, b, ctora, ctorb, flow) =>
        Constructor(b, a, ctorb, ctora, flow.rev)
    }

    override def toString: Str = this match {
      case c@Constraint(a, b) => if (c.dir) s"${a.unwrapProvs} <: ${b.unwrapProvs}" else s"${a.unwrapProvs} :> ${b.unwrapProvs}"
      case Constructor(a, b, ctora, ctorb, flow) => s"[${a.unwrapProvs} - ${ctora.unwrapProvs} ~ ${ctorb.unwrapProvs} - ${b.unwrapProvs}, $flow]"
    }
  }

  case class Constraint(a: ST, b: ST) extends DataFlow {
    // true flow from a to b
    var dir = true
    def getCleanProvs: Ls[TP] = {
      val provs = a.uniqueTypeUseLocations reverse_::: b.uniqueTypeUseLocations
      if (dir) {
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
  case class Constructor(a: ST, b: ST, ctora: ST, ctorb: ST, uni: NewUnification) extends DataFlow
}
