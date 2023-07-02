package mlscript

import mlscript.Message.MessageContext
import mlscript.utils._
import mlscript.utils.shorthands._

import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.collection.mutable.{Queue => MutQueue, Set => MutSet, Map => MutMap}

trait UnificationSolver extends TyperDatatypes {
  self: Typer =>

  val cache: MutSet[(ST, ST)] = MutSet()
  var unifyMode: Bool = false

  class UnificationState (
    cache: MutSet[(ST, ST, Int)] = MutSet(),
    val queue: MutQueue[Unification] = MutQueue(),
    val error: MutSet[Unification] = MutSet(),
    // max queue length, total unifications solved
  ) extends Iterator[Unification] {
    var stats: (Int, Int) = (0, 0)
    def enqueueUnification(u: Unification): Unit = if (!cached(u)) {
      // TODO: fix this so that recursion can be stopped
      if (u.level >= 4) {
        println(s"U X ${u.a} ~ ${u.b}")
        return
      }
      println(s"U Q ${u.a} ~ ${u.b}")
      queue += u
      cache(u)
      stats = (math.max(stats._1, queue.length), stats._2 + 1)
    }
    def cached(u: Unification): Bool =
      cache((u.a.unwrapProvs, u.b.unwrapProvs, u.level)) || cache((u.b.unwrapProvs, u.a.unwrapProvs, u.level))
    def cache(u: Unification): Unit = cache += ((u.a, u.b, u.level))
    def addError(u: Unification): Unit = {
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
    override def next(): Unification = queue.dequeue()

    def unify(a: ST, b: ST): Unit = {
      enqueueUnification(Unification.fromLhsRhs(a, b))
      unify()
    }

    def unify(): Unit = foreach { u =>
      println(s"U $u")
      val st1 = u.a.unwrapProvs
      val st2 = u.b.unwrapProvs

      (st1, st2) match {
        case (tr1: TypeRef, tr2: TypeRef) if tr1.defn === tr2.defn && tr1.targs.length === tr2.targs.length =>
          tr1.targs.zip(tr2.targs).foreach { case (arg1, arg2) =>
            enqueueUnification(Unification(Queue(Constructor(arg1, arg2, tr1, tr2, u))))
          }
        case (_: TypeRef, _: TypeRef) => addError(u)
        case (tup1: TupleType, tup2: TupleType) if tup1.fields.length === tup2.fields.length =>
            tup1.fields.map(_._2).zip(tup2.fields.map(_._2)).foreach {
              case (arg1, arg2) => enqueueUnification(Unification(Queue(Constructor(arg1, arg2, tup1, tup2, u))))
            }
        case (_: TupleType, _: TupleType) => addError(u)
        case (FunctionType(arg1, res1), FunctionType(arg2, res2)) =>
          enqueueUnification(Unification(Queue(Constructor(arg1, arg2, st1, st2, u))))
          enqueueUnification(Unification(Queue(Constructor(res1, res2, st1, st2, u))))
        case (_: FunctionType, _: FunctionType) => addError(u)
        case (tv1: TypeVariable, tv2: TypeVariable) if tv1 === tv2 =>
          // because bounds are added to both sides any a1 <: a2 and a2 :> a1
          // all type variables will become equal to each other
          // TODO: find a way to not catch those or catch recursive types differently
          // registerUnificationError(u)
          ()
        case (tv: TypeVariable, rhs) if rhs.level <= tv.level =>
          // u = tv ---- st2
          tv.uni.foreach(tvuni => {
            // tvuni = tv ---- b
            if (tvuni.a.unwrapProvs == tv) enqueueUnification(tvuni.rev.concat(u))
            // tvuni = a ---- tv
            else enqueueUnification(tvuni.concat(u))
          })

          // update rhs with unification if it is a type variable
          rhs match {
            case tv: TV => tv.uni ::= u
            case _ => ()
          }
          tv.uni ::= u
        // rhs0.level >= tv.level
        case (tv: TypeVariable, rhs) =>
          extrudeTy(rhs)(tv.level)
          println(s"U EXTR ~> ${rhs.toString}")
          // u = tv ---- st2
          tv.uni.foreach(tvuni => {
            // tvuni = tv ---- b
            if (tvuni.a.unwrapProvs == tv) enqueueUnification(tvuni.rev.concat(u))
            // tvuni = a ---- tv
            else enqueueUnification(tvuni.concat(u))
          })

          // update rhs with unification if it is a type variable
          rhs match {
            case tv: TV => tv.uni ::= u
            case _ => ()
          }
          tv.uni ::= u
        case (lhs, tv: TypeVariable) if lhs.level <= tv.level =>
          // u = st1 ---- tv
          tv.uni.foreach(tvuni => {
            // tvuni = tv ---- b
            if (tvuni.a.unwrapProvs == tv) enqueueUnification(u.concat(tvuni))
            // tvuni = a ---- tv
            else enqueueUnification(u.concat(tvuni.rev))
          })

          // update lhs with unification if it is a type variable
          lhs match {
            case tv: TV => tv.uni ::= u
            case _ => ()
          }
          tv.uni ::= u
        // lhs0.level >= tv.level
        case (lhs, tv: TypeVariable) =>
          extrudeTy(lhs)(tv.level)
          println(s"U EXTR ~> ${lhs.toString}")
          // u = st1 ---- tv
          tv.uni.foreach(tvuni => {
            // tvuni = tv ---- b
            if (tvuni.a.unwrapProvs == tv) enqueueUnification(u.concat(tvuni))
            // tvuni = a ---- tv
            else enqueueUnification(u.concat(tvuni.rev))
          })

          // update lhs with unification if it is a type variable
          lhs match {
            case tv: TV => tv.uni ::= u
            case _ => ()
          }
          tv.uni ::= u
        case (st1, st2) if st1 != st2 => addError(u)
        case _ => ()
      }
    }
    
     def extrudeDF(df: DataFlow)(implicit lvl: Int): Unit = df match {
       case c@Constraint(a, b) => extrudeTy(a); extrudeTy(b)
       case Constructor(a, b, ctora, ctorb, uni) =>
         extrudeTy(a); extrudeTy(b); extrudeTy(ctora); extrudeTy(ctorb); extrudeUni(uni)
     }

     def extrudeUni(uni: Unification)(implicit lvl: Int): Unit = uni.flow.foreach(extrudeDF)

     def extrudeTy(ty: ST)(implicit lvl: Int): Unit = {
       if (ty.level <= lvl) ty else ty match {
         case t @ FunctionType(l, r) => extrudeTy(l); extrudeTy(r)
         case t @ ComposedType(p, l, r) => extrudeTy(l); extrudeTy(r)
         case t @ RecordType(fs) => fs.foreach(tup => extrudeTy(tup._2))
         case t @ TupleType(fs) => fs.foreach(tup => extrudeTy(tup._2))
         case tv: TypeVariable =>
           tv.level = lvl
           tv.uni.foreach(extrudeUni(_))
         case e @ ExtrType(_) => e
         case p @ ProvType(und) => extrudeTy(und)
         case _: RigidTypeVariable => ty
         case tr @ TypeRef(d, ts) => ts.foreach(extrudeTy)
       }
     }

    def subsume(ty_sch: PolymorphicType, sign: PolymorphicType)
               (implicit ctx: Ctx, raise: Raise, prov: TypeProvenance): Unit = {
      enqueueUnification(Unification.fromLhsRhs(ty_sch.instantiate, sign.rigidify))
    }
  }

  val newErrorCache: MutSet[Unification] = MutSet()
  val uniState = new UnificationState()

  def outputUnificationErrors(): Ls[Str] = {
    if (uniState.error.nonEmpty) {
      uniState.reportStats :: s"UERR ${uniState.error.size} errors" :: uniState.error.map(_.toString()).toList
    } else {
      Ls()
    }
  }

  def reportNewUnificationErrors(implicit ctx: Ctx, raise: Raise): Unit =
    uniState.error.toList.sorted.foreach(u => raise(u.createErrorMessage()(ctx, u.sequenceTVs)))

  case class Unification(flow: Queue[DataFlow]) extends Ordered[Unification] {
    lazy val a: ST = flow.head.getStart
    lazy val b: ST = flow.last.getEnd
    lazy val level: Int = flow.iterator.map(_.level).max

    override def compare(that: Unification): Int = {
      val levelComp = this.level.compare(that.level)
      if (levelComp != 0) {
        levelComp
      } else {
        val lengthComp = this.constraintSequence.length.compare(that.constraintSequence.length)
        if (lengthComp != 0) {
          lengthComp
        } else {
          val firstComp = a.ord.compare(a, that.a)
          if (firstComp == 0) {
            b.ord.compare(b, that.b)
          } else {
            firstComp
          }
        }
      }
    }

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
        (Constraint.startTransition(a, ctora), level) :: uni.constraintSequence ::: (Constraint.endTransition(ctorb, b), level) :: Nil
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

      implicit val showTV: Set[TV] = sequenceTVs

      // Currently only show type sequence for types at current level of nesting
      val sequenceMessage: Ls[Message] = constraintSequence.iterator.zipWithIndex.filter(_._1._2 == level).map{
        case ((c@Constraint(a, b), _), idx) => c.transition match {
          case Some(true) => msg"(${a.expOcamlTy()(ctx, showTV)}) ~~~~ "
          case Some(false) => if (idx == constraintSequence.length - 1) msg"(${b.expOcamlTy()(ctx, showTV)})" else msg""
          case None =>
            val arrowStr = if (c.dir) "--->" else "<---"
            val last = idx == constraintSequence.length - 1
            msg"(${a.expOcamlTy()(ctx, showTV)}) $arrowStr " + (if (last) msg"(${b.expOcamlTy()(ctx, showTV)})" else msg"")
        }
      }.toList

      val sctx = Message.mkCtx(sequenceMessage, "?")
      val sb = new mutable.StringBuilder();
      sequenceMessage.foreach(msg => sb ++= msg.showIn(sctx))
      sb.toString()
    }

    def concat(other: Unification): Unification = {
      assert(b.unwrapProvs == other.a.unwrapProvs, s"$b != ${other.a}")
      Unification(flow.enqueueAll(other.flow))
    }

    def nestingLevel: Int = flow.map {
      case _: Constraint => 0
      case ctor: Constructor => 1 + ctor.uni.nestingLevel
    }.max

    def rev: Unification = Unification(flow.map(_.rev).reverse)

    def createErrorMessage(level: Int = 0)(implicit ctx: Ctx, showTV: Set[TV]): UniErrReport = {
      println(s"UERR REPORT $toString")
      val mainMsg = msg"Type `${a.expOcamlTy()(ctx, Set())}` does not match `${b.expOcamlTy()(ctx, Set())}`"
      val seqString = createSequenceString
      def msg(a: ST): Message = a.unwrapProvs match {
        case tv: TV => msg"(${tv.expOcamlTy()}) is assumed for"
        case st => msg"(${st.expOcamlTy()}) comes from"
      }

      def constraintToMessage(c: Constraint, last: Bool = false): Ls[(Message, Ls[Loc], Bool, Int, Bool)] = {
        val (a, b) = Constraint.unapply(c).get
        val flow = c.dir
        val locs = c.getCleanProvs.collect {
          case TypeProvenance(S(loc), _, _, _) => loc
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

      // Helpful show types being projected from their constructors
      def constructorArgumentMessage(c: Constructor, leftEnd: Bool, level: Int): (Message, Ls[Loc], Bool, Int, Bool) = {
        val ty = if (leftEnd) { c.a } else { c.b }
        val locs = ty.uniqueTypeUseLocations.collect {
          case TypeProvenance(S(loc), _, _, _) => loc
        }
        (msg(ty), locs, false, level, true)
      }

      val report = {
        val msgs = flow.iterator.sliding(2).zipWithIndex.collect {
          // single constraint
          case (Seq(c: Constraint), _) => constraintToMessage(c, true).map(L(_))
          // single constructor show projected types
          case (Seq(ctor@Constructor(_, _, _, _, uni)), _) =>
            L(constructorArgumentMessage(ctor, true, level)) ::
              R(uni.createErrorMessage(level + 1)(ctx, showTV)) ::
              L(constructorArgumentMessage(ctor, false, level)) ::
              Nil
          case (Seq(c: Constraint, _: Constraint), _) => constraintToMessage(c).map(L(_))
          case (Seq(c: Constraint, _: Constructor), _) => constraintToMessage(c, true).map(L(_))
          // if there are two constructors side by side
          // project their common type once
          case (Seq(ctor@Constructor(_, _, _, _, uni), ctor2: Constructor), idx) if ctor.b === ctor2.a =>
            val project = L(constructorArgumentMessage(ctor, false, level))
            val nestedReport = R(uni.createErrorMessage(level + 1)(ctx, showTV))
            if (idx == 0) {
              L(constructorArgumentMessage(ctor, true, level)) :: nestedReport :: project :: Nil
            } else {
              nestedReport :: project :: Nil
            }
          // if constructor is first in the sequence project left type
          case (Seq(ctor@Constructor(_, _, _, _, uni), _), idx) =>
            val nestedReport = R(uni.createErrorMessage(level + 1)(ctx, showTV)) :: Nil
            if (idx == 0) {
              L(constructorArgumentMessage(ctor, true, level)) :: nestedReport
            } else {
              nestedReport
            }
        }.flatten.toList ::: (if (flow.length != 1) flow.last match {
          case c: Constraint => constraintToMessage(c, true).map(L(_))
          case ctor@Constructor(_, _, _, _, uni) =>
            // if constructor is last in the sequence project type
            R(uni.createErrorMessage(level + 1)(ctx, showTV)) :: L(constructorArgumentMessage(ctor, false, level)) :: Nil
        } else { Nil })
        UniErrReport(mainMsg, seqString, msgs, level)
      }
      report
    }
  }

  object Unification {
    def fromLhsRhs(lhs: ST, rhs: ST): Unification = Unification(Queue(Constraint(lhs, rhs)))
  }

  sealed abstract class DataFlow {
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

    lazy val level: Int = this match {
      case _: Constraint => 0
      case ctor: Constructor => ctor.uni.level + 1
    }

    override def toString: Str = this match {
      case c@Constraint(a, b) => if (c.dir) s"${a.unwrapProvs} <: ${b.unwrapProvs}" else s"${a.unwrapProvs} :> ${b.unwrapProvs}"
      case Constructor(a, b, ctora, ctorb, flow) => s"[${a.unwrapProvs} - ${ctora.unwrapProvs} ~ ${ctorb.unwrapProvs} - ${b.unwrapProvs}, $flow]"
    }
  }

  case class Constraint(a: ST, b: ST) extends DataFlow {
    // true flow from a to b
    var dir = true
    // this is a special constrain that shows a transition between levels
    // this variable is only used during error reporting and not during
    // actual unification
    // N - default no transition
    // S(true) - start transition, `a` goes into `b`
    // S(false) - end transition, `b` comes out of `a`
    var transition: Opt[Bool] = N
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

  object Constraint {
    def startTransition(a: ST, b: ST) = {
      val c = Constraint(a, b)
      c.transition = S(true)
      c
    }
    def endTransition(a: ST, b: ST) = {
      val c = Constraint(a, b)
      c.transition = S(false)
      c
    }
  }
  case class Constructor(a: ST, b: ST, ctora: ST, ctorb: ST, uni: Unification) extends DataFlow


  // Note: maybe this and `extrude` should be merged?
  def freshenAbove(lim: Int, ty: SimpleType, rigidify: Bool = false)(implicit lvl: Int): SimpleType = {
    val freshened = MutMap.empty[TV, SimpleType]

    def freshenDataFlow(df: DataFlow): DataFlow = df match {
      case c@Constraint(a, b) =>
        val c1 = Constraint(freshen(a), freshen(b))
        c1.dir = c.dir
        c1
      case Constructor(a, b, ctora, ctorb, uni) =>
        Constructor(freshen(a), freshen(b), freshen(ctora), freshen(ctorb), freshenUnification(uni))
    }

    def freshenUnification(u: Unification): Unification = u.copy(flow = u.flow.map(freshenDataFlow))

    def freshen(ty: SimpleType): SimpleType =
      if (!rigidify // Rigidification now also substitutes TypeBound-s with fresh vars;
        // since these have the level of their bounds, when rigidifying
        // we need to make sure to copy the whole type regardless of level...
        && ty.level <= lim) ty else ty match {
        case tv: TypeVariable => freshened.get(tv) match {
          case Some(tv) => tv
          case None if rigidify =>
            val rv = RigidTypeVariable( // Rigid type variables (ie, skolems) are encoded as TraitTag-s
              Var(tv.nameHint.getOrElse("_" + freshVar(noProv).toString)))(tv.prov)
            if (tv.lowerBounds.nonEmpty || tv.upperBounds.nonEmpty) {
              // The bounds of `tv` may be recursive (refer to `tv` itself),
              //    so here we create a fresh variabe that will be able to tie the presumed recursive knot
              //    (if there is no recursion, it will just be a useless type variable)
              val tv2 = freshVar(tv.prov, tv.nameHint)
              freshened += tv -> tv2
              // Assuming there were no recursive bounds, given L <: tv <: U,
              //    we essentially need to turn tv's occurrence into the type-bounds (rv | L)..(rv & U),
              //    meaning all negative occurrences should be interpreted as rv | L
              //    and all positive occurrences should be interpreted as rv & U
              //    where rv is the rigidified variables.
              // Now, since there may be recursive bounds, we do the same
              //    but through the indirection of a type variable tv2:
              tv2.lowerBounds ::= tv.lowerBounds.map(freshen).foldLeft(rv: ST)(_ & _)
              tv2.upperBounds ::= tv.upperBounds.map(freshen).foldLeft(rv: ST)(_ | _)
              // freshen all the unifications for the type variable
              // without this unification cannot detect errors because
              // when a type scheme is instantiated it won't have the
              // types it's been previously unified with
              tv2.uni = tv.uni.map(freshenUnification)
              tv2
            } else {
              freshened += tv -> rv
              rv
            }
          case None =>
            val v = freshVar(tv.prov, tv.nameHint)
            freshened += tv -> v
            v.lowerBounds = tv.lowerBounds.mapConserve(freshen)
            v.upperBounds = tv.upperBounds.mapConserve(freshen)
            // freshen all the unifications for the type variable
            // without this unification cannot detect errors because
            // when a type scheme is instantiated it won't have the
            // types it's been previously unified with
            v.uni = tv.uni.map(freshenUnification)
            v
        }
        case t@FunctionType(l, r) => FunctionType(freshen(l), freshen(r))(t.prov)
        case t@ComposedType(p, l, r) => ComposedType(p, freshen(l), freshen(r))(t.prov)
        case t@RecordType(fs) => RecordType(fs.mapValues(freshen))(t.prov)
        case t@TupleType(fs) => TupleType(fs.mapValues(freshen))(t.prov)
        case e@ExtrType(_) => e
        case p@ProvType(und) => ProvType(freshen(und))(p.prov)
        case p@ProvType(und) => freshen(und)
        case _: RigidTypeVariable => ty
        case tr@TypeRef(d, ts) => TypeRef(d, ts.map(freshen))(tr.prov)
      }

    freshen(ty)
  }

  // helper functions
  def err(msg: Message, loco: Opt[Loc])(implicit raise: Raise): SimpleType = err(msg -> loco :: Nil)
  def err(msgs: List[Message -> Opt[Loc]])(implicit raise: Raise): SimpleType = {
    raise(ErrorReport(msgs))
    TypeRef(TypeName("err"), Nil)(noProv)
  }
  def warn(msg: Message, loco: Opt[Loc])(implicit raise: Raise): Unit = warn(msg -> loco :: Nil)
  def warn(msgs: List[Message -> Opt[Loc]])(implicit raise: Raise): Unit = raise(WarningReport(msgs))

}
