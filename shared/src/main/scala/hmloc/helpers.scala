package hmloc

import hmloc.utils._
import hmloc.utils.shorthands._

import scala.collection.mutable
import scala.collection.mutable.{Buffer, Map => MutMap, SortedMap => SortedMutMap}
import scala.math.Ordered.orderingToOrdered
import scala.util.chaining._


// Auxiliary definitions for types

abstract class TypeImpl extends Located { self: Type =>
  
  lazy val typeVarsList: List[TypeVar] = this match {
    case uv: TypeVar => uv :: Nil
    case Recursive(n, b) => n :: b.typeVarsList
    case _ => children.flatMap(_.typeVarsList)
  }

  /**
    * @return
    *  set of free type variables in type
    */
  lazy val freeTypeVariables: Set[TypeVar] = this match {
    case Recursive(uv, body) => body.freeTypeVariables - uv
    case t: TypeVar => Set.single(t)
    case _ => this.children.foldRight(Set.empty[TypeVar])((ty, acc) => ty.freeTypeVariables ++ acc)
  }
  
  def show: Str = showIn(ShowCtx.mk(this :: Nil), 0)
  
  private def parensIf(str: Str, cnd: Boolean): Str = if (cnd) "(" + str + ")" else str

  def showOcaml(ctx: ShowCtx, outerPrec: Int): Str = this match {
    case Top => "anything"
    case Bot => "nothing"
    case c: Composed =>
      val prec = if (c.pol) 20 else 25
      val opStr = if (c.pol) " | " else " & "
      c.distinctComponents match {
        case Nil => (if (c.pol) Bot else Top).showIn(ctx, prec)
        case x :: Nil => x.showIn(ctx, prec)
        case _ =>
          parensIf(c.distinctComponents.iterator
            .map(_.showIn(ctx, prec))
            .reduce(_ + opStr + _), outerPrec > prec)
      }
    case TypeVar(_, S("_")) => "_"
    case uv: TypeVar => ctx.vs(uv)
    case Function(l, r) => parensIf(l.showOcaml(ctx, 31) + " -> " + r.showOcaml(ctx, 30), outerPrec > 30)
    case Tuple(fields) => parensIf(s"${fields.map(_.showOcaml(ctx, 2)).mkString(" * ")}", outerPrec > 1)
    case AppliedType(n, arg :: Nil) => s"${arg.showOcaml(ctx, 2)} ${n.name}"
    case AppliedType(n, args) => s"(${args.map(_.showOcaml(ctx, 2)).mkString(", ")}) ${n.name}"
    case TypeName(name) => name
    case TypeTag(name) => "#"+name
    case _ => lastWords(s"Cannot create showOcaml for ${this.show} ${this.getClass.getSimpleName}")
  }

  def showIn(ctx: ShowCtx, outerPrec: Int): Str = this match {
  // TODO remove obsolete pretty-printing hacks
    case Top => "anything"
    case Bot => "nothing"
    case TypeName(name) => name
    // case uv: TypeVar => ctx.vs.getOrElse(uv, s"[??? $uv ???]")
    case TypeTag(name) => "#"+name
    case uv: TypeVar => ctx.vs(uv)
    case Recursive(n, b) => parensIf(s"${b.showIn(ctx, 2)} as ${ctx.vs(n)}", outerPrec > 1)
    case Function(Tuple(l :: Nil), r) => Function(l, r).showIn(ctx, outerPrec)
    case Function(l, r) => parensIf(l.showIn(ctx, 31) + " -> " + r.showIn(ctx, 30), outerPrec > 30)
    case Tuple(fs) =>
      fs.map(nt => s"${nt.showIn(ctx, 0)},").mkString("(", " ", ")")
    case Union(TypeName("true"), TypeName("false")) | Union(TypeName("false"), TypeName("true")) =>
      TypeName("bool").showIn(ctx, 0)
    case c: Composed =>
      val prec = if (c.pol) 20 else 25
      val opStr = if (c.pol) " | " else " & "
      c.distinctComponents match {
        case Nil => (if (c.pol) Bot else Top).showIn(ctx, prec)
        case x :: Nil => x.showIn(ctx, prec)
        case _ =>
          parensIf(c.distinctComponents.iterator
            .map(_.showIn(ctx, prec))
            .reduce(_ + opStr + _), outerPrec > prec)
      }
    case AppliedType(n, args) => s"${n.name}[${args.map(_.showIn(ctx, 0)).mkString(", ")}]"
    case Unified(ty, us) => parensIf(s"${ty.showIn(ctx, 0)}\n  where${us.map {
      case (tv, tys) =>
        s"\n    ${ctx.vs(tv)} = ${tys.map(_.showIn(ctx, 0)).mkString(", ")}"
    }.mkString}", outerPrec > 0)
  }
  
  def children: List[Type] = this match {
    case Function(l, r) => l :: r :: Nil
    case Tuple(fs) => fs
    case Union(l, r) => l :: r :: Nil
    case Inter(l, r) => l :: r :: Nil
    case Recursive(n, b) => b :: Nil
    case AppliedType(n, ts) => n :: ts
    case Unified(ty, us) => ty.children ::: us.flatMap { case (tv, tys) => tv :: tys.flatMap(_.children)}
    case _: TypeName | _: TypeVar | _: Top.type | _: Bot.type => Nil
    case t => lastWords(s"Cannot find children for type $t")
  }
}


final case class ShowCtx(vs: Map[TypeVar, Str])
object ShowCtx {
  /**
    * Create a context from a list of types. For named variables and
    * hinted variables use what is given. For unnamed variables generate
    * completely new names. If same name exists increment counter suffix
    * in the name.
    */
  def mk(tys: IterableOnce[Type], pre: Str = "'"): ShowCtx = {
    val (otherVars, namedVars) = tys.iterator.toList.flatMap(_.typeVarsList).distinct.partitionMap { tv =>
      tv.identifier match { case L(_) => L(tv.nameHint -> tv); case R(nh) => R(nh -> tv) }
    }
    val (hintedVars, unnamedVars) = otherVars.partitionMap {
      case (S(nh), tv) => L(nh -> tv)
      case (N, tv) => R(tv)
    }
    val usedNames = MutMap.empty[Str, Int]
    def assignName(n: Str): Str =
      usedNames.get(n) match {
        case S(cnt) =>
          usedNames(n) = cnt + 1
          pre + 
          n + cnt
        case N =>
          usedNames(n) = 0
          pre + 
          n
      }
    val namedMap = (namedVars ++ hintedVars).map { case (nh, tv) =>
      tv -> assignName(nh.dropWhile(_ === '\''))
      // tv -> assignName(nh.stripPrefix(pre))
    }.toMap
    val used = usedNames.keySet
    
    // * Generate names for unnamed variables
    val numLetters = 'z' - 'a' + 1
    val names = Iterator.unfold(0) { idx =>
      val postfix = idx/numLetters
      S(('a' + idx % numLetters).toChar.toString + (if (postfix === 0) "" else postfix.toString), idx + 1)
    }.filterNot(used).map(assignName)
    
    ShowCtx(namedMap ++ unnamedVars.zip(names))
  }
}

trait ComposedImpl { self: Composed =>
  val lhs: Type
  val rhs: Type
  def components: Ls[Type] = (lhs match {
    case c: Composed if c.pol === pol => c.components
    case _ => lhs :: Nil
  }) ::: (rhs match {
    case c: Composed if c.pol === pol => c.components
    case _ => rhs :: Nil
  })
  lazy val distinctComponents =
    components.filterNot(c => if (pol) c === Bot else c === Top).distinct
}

abstract class PolyTypeImpl extends Located { self: PolyType =>
  def children: List[Located] =  targs :+ body
  def show: Str = s"${targs.iterator.map(_.name).mkString("[", ", ", "] ->")} ${body.show}"
}

trait TypeVarImpl extends Ordered[TypeVar] { self: TypeVar =>
  def compare(that: TypeVar): Int = {
    this.identifier.fold((_, ""), (0, _)) compare that.identifier.fold((_, ""), (0, _))
  }
}

// Auxiliary definitions for terms

trait PgrmImpl { self: Pgrm =>
  lazy val desugared: (Ls[TypeDef], Ls[Terms]) = {
    tops.partitionMap {
      case td: TypeDef => L(td)
      case ot: Terms => R(ot)
   }
  }
  override def toString = tops.map("" + _ + ";").mkString(" ")
}

object OpApp {
  def apply(op: Str, trm: Term): Term = App(Var(op), trm)
  def unapply(trm: Term): Opt[Term -> Term] = trm |>? {
    case App(op, lhs)
      if op.toLoc.exists(l => lhs.toLoc.exists(l.spanStart > _.spanStart))
      => (op, lhs)
  }
}

trait DeclImpl extends Located { self: Decl =>
  val body: Located
  def showBody: Str = this match {
    case Def(_, _, rhs, isByname) => rhs.fold(_.toString, _.show)
    case td: TypeDef => td.body.show
  }
  override def describe: Str = this match {
    case _: Def => "definition"
    case _: TypeDef => "type declaration"
  }
  def show: Str = showHead + (this match {
    case TypeDef(Als, _, _, _, _) => " = "; case _ => ": " }) + showBody
  def showHead: Str = this match {
    case Def(true, n, b, isByname) => s"rec def $n"
    case Def(false, n, b, isByname) => s"def $n"
    case TypeDef(k, n, tps, b, _) =>
      s"${k.str} ${n.name}${if (tps.isEmpty) "" else tps.map(_.name).mkString("[", ", ", "]")}"
  }
}

trait TypeNameImpl extends Ordered[TypeName] { self: TypeName =>
  def compare(that: TypeName): Int = this.name compare that.name
}

trait TermImpl extends StatementImpl { self: Term =>
  private var sugaredTerm: Opt[Term] = N

  override def describe: Str = sugaredTerm match {
    case S(t) => t.describe
    case N => this match {
      // TODO check this case
      case Blk((trm: Term) :: Nil) => trm.describe
      case Blk(_) => "block of statements"
      case IntLit(value) => "integer literal"
      case DecLit(value) => "float literal"
      case StrLit(value) => "string literal"
      case UnitLit(value) => if (value) "undefined literal" else "null literal"
      case Var(name) => "reference" // "variable reference"
      case Asc(trm, ty) => "type ascription"
      case Lam(name, rhs) => "lambda expression"
      case App(OpApp(Var("|"), lhs), rhs) => "type union"
      case App(OpApp(Var("&"), lhs), rhs) => "type intersection"
      case App(OpApp(op, lhs), rhs) => "operator application"
      case OpApp(op, lhs) => "operator application"
      case App(lhs, rhs) => "application"
      case Let(isRec, name, rhs, body) => "let binding"
      case Tup(x :: Nil) => x.describe
      case Tup(xs) => "tuple"
      case If(_, _) => "if-else block"
    }
  }
  
  override def toString: Str = print(false)
  
  def print(brackets: Bool): Str = {
      def bra(str: Str): Str = if (brackets) s"($str)" else str
      this match {
    case Blk(stmts) => stmts.mkString("{", "; ", "}")
    case IntLit(value) => value.toString
    case DecLit(value) => value.toString
    case StrLit(value) => '"'.toString + value + '"'
    case UnitLit(value) => if (value) "undefined" else "null"
    case Var(name) => name
    case Asc(trm, ty) => s"$trm : $ty"  |> bra
    case Lam(name, rhs) => s"$name => $rhs" |> bra
    case App(lhs, rhs) => s"${lhs.print(!lhs.isInstanceOf[App])} ${rhs.print(true)}" |> bra
    case Let(isRec, name, rhs, body) =>
      s"let${if (isRec) " rec" else ""} $name = $rhs in $body" |> bra
    case Tup(xs) =>
      xs.iterator.map(t => t + ",").mkString(" ") |> bra
    case If(cond, body) => s"if $cond" + body.mkString(" then ") |> bra
  }}
}

trait VarImpl { self: Var =>
  def isPatVar: Bool =
    name.head.isLetter && name.head.isLower && name =/= "true" && name =/= "false"
  var uid: Opt[Int] = N
}

trait SimpleTermImpl extends Ordered[SimpleTerm] { self: SimpleTerm =>
  def compare(that: SimpleTerm): Int = this.idStr compare that.idStr
  val idStr: Str = this match {
    case Var(name) => name
    case lit: Lit => lit.toString
  }
}

trait Located {
  def children: List[Located]
  
  private var spanStart: Int = -1
  private var spanEnd: Int = -1
  private var origin: Opt[Origin] = N
  
  // TODO just store the Loc directly...
  def withLoc(s: Int, e: Int, ori: Origin): this.type = {
    // assert(origin.isEmpty)
    origin = S(ori)
    // assert(spanStart < 0)
    // assert(spanEnd < 0)
    spanStart = s
    spanEnd = e
    this
  }
  def withLoc(loco: Opt[Loc]): this.type = {
    loco.foreach { that =>
      spanStart = that.spanStart
      spanEnd = that.spanEnd
      origin = S(that.origin)
    }
    this
  }
  def withLocOf(that: Located): this.type = withLoc(that.toLoc)
  def hasLoc: Bool = origin.isDefined
  lazy val toLoc: Opt[Loc] = getLoc
  private def getLoc: Opt[Loc] = {
    def subLocs = children.iterator.flatMap(_.toLoc.iterator)
    if (spanStart < 0) spanStart =
      subLocs.map(_.spanStart).minOption.getOrElse(return N)
    if (spanEnd < 0) spanEnd =
      subLocs.map(_.spanEnd).maxOption.getOrElse(return N)
    val origins = origin.fold(subLocs.map(_.origin).toList.distinct)(_ :: Nil)
    assert(origins.size === 1, origins)
    S(Loc(spanStart, spanEnd, origins.head))
  }
  /** Like toLoc, but we make sure the span includes the spans of all subterms. */
  def toCoveringLoc: Opt[Loc] = {
    // TODO factor logic with above
    def subLocs = (this :: children).iterator.flatMap(_.toLoc.iterator)
    val spanStart =
      subLocs.map(_.spanStart).minOption.getOrElse(return N)
    val spanEnd =
      subLocs.map(_.spanEnd).maxOption.getOrElse(return N)
    val origins = origin.fold(subLocs.map(_.origin).toList.distinct)(_ :: Nil)
    assert(origins.size === 1)
    S(Loc(spanStart, spanEnd, origins.head))
  }
}

trait StatementImpl extends Located { self: Statement =>
  def children: List[Located] = this match {
    case Var(name) => Nil
    case Asc(trm, ty) => trm :: Nil
    case Lam(lhs, rhs) => lhs :: rhs :: Nil
    case App(lhs, rhs) => lhs :: rhs :: Nil
    case Tup(fields) => fields
    case Let(isRec, name, rhs, body) => rhs :: body :: Nil
    case Blk(stmts) => stmts
    case _: Lit => Nil
    case d @ Def(_, n, b, _) => n :: d.body :: Nil
    case TypeDef(kind, nme, tparams, body, _) => nme :: tparams ::: body :: Nil
    case If(body, els) => body :: els
  }

  override def toString: Str = this match {
    case _: Term => super.toString
    case d: Decl => d.show
  }

  def describe: Str = this match {
    case decl: Decl => decl.describe
    case terms: Terms => terms.describe
  }
}

trait BlkImpl { self: Blk =>
  
  def flatten: Blk = Blk(stmts.flatMap {
    case b: Blk => b.flatten.stmts
    case t => t :: Nil
  })
  
}

trait IfBodyImpl extends Located { self: IfBody =>

  def children: List[Located] = this match {
    case IfThen(l, r) => l :: r :: Nil
    case IfElse(t) => t :: Nil
  }

  override def toString: String = this match {
    case IfThen(lhs, rhs) => s"($lhs) then $rhs"
    case IfElse(trm) => s"else $trm"
  }
}


// Pretty print AST
object PrettyPrintHelper {
  /**
    * Show how a term is actually structured.
    */
  def inspect(t: Statement): Str = t match {
    case Var(name)     => s"Var($name)"
    case Lam(lhs, rhs) => s"Lam(${inspect(lhs)}, ${inspect(rhs)})"
    case App(lhs, rhs) => s"App(${inspect(lhs)}, ${inspect(rhs)})"
    case Tup(fields) =>
      val entries = fields.map(inspect)
      s"Tup(${entries mkString ", "})"
    case Let(isRec, name, rhs, body) => s"Let($isRec, $name, ${inspect(rhs)}, ${inspect(body)})"
    case Blk(stmts)                  => s"Blk(...)"
    case Asc(trm, ty)                => s"Asc(${inspect(trm)}, $ty)"
    case IntLit(value)  => s"IntLit($value)"
    case DecLit(value)  => s"DecLit($value)"
    case StrLit(value)  => s"StrLit($value)"
    case UnitLit(value)  => s"UnitLit($value)"
    case If(bod, els) => s"If(${inspect(bod)}, ${els.map(inspect)})"
    case TypeDef(Cls, nme, tparams, tbody, adtData) => s"TypeDef($Cls, $nme, $tparams, $tbody) of adt: $adtData"
    case TypeDef(kind, nme, tparams, tbody, _) => s"TypeDef($kind, $nme, $tparams, $tbody)"
    case Def(rec, nme, rhs, isByname) =>
      s"Def($rec, $nme, ${rhs.fold(inspect, "" + _)}, $isByname)"
  }

  def inspect(pgrm: Pgrm): Str =
    pgrm.tops.map(inspect).mkString("\n")

  def inspect(body: IfBody): Str = body match {
    case IfElse(expr) => s"IfElse(${inspect(expr)}"
    case IfThen(expr, rhs) => s"IfThen(${inspect(expr)}, ${inspect(rhs)}"
  }
}
