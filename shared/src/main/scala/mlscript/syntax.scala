package mlscript

import mlscript.utils._, shorthands._


// Terms

final case class Pgrm(tops: Ls[Statement]) extends PgrmOrTypingUnit with PgrmImpl

sealed abstract class Decl extends DesugaredStatement with DeclImpl
/** A Def is any function declaration in the global scope
  *
  * @param rec
  *   If the defintion is recursive
  * @param nme
  *   the name the body of the defintion is bound
  * @param rhs
  *   Is a term if it's a definition and a type if it's a declaration
  * @param isByname
  *   If this defintion is call by name or call by value
  */
final case class Def(rec: Bool, nme: Var, rhs: Term \/ PolyType, isByname: Bool) extends Decl with Terms { val body: Located = rhs.fold(identity, identity)
}

final case class TypeDef(
  kind: TypeDefKind,
  nme: TypeName,
  tparams: List[TypeName],
  body: Type,
  positionals: Ls[Var],
  // maps a class to it's adt by name and maps params to adt param by position
  // for e.g. in type 'a, 'b either = Left of 'a | Right of 'b
  // Right will have an adtData = S((TypeName("either"), List(1)))
  // indicating that it's adt is either and it's param is the 1th param of either
  adtData: Opt[(TypeName, Ls[Int])] = N
) extends Decl

sealed abstract class TypeDefKind(val str: Str)
sealed trait ObjDefKind
case object Cls extends TypeDefKind("class") with ObjDefKind
case object Als extends TypeDefKind("type alias")

sealed abstract class Term                                           extends Terms with TermImpl
sealed abstract class Lit                                            extends SimpleTerm with LitImpl
final case class Var(name: Str)                                      extends SimpleTerm with VarImpl
final case class Lam(lhs: Term, rhs: Term)                           extends Term
final case class App(lhs: Term, rhs: Term)                           extends Term
case class Tup(fields: Ls[Term]) extends Term
final case class Rcd(fields: Ls[Var -> Term])                         extends Term
final case class Sel(receiver: Term, fieldName: Var)                 extends Term
final case class Let(isRec: Bool, name: Var, rhs: Term, body: Term)  extends Term
/** A block of statements that are parsed and type checked together */
final case class Blk(stmts: Ls[Statement])                           extends Term with BlkImpl
/** Braces or brackets that encloses a term
  * @param rcd set this flag if this term is a record, otherwise it's a tuple
  * @param trm
  */
/** A term is optionally ascribed with a type as in: term: ty */
final case class Asc(trm: Term, ty: Type)                            extends Term
final case class Bind(lhs: Term, rhs: Term)                          extends Term
final case class With(trm: Term, fields: Rcd)                        extends Term
final case class Assign(lhs: Term, rhs: Term)                        extends Term
final case class If(lhs: Term, rhs: Ls[IfBody])                    extends Term with IfImpl

sealed abstract class IfBody extends IfBodyImpl
final case class IfThen(expr: Term, rhs: Term) extends IfBody
final case class IfElse(expr: Term) extends IfBody

final case class IntLit(value: BigInt)            extends Lit
final case class DecLit(value: BigDecimal)        extends Lit
final case class StrLit(value: Str)               extends Lit
final case class UnitLit(undefinedOrNull: Bool)   extends Lit

sealed abstract class SimpleTerm extends Term with SimpleTermImpl

sealed trait Statement extends StatementImpl
final case class LetS(isRec: Bool, pat: Term, rhs: Term)  extends Statement
final case class DataDefn(body: Term)                     extends Statement
final case class DatatypeDefn(head: Term, body: Term)     extends Statement

sealed trait DesugaredStatement extends Statement with DesugaredStatementImpl

sealed trait Terms extends DesugaredStatement


// Types

sealed abstract class Type extends TypeImpl

sealed trait NamedType extends Type { val base: TypeName }

sealed abstract class Composed(val pol: Bool) extends Type with ComposedImpl

final case class Union(lhs: Type, rhs: Type)             extends Composed(true)
final case class Inter(lhs: Type, rhs: Type)             extends Composed(false)
final case class Function(lhs: Type, rhs: Type)          extends Type
final case class Record(fields: Ls[Var -> Type])        extends Type
case class Tuple(fields: Ls[Type])    extends Type
final case class Recursive(uv: TypeVar, body: Type)      extends Type
final case class AppliedType(base: TypeName, targs: List[Type]) extends Type with NamedType
final case class Bounds(lb: Type, ub: Type)              extends Type
final case class Constrained(base: Type, where: Ls[TypeVar -> Bounds]) extends Type
final case class Unified(base: Type, where: Ls[TypeVar -> Ls[Type]]) extends Type

sealed abstract class NullaryType                        extends Type

case object Top                                          extends NullaryType
case object Bot                                          extends NullaryType

/** Literal type type, e.g. type `0` is a type with only one possible value `0`. */
final case class Literal(lit: Lit)                       extends NullaryType

/** Reference to an existing type with the given name. */
final case class TypeName(name: Str)                     extends NullaryType with NamedType with TypeNameImpl
final case class TypeTag (name: Str)                     extends NullaryType

final case class TypeVar(val identifier: Int \/ Str, nameHint: Opt[Str]) extends NullaryType with TypeVarImpl {
  require(nameHint.isEmpty || identifier.isLeft)
  // ^ The better data structure to represent this would be an EitherOrBoth
  override def toString: Str = identifier.fold("α" + _, identity)
}

final case class PolyType(targs: Ls[TypeName], body: Type) extends PolyTypeImpl


// New Definitions AST

final case class TypingUnit(entities: Ls[Statement]) extends PgrmOrTypingUnit with TypingUnitImpl

sealed abstract class PgrmOrTypingUnit
