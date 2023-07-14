# Documentation of the HM<sup>ℓ</sup> type system implementation

## Overview

The codebase of HM<sup>ℓ</sup> has all basic components
of a static type checker: lexer, parser, typer (but no code generation).
For testing, there is a web demo of HMloc as well as a test suite.
We now give a high-level introduction to each compiler component and its correspondence to
our Scala sources. Note that source file paths are rooted in `/shared/src/main/scala/hmloc`.

## Lexing

Class `Lexer` in `Lexer.scala` is the lexer class. It takes an `origin` object,
which contains the original source string together with the source file name,
the number of the first line, and some helper functions. Scala native types are
used as token types.

### Parsing

Class `OcamlParser` in `OcamlParser.scala` is the parser class. It uses parser
combinators functions to parse a subset of OCaml syntax. It consumes the source
and produces syntax data types. Method `pgrm` splits the souce by the separator
into blocks that parsed by `toplvl` method. It allows type declarations,
function declarations or other terms.

File `syntax.scala` contains surface syntax data types of HMloc
which are *immutable*, different from internal representations in the
typer for later type inference.  Here we introduce several surface syntax
data types:

- Class `TypeDef` defines type and data constructors. `TypeDefKind` is the kind of the
  declaration, a `Cls` for data constructor, `Als` for type constructor.
- Class `Def` defines top level functions
- Class `Term` includes HMloc term data types.
- Class `IfBody` defines terms for if-then-else and match-with.
- Class `Type` defines surface level types.

## Typing

The typer accepts the abstract syntax tree of a program
and performs type inference. It tracks source locations of the program terms.
It wraps the inferred type with the provenance that contains the source location
of the corresponding terms.
For more information about the type system, see the paper.

The corresponding files are:
- `Typer.scala` contains the main typer class.
- `TyperDatatypes.scala` contains class `TyperDatatypes` which includes data types
  to support **internal** representation of types with mutable states to support
- `TypeDefs.scala` contains class `TypeDefs` and methods for declaring data types.
- `TypeSimplifier.scala` contains type simplification algorithms to simplify inferred types.
- `TyperHelpers.scala` contains class `TyperHelpers` that provides helper methods
  for the typer.

The typer (class `Typer`) works with a typing context (class `Ctx`) which
mainly maintains all global and local bindings of names to their types.
The typer accepts a type (surface level), a top level definition, or type constructor
definitions or terms. It types the term/statement and returns the resulting type.
All the typing errors are collected and reported at once. Any new bindings
and definitions are added to the context. The resulting type
is sent to the type simplifier and is finally expanded, i.e., converted
back to types in the surface syntax for presentation.
The `Typer` also defines builtin bindings and types.

`TyperDatatypes.scala` defines internal representation of types.

- Class `PolymorphicType` represents a type with universally quantified type variables.
  By convention, in the type body, type variables of levels greater than
  the polymorphic type's level are polymorphic.
- Class `SimpleType` is a general type form of all types.
  It has a field `level` for level-based polymorphism.
  - Class `FunctionType` is a function type.
  - Class `TupleType` is a tuple type. The tuple's fields are a list of types.
  - Class `ProvType` is a derived type form to store more type provenance information.
  - Class `TypeRef` is a reference to named types - builtin or user-defined
    It has a list of type arguments, for parametric type definitions.
  - Class `TypeVariable` represents a type variable, which is unified with other types
    for type inference. It has mutable state to store types it's been unified with.
  - Class `RigidTypeVariable` is used when unifying type signatures with inferred
    types. They can only be unified with themselves.
- Class `TypeProvenance` stores the source code location where a type is introduced.

It has two methods for typing -
* `typeTerm` accepts a term and types it. It also tracks the source code location
  of the term using type provenance. When two inferred types need to be unified
  it generates a data flow constraint and passes it on to the `UnificationSolver`
* `typeType` accepts a surface level type and converts it into an internal type
  representation.

`TypeDefs.scala` defines the internal representation of classes and aliases. It
defines the `processTypeDefs` method which type checks new type definitions and
adds them to the `Ctx`.

## Unification

`UnificationSolver.scala` defines the `UnificationSolver` class which unifies types.
As described in the corresponding paper, it tracks the provenances of the unified
types.

Unification is particularly interesting because it tracks the data flow through
source code locations.

- class `Unification` stores a sequence of data flows that shows how two types are unified
- abstract class `DataFlow` shows how two types are unified
  - class `Constraint` is the smallest data flow and shows two types unified together
  - class `Constructor` is a data flow that unifies the arguments of a constructor type
    tuple type, function type, record types etc.

- method `unify` unifies two types and propagates any newly generated constraints
- method `createErrorMessage` creates the textual representation of the error if
  two types cannot be unified.

Unification logic can be broadly described as follows -
- A type can be unified with a type variable with a `Constraint`. The unification
  is recorded in with type variable. The type is unified with all other types
  the type variable has been unified with.
- Two primitives can be unified with a `Constraint` if they are equal otherwise it's an error
- Two constructor types can be unified with a `Constraint` and their type arguments
  can be unified with a `Constructor` data flow, if they are equal otherwise it's an error
