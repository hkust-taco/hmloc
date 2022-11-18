
:p
data type Boolean of Tru, Fals
//│ Defined type alias Boolean
//│ Defined class Tru
//│ Defined class Fals
//│ Tru: Tru
//│ Fals: Fals

:e
Boolean
//│ ╔══[ERROR] identifier not found: Boolean
//│ ║  l.11: 	Boolean
//│ ╙──      	^^^^^^^
//│ res: error


:p
:e
data type Bool2 of True2 & False2
//│ ╔══[ERROR] type identifier not found: True2
//│ ║  l.20: 	data type Bool2 of True2 & False2
//│ ╙──      	                   ^^^^^
//│ ╔══[ERROR] type identifier not found: False2
//│ ║  l.20: 	data type Bool2 of True2 & False2
//│ ╙──      	                           ^^^^^^
//│ ╔══[ERROR] Field identifiers must start with a small letter
//│ ║  l.20: 	data type Bool2 of True2 & False2
//│ ╙──      	                           ^^^^^^
//│ ╔══[ERROR] Field identifiers must start with a small letter
//│ ║  l.20: 	data type Bool2 of True2 & False2
//│ ╙──      	                   ^^^^^
//│ Defined type alias Bool2
//│ Defined class &[+True2, +False2]
//│ &: 'a -> 'b -> &['a, 'b]






data type Bool3 of
  True3; False3
//│ Defined type alias Bool3
//│ Defined class True3
//│ Defined class False3
//│ True3: True3
//│ False3: False3

data type Bool4 of
  True4
  False4
//│ Defined type alias Bool4
//│ Defined class True4
//│ Defined class False4
//│ True4: True4
//│ False4: False4

:e
Boolean
//│ ╔══[ERROR] identifier not found: Boolean
//│ ║  l.60: 	Boolean
//│ ╙──      	^^^^^^^
//│ res: error


Tru
//│ res: Tru

:e // TODO support types on RHS of `as`
Tru as Boolean
Tru : Boolean
//│ ╔══[ERROR] identifier not found: Boolean
//│ ║  l.71: 	Tru as Boolean
//│ ╙──      	       ^^^^^^^
//│ res: error
//│ ╔══[ERROR] identifier not found: Boolean
//│ ║  l.72: 	Tru : Boolean
//│ ╙──      	      ^^^^^^^
//│ res: (Tru: error,)



:e // Maybe we shouldn't interpret capitalized identifiers as field names...
Tru : Boolean
//│ ╔══[ERROR] identifier not found: Boolean
//│ ║  l.85: 	Tru : Boolean
//│ ╙──      	      ^^^^^^^
//│ res: (Tru: error,)


:pe
(Tru) : Boolean
//│ /!\ Parse error: Expected end-of-input:1:7, found ": Boolean\n" at l.93:7: (Tru) : Boolean


// TODO treat the ending curly-blocks as bodies (not params)?
// data type List of
//   Nil { T: Nothing }
//   Cons head tail { T: head | tail.T }

// TODO also try the one-line version:
// data type List of Nil { T: Nothing }, Cons head tail { T: head | tail.T }

:p
:w
data type List a of
  Nil
  Cons (head: a) (tail: List a)
//│ Defined type alias List[+a]
//│ Defined class Nil[±a]
//│ Defined class Cons[+a]
//│ ╔══[WARNING] Type definition Nil has bivariant type parameters:
//│ ║  l.108: 	  Nil
//│ ║         	  ^^^
//│ ╟── a is irrelevant and may be removed
//│ ║  l.107: 	data type List a of
//│ ╙──       	               ^
//│ Nil: Nil[?]
//│ Cons: (head: 'a,) -> (tail: List['a],) -> Cons['a]



// TODO interpret as free type variable?
:p
data type Ls of LsA a
//│ ╔══[ERROR] type identifier not found: a
//│ ║  l.126: 	data type Ls of LsA a
//│ ╙──       	                    ^
//│ Defined type alias Ls
//│ Defined class LsA[+a]
//│ LsA: 'a -> LsA['a]


:p
data type Ls2 of LsA2 `a
//│ Defined type alias Ls2
//│ Defined class LsA2
//│ LsA2: anything -> LsA2

Nil
Cons
Cons 1
Cons 2 Nil
Cons 1 (Cons 2 Nil)
//│ res: Nil[?]
//│ res: (head: 'a,) -> (tail: List['a],) -> Cons['a]
//│ res: (tail: List['a],) -> Cons[1 | 'a]
//│ res: Cons[2]
//│ res: Cons[1 | 2]

(Cons 3 Nil).head
succ (Cons 3 Nil).head
not (Cons false Nil).head
//│ res: 3
//│ res: int
//│ res: bool

:e
not (Cons 42 Nil).head
//│ ╔══[ERROR] Type mismatch in application:
//│ ║  l.160: 	not (Cons 42 Nil).head
//│ ║         	^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── integer literal of type `42` is not an instance of type `bool`
//│ ║  l.160: 	not (Cons 42 Nil).head
//│ ║         	          ^^
//│ ╟── but it flows into application with expected type `bool`
//│ ║  l.160: 	not (Cons 42 Nil).head
//│ ╙──       	     ^^^^^^^^^^^
//│ res: bool | error




:e
(Cons 4).head
//│ ╔══[ERROR] Type mismatch in field selection:
//│ ║  l.176: 	(Cons 4).head
//│ ║         	        ^^^^^
//│ ╟── type `(tail: List[?a],) -> Cons[?a]` does not have field 'head'
//│ ║  l.107: 	data type List a of
//│ ║         	               ^^^^
//│ ║  l.108: 	  Nil
//│ ║         	^^^^^
//│ ║  l.109: 	  Cons (head: a) (tail: List a)
//│ ║         	^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── but it flows into receiver with expected type `{head: ?head}`
//│ ║  l.176: 	(Cons 4).head
//│ ╙──       	^^^^^^^^
//│ res: error






:e
Cons 1 2
//│ ╔══[ERROR] Type mismatch in application:
//│ ║  l.198: 	Cons 1 2
//│ ║         	^^^^^^^^
//│ ╟── integer literal of type `2` is not a 1-element tuple
//│ ║  l.198: 	Cons 1 2
//│ ║         	       ^
//│ ╟── Note: constraint arises from tuple type:
//│ ║  l.109: 	  Cons (head: a) (tail: List a)
//│ ║         	                        ^^^^^^
//│ ╟── from union type:
//│ ║  l.107: 	data type List a of
//│ ╙──       	               ^
//│ res: Cons[1] | error





// TODO Allow method/field defintions in the same file (lose the let?):
:e
let List.head = () // ...
//│ ╔══[ERROR] Unsupported pattern shape
//│ ║  l.219: 	let List.head = () // ...
//│ ╙──       	        ^^^^^
//│ <error>: ()



