
succ / 1
succ / succ / 1
//│ res: int
//│ res: int

let foo = f => f 1
//│ foo: (int -> 'a) -> 'a

foo / x => x
//│ res: int

foo / x => succ x
//│ res: int

x => succ / x + 1
//│ res: int -> int

x => succ / succ / x + 1
//│ res: int -> int

:p
foo / x => succ / succ / x
//│ res: int

:e
foo / foo / x => succ / succ / x
//│ ╔══[ERROR] Type mismatch in application:
//│ ║  l.27: 	foo / foo / x => succ / succ / x
//│ ║        	^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── application of type `int` is not a function
//│ ║  l.27: 	foo / foo / x => succ / succ / x
//│ ║        	                 ^^^^^^^^^^^^^^^
//│ ╟── but it flows into application with expected type `int -> ?a`
//│ ║  l.27: 	foo / foo / x => succ / succ / x
//│ ║        	      ^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── Note: constraint arises from reference:
//│ ║  l.7: 	let foo = f => f 1
//│ ╙──     	               ^
//│ res: nothing






:e
foo / foo
//│ ╔══[ERROR] Type mismatch in application:
//│ ║  l.48: 	foo / foo
//│ ║        	^^^^^^^^^
//│ ╟── integer literal of type `int` is not a function
//│ ║  l.7: 	let foo = f => f 1
//│ ║       	                 ^
//│ ╟── Note: constraint arises from reference:
//│ ║  l.7: 	let foo = f => f 1
//│ ╙──     	               ^
//│ res: nothing




