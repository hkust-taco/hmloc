
1
//│ res: int

"hello"
//│ res: string

// TODO literal booleans
true
//│ res: true

1 as Int
"hello" as String
true as Bool
//│ ╔══[ERROR] identifier not found: Int
//│ ║  l.12: 	1 as Int
//│ ╙──      	     ^^^
//│ res: error
//│ ╔══[ERROR] identifier not found: String
//│ ║  l.13: 	"hello" as String
//│ ╙──      	           ^^^^^^
//│ res: error
//│ ╔══[ERROR] identifier not found: Bool
//│ ║  l.14: 	true as Bool
//│ ╙──      	        ^^^^
//│ res: error

:w
1 as int
"hello" as string
//│ ╔══[WARNING] Variable name 'int' already names a symbol in scope. If you want to refer to that symbol, you can use `scope.int`; if not, give your future readers a break and use another name :^)
//│ ║  l.29: 	1 as int
//│ ╙──      	     ^^^
//│ res: int
//│ ╔══[WARNING] Variable name 'string' already names a symbol in scope. If you want to refer to that symbol, you can use `scope.string`; if not, give your future readers a break and use another name :^)
//│ ║  l.30: 	"hello" as string
//│ ╙──      	           ^^^^^^
//│ res: string



1 as (_: int)
"hello" as (_: string)
//│ res: (_: int,)
//│ res: (_: string,)

:e
1 as true
true as Int
false as 1
//│ ╔══[ERROR] Type mismatch in 'as' binding:
//│ ║  l.48: 	1 as true
//│ ║        	^^^^^^^^^
//│ ╟── integer literal of type `int` is not an instance of type `true`
//│ ║  l.48: 	1 as true
//│ ║        	^
//│ ╟── Note: constraint arises from reference:
//│ ║  l.48: 	1 as true
//│ ╙──      	     ^^^^
//│ res: true
//│ ╔══[ERROR] identifier not found: Int
//│ ║  l.49: 	true as Int
//│ ╙──      	        ^^^
//│ res: error
//│ ╔══[ERROR] Type mismatch in 'as' binding:
//│ ║  l.50: 	false as 1
//│ ║        	^^^^^^^^^^
//│ ╟── reference of type `false` is not an instance of `int`
//│ ║  l.50: 	false as 1
//│ ║        	^^^^^
//│ ╟── Note: constraint arises from integer literal:
//│ ║  l.50: 	false as 1
//│ ╙──      	         ^
//│ res: int










let f = b => if b then 0 else 1
//│ f: bool -> int

let pred = n => 0 < n
//│ pred: int -> bool

let g = x => if pred x then x else f false
//│ g: int -> int

g 3
//│ res: int

g / succ 3
//│ res: int

x => if x then x else f false
//│ res: (bool & 'a) -> (int | 'a)

res false
//│ res: false | int

let rec f = n =>
  if pred n then n else f (n + 1)
//│ f: int -> int

let g = n =>
  if pred n then 0 else if not (pred n) then 1 else f n
//│ g: int -> int

x => if pred x then x else f x
//│ res: int -> int

:e
f false
//│ ╔══[ERROR] Type mismatch in application:
//│ ║  l.118: 	f false
//│ ║         	^^^^^^^
//│ ╟── reference of type `false` is not an instance of type `int`
//│ ║  l.118: 	f false
//│ ║         	  ^^^^^
//│ ╟── Note: constraint arises from reference:
//│ ║  l.107: 	  if pred n then n else f (n + 1)
//│ ╙──       	                           ^
//│ res: false | int




let take0 (x: 0) = 0
let take1 (x: 1) = 1
//│ take0: (x: int,) -> int
//│ take1: (x: int,) -> int

let takeWhat y = if y < 0 then take0 y else take1 y
//│ takeWhat: nothing -> int

let takeWhat y = if y < 0 then take0 (x: y) else take1 (x: y)
//│ takeWhat: int -> int

