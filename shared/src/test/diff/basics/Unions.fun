

let f(x: 0 | 1) = x
//│ f: (x: int & 'a,) -> 'a

let f(x: 0 | 1) = succ x
//│ f: (x: int,) -> int

let f(x) = x as 0 | 1
//│ f: int -> int

f 1
f 0
f(0 as 0 | 1)
//│ res: int
//│ res: int
//│ res: int

:e
f 3
f (0 as 1 | 3)
f (0 as 0 | 3)
f (0 as 3 | 4)
f (0 as Int)
//│ res: int
//│ res: int
//│ res: int
//│ res: int
//│ ╔══[ERROR] identifier not found: Int
//│ ║  l.24: 	f (0 as Int)
//│ ╙──      	        ^^^
//│ res: int































let g(x: int) = succ x
g 0
g (0 as 0 | 1)
let h = y => g(y as 0 | 1)
h(0)
//│ g: (x: int,) -> int
//│ res: int
//│ res: int
//│ h: int -> int
//│ res: int

let foo(r: { v: 0 } | { v: 1 }) = if r.v < 1 then r.v else 2
//│ foo: (r: {v: int},) -> int

foo({ v: 0 })
foo({ v: 1 })
//│ res: int
//│ res: int

x => foo(x)
//│ res: (r: {v: int},) -> int

x => foo { v: x }
//│ res: int -> int


// Notice that in MLscript, `(0, 0) | (1, 1)` is equivalent to `(0 | 1, 0 | 1)`
let bar(r: (0, 0) | (1, 1)) = if r._1 < 1 then r._1 else r._2
//│ bar: (r: (int & 'a, int & 'a,),) -> 'a

bar(0, 1)
//│ res: int

:e
bar(2, 2)
//│ res: int



bar(0, 0)
bar(1, 1)
bar(0, _)
bar(_, 1)
//│ res: int
//│ res: int
//│ res: int
//│ res: int

let f x = bar(x, x)
//│ f: (int & 'a) -> 'a

f 0
f 1
//│ res: int
//│ res: int

:e
f 2
//│ res: int




x => bar(1, x)
x => bar(x, 0)
//│ res: int -> int
//│ res: int -> int

bar(_, _)
(x, y) => bar(x, y)
//│ res: nothing
//│ res: (int & 'a, int & 'a,) -> 'a

// ^ TODO allow explicit request for inferring an overloaded type in case of ambiguities

x => bar(bar(0, x), 0)
x => bar(bar(x, x), 0)
x => bar(bar(0, x), x)
x => bar(bar(x, x), 0)
//│ res: int -> int
//│ res: int -> int
//│ res: int -> int
//│ res: int -> int

x => bar(bar(x, 1), 0)
(x, y) => bar(bar(x, y), x)
//│ res: int -> int
//│ res: (int & 'a, int & 'a,) -> 'a

(x, y) => bar(bar(x, y), 0)
//│ res: (int, int,) -> int


let baz(r: (0, 0) | _) = if r._1 < 1 then r._1 else r._2
//│ baz: (r: {_1: int & 'a, _2: 'a},) -> 'a

:e
baz(0)
//│ ╔══[ERROR] Type mismatch in application:
//│ ║  l.161: 	baz(0)
//│ ║         	^^^^^^
//│ ╟── integer literal of type `int` does not have field '_2'
//│ ║  l.161: 	baz(0)
//│ ║         	    ^
//│ ╟── but it flows into argument with expected type `{_2: ?a}`
//│ ║  l.161: 	baz(0)
//│ ║         	   ^^^
//│ ╟── Note: constraint arises from binding:
//│ ║  l.157: 	let baz(r: (0, 0) | _) = if r._1 < 1 then r._1 else r._2
//│ ║         	        ^^^^^^^^^^^^^
//│ ╟── from receiver:
//│ ║  l.157: 	let baz(r: (0, 0) | _) = if r._1 < 1 then r._1 else r._2
//│ ╙──       	                                                    ^
//│ res: nothing






baz(0, 0)
baz(0, 1)
baz(1, 1)
//│ res: int
//│ res: int
//│ res: int

x => baz(x, 0)
x => baz(0, x)
x => baz(x, x)
(x, y) => baz(x, y)
//│ res: int -> int
//│ res: 'a -> (int | 'a)
//│ res: (int & 'a) -> 'a
//│ res: (int & 'a, 'a,) -> 'a


let baz(r: (0, 0) | (1, _)) = if r._1 < 1 then r._1 else r._2
//│ baz: (r: (int & 'a, 'a,),) -> 'a

:e
baz(0)
baz(0, 1)
//│ ╔══[ERROR] Type mismatch in application:
//│ ║  l.205: 	baz(0)
//│ ║         	^^^^^^
//│ ╟── integer literal of type `int` does not have field '_2'
//│ ║  l.205: 	baz(0)
//│ ║         	    ^
//│ ╟── but it flows into argument with expected type `{_2: ?a}`
//│ ║  l.205: 	baz(0)
//│ ║         	   ^^^
//│ ╟── Note: constraint arises from binding:
//│ ║  l.201: 	let baz(r: (0, 0) | (1, _)) = if r._1 < 1 then r._1 else r._2
//│ ║         	        ^^^^^^^^^^^^^^^^^^
//│ ╟── from receiver:
//│ ║  l.201: 	let baz(r: (0, 0) | (1, _)) = if r._1 < 1 then r._1 else r._2
//│ ╙──       	                                                         ^
//│ res: nothing
//│ res: int






baz(0, 0)
baz(1, 1)
x => baz(0, x)
x => baz(1, x)
x => baz(x, 1)
//│ res: int
//│ res: int
//│ res: 'a -> (int | 'a)
//│ res: 'a -> (int | 'a)
//│ res: int -> int

x => baz(x, 0)
x => baz(x, x)
(x, y) => baz(x, y)
//│ res: int -> int
//│ res: (int & 'a) -> 'a
//│ res: (int & 'a, 'a,) -> 'a

