
let t3 = 1, 2, 3
let t3 = 1, 2, 3,
let t2 = 1, 2,
let t1 = 1,
let t0 = ()
//│ t3: (int, int, int,)
//│ t3: (int, int, int,)
//│ t2: (int, int,)
//│ t1: (int,)
//│ t0: ()

let t = 1, y: 2, 3
let t = x: 1, y: 2, z: 3
//│ t: (int, y: int, int,)
//│ t: (x: int, y: int, z: int,)

(1, true, "hey")._2
(1, true, "hey")._3
//│ res: bool
//│ res: string

:e
(1, true, "hey")._4
//│ ╔══[ERROR] Type mismatch in field selection:
//│ ║  l.24: 	(1, true, "hey")._4
//│ ║        	                ^^^
//│ ╟── tuple of type `(int, bool, string,)` does not have field '_4'
//│ ║  l.24: 	(1, true, "hey")._4
//│ ║        	 ^^^^^^^^^^^^^^
//│ ╟── but it flows into receiver with expected type `{_4: ?a}`
//│ ║  l.24: 	(1, true, "hey")._4
//│ ╙──      	^^^^^^^^^^^^^^^^
//│ res: nothing




:p
:e
(1, true, "hey").2
//│ ╔══[ERROR] Type mismatch in application:
//│ ║  l.41: 	(1, true, "hey").2
//│ ║        	^^^^^^^^^^^^^^^^^^
//│ ╟── tuple of type `(int, bool, string,)` is not a function
//│ ║  l.41: 	(1, true, "hey").2
//│ ╙──      	 ^^^^^^^^^^^^^^
//│ res: nothing




:w
let not-tup = (
  1
  2
)
//│ ╔══[WARNING] Pure expression does nothing in statement position.
//│ ║  l.55: 	  1
//│ ╙──      	  ^
//│ not-tup: int


:w
let tup = (
  1,
  2
)
//│ ╔══[WARNING] Previous field definitions are discarded by this returned expression.
//│ ║  l.67: 	  2
//│ ╙──      	  ^
//│ tup: int


:w
let tup =
  1,
  2,
  3
//│ ╔══[WARNING] Previous field definitions are discarded by this returned expression.
//│ ║  l.79: 	  3
//│ ╙──      	  ^
//│ tup: int


let tup =
  1,
  2,
  3,
//│ tup: (int, int, int,)

