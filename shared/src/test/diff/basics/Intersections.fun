// Overloading is not yet really supported...
// the simplifier thinks it's an impossible type!
let foo = _ as (_: (int => int) & (bool => bool))
//│ foo: (_: nothing,)

:ns
let foo = _ as (_: (Int => Int) & (Bool => Bool))
let foo = (_ as (_: (Int => Int) & (Bool => Bool)))._1
//│ ╔══[ERROR] identifier not found: Int
//│ ║  l.7: 	let foo = _ as (_: (Int => Int) & (Bool => Bool))
//│ ╙──     	                    ^^^
//│ ╔══[ERROR] identifier not found: Int
//│ ║  l.7: 	let foo = _ as (_: (Int => Int) & (Bool => Bool))
//│ ╙──     	                           ^^^
//│ ╔══[ERROR] identifier not found: Bool
//│ ║  l.7: 	let foo = _ as (_: (Int => Int) & (Bool => Bool))
//│ ╙──     	                                   ^^^^
//│ ╔══[ERROR] identifier not found: Bool
//│ ║  l.7: 	let foo = _ as (_: (Int => Int) & (Bool => Bool))
//│ ╙──     	                                           ^^^^
//│ foo: (_: 'a,)
//│   where
//│     'a <: error -> error
//│ ╔══[ERROR] identifier not found: Int
//│ ║  l.8: 	let foo = (_ as (_: (Int => Int) & (Bool => Bool)))._1
//│ ╙──     	                     ^^^
//│ ╔══[ERROR] identifier not found: Int
//│ ║  l.8: 	let foo = (_ as (_: (Int => Int) & (Bool => Bool)))._1
//│ ╙──     	                            ^^^
//│ ╔══[ERROR] identifier not found: Bool
//│ ║  l.8: 	let foo = (_ as (_: (Int => Int) & (Bool => Bool)))._1
//│ ╙──     	                                    ^^^^
//│ ╔══[ERROR] identifier not found: Bool
//│ ║  l.8: 	let foo = (_ as (_: (Int => Int) & (Bool => Bool)))._1
//│ ╙──     	                                            ^^^^
//│ foo: 'a

foo(1)
//│ res: nothing

:ns
foo(1)
//│ res: 'a

succ / foo(1)
//│ res: int

// Intersection-based overloading is not actually supported... a value of this type is impossible to provide:
let foo = (Int => Int) & (Bool => Bool)
//│ ╔══[ERROR] identifier not found: Int
//│ ║  l.49: 	let foo = (Int => Int) & (Bool => Bool)
//│ ╙──      	           ^^^
//│ ╔══[ERROR] identifier not found: Int
//│ ║  l.49: 	let foo = (Int => Int) & (Bool => Bool)
//│ ╙──      	                  ^^^
//│ ╔══[ERROR] identifier not found: Bool
//│ ║  l.49: 	let foo = (Int => Int) & (Bool => Bool)
//│ ╙──      	                          ^^^^
//│ ╔══[ERROR] identifier not found: Bool
//│ ║  l.49: 	let foo = (Int => Int) & (Bool => Bool)
//│ ╙──      	                                  ^^^^
//│ foo: error -> error

foo(1) // returns int & bool, equivalent to nothing
succ / foo(1)
foo(true)
not / foo(true)
//│ res: error
//│ res: int
//│ res: error
//│ res: bool

not / foo(1)
foo(1) as Nothing
//│ res: bool
//│ ╔══[ERROR] identifier not found: Nothing
//│ ║  l.74: 	foo(1) as Nothing
//│ ╙──      	          ^^^^^^^
//│ res: error

:e
foo as Nothing
//│ ╔══[ERROR] identifier not found: Nothing
//│ ║  l.82: 	foo as Nothing
//│ ╙──      	       ^^^^^^^
//│ res: error





:e
let oops = (&)
//│ ╔══[ERROR] Illegal use of operator: &
//│ ║  l.93: 	let oops = (&)
//│ ╙──      	           ^^^
//│ ╔══[ERROR] identifier not found: &
//│ ║  l.93: 	let oops = (&)
//│ ╙──      	           ^^^
//│ oops: error



