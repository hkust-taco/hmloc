// Overloading is not yet really supported...
// the simplifier thinks it's an impossible type!
let foo = _ as (_: (int => int) & (bool => bool))
//│ ╔══[WARNING] Variable name 'int' already names a symbol in scope. If you want to refer to that symbol, you can use `scope.int`; if not, give your future readers a break and use another name :^)
//│ ║  l.3: 	let foo = _ as (_: (int => int) & (bool => bool))
//│ ╙──     	                    ^^^
//│ ╔══[WARNING] Variable name 'bool' already names a symbol in scope. If you want to refer to that symbol, you can use `scope.bool`; if not, give your future readers a break and use another name :^)
//│ ║  l.3: 	let foo = _ as (_: (int => int) & (bool => bool))
//│ ╙──     	                                   ^^^^
//│ foo: (_: nothing,)

:ns
let foo = _ as (_: (Int => Int) & (Bool => Bool))
let foo = (_ as (_: (Int => Int) & (Bool => Bool)))._1
//│ ╔══[ERROR] identifier not found: Int
//│ ║  l.13: 	let foo = _ as (_: (Int => Int) & (Bool => Bool))
//│ ╙──      	                    ^^^
//│ ╔══[ERROR] identifier not found: Int
//│ ║  l.13: 	let foo = _ as (_: (Int => Int) & (Bool => Bool))
//│ ╙──      	                           ^^^
//│ ╔══[ERROR] identifier not found: Bool
//│ ║  l.13: 	let foo = _ as (_: (Int => Int) & (Bool => Bool))
//│ ╙──      	                                   ^^^^
//│ ╔══[ERROR] identifier not found: Bool
//│ ║  l.13: 	let foo = _ as (_: (Int => Int) & (Bool => Bool))
//│ ╙──      	                                           ^^^^
//│ foo: (_: 'a,)
//│   where
//│     'a <: error -> error
//│ ╔══[ERROR] identifier not found: Int
//│ ║  l.14: 	let foo = (_ as (_: (Int => Int) & (Bool => Bool)))._1
//│ ╙──      	                     ^^^
//│ ╔══[ERROR] identifier not found: Int
//│ ║  l.14: 	let foo = (_ as (_: (Int => Int) & (Bool => Bool)))._1
//│ ╙──      	                            ^^^
//│ ╔══[ERROR] identifier not found: Bool
//│ ║  l.14: 	let foo = (_ as (_: (Int => Int) & (Bool => Bool)))._1
//│ ╙──      	                                    ^^^^
//│ ╔══[ERROR] identifier not found: Bool
//│ ║  l.14: 	let foo = (_ as (_: (Int => Int) & (Bool => Bool)))._1
//│ ╙──      	                                            ^^^^
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
//│ ║  l.55: 	let foo = (Int => Int) & (Bool => Bool)
//│ ╙──      	           ^^^
//│ ╔══[ERROR] identifier not found: Int
//│ ║  l.55: 	let foo = (Int => Int) & (Bool => Bool)
//│ ╙──      	                  ^^^
//│ ╔══[ERROR] identifier not found: Bool
//│ ║  l.55: 	let foo = (Int => Int) & (Bool => Bool)
//│ ╙──      	                          ^^^^
//│ ╔══[ERROR] identifier not found: Bool
//│ ║  l.55: 	let foo = (Int => Int) & (Bool => Bool)
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
//│ ║  l.80: 	foo(1) as Nothing
//│ ╙──      	          ^^^^^^^
//│ res: error

:e
foo as Nothing
//│ ╔══[ERROR] identifier not found: Nothing
//│ ║  l.88: 	foo as Nothing
//│ ╙──      	       ^^^^^^^
//│ res: error





:e
let oops = (&)
//│ ╔══[ERROR] Illegal use of operator: &
//│ ║  l.99: 	let oops = (&)
//│ ╙──      	           ^^^
//│ ╔══[ERROR] identifier not found: &
//│ ║  l.99: 	let oops = (&)
//│ ╙──      	           ^^^
//│ oops: error



