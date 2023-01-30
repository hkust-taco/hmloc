:p
data Test a b
//│ Defined class Test[+a, +b]
//│ Test: 'a -> 'b -> Test['a, 'b]

:p
data Person(name: string, age: int)
//│ Defined class Person
//│ Person: (name: string, age: int,) -> Person

let p = Person("Bob", 42)
//│ p: Person

let foo q = q.age
foo p
//│ foo: {age: 'age} -> 'age
//│ res: int

// TODO properly check pattern types!
let bar (q: Person _) = q.age
//│ bar: (q: {age: 'age},) -> 'age

bar p
//│ res: int

:e
bar {}
bar {name: "Bob"}
bar {age: 1} // TODO B/E
//│ ╔══[ERROR] Type mismatch in application:
//│ ║  l.27: 	bar {}
//│ ║        	^^^^^^
//│ ╟── tuple of type `anything` does not have field 'age'
//│ ║  l.27: 	bar {}
//│ ║        	    ^^
//│ ╟── Note: constraint arises from binding:
//│ ║  l.20: 	let bar (q: Person _) = q.age
//│ ║        	         ^^^^^^^^^^^
//│ ╟── from receiver:
//│ ║  l.20: 	let bar (q: Person _) = q.age
//│ ╙──      	                        ^
//│ res: nothing
//│ ╔══[ERROR] Type mismatch in application:
//│ ║  l.28: 	bar {name: "Bob"}
//│ ║        	^^^^^^^^^^^^^^^^^
//│ ╟── record of type `{name: string}` does not have field 'age'
//│ ║  l.28: 	bar {name: "Bob"}
//│ ║        	    ^^^^^^^^^^^^^
//│ ╟── Note: constraint arises from binding:
//│ ║  l.20: 	let bar (q: Person _) = q.age
//│ ║        	         ^^^^^^^^^^^
//│ ╟── from receiver:
//│ ║  l.20: 	let bar (q: Person _) = q.age
//│ ╙──      	                        ^
//│ res: nothing
//│ res: int









let fake-p = { name: "Bob", age: 42 }
//│ fake-p: {age: int, name: string}

// :e // TODO B/E
bar fake-p
//│ res: int

data Wine(name: string, age: int)
let w = Wine("Côtes du Rhône", 3)
//│ Defined class Wine
//│ Wine: (name: string, age: int,) -> Wine
//│ w: Wine

// :e
bar w
bar (q: w)
//│ res: int
//│ res: int

let bar2 (q: Person _) = succ q.age
//│ bar2: (q: {age: int},) -> int


:e
let nested x =
  data Foo a // Note: we get one error for the synthetic class, and one for the synthetic def...
  Foo x
//│ ╔══[ERROR] Illegal position for this type declaration statement.
//│ ║  l.91: 	  data Foo a // Note: we get one error for the synthetic class, and one for the synthetic def...
//│ ╙──      	       ^^^^^
//│ ╔══[ERROR] Illegal position for this definition statement.
//│ ║  l.91: 	  data Foo a // Note: we get one error for the synthetic class, and one for the synthetic def...
//│ ╙──      	       ^^^^^
//│ ╔══[ERROR] identifier not found: Foo
//│ ║  l.92: 	  Foo x
//│ ╙──      	  ^^^
//│ nested: error -> error




