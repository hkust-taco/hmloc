
:p
data L x
data R x
//│ Defined class L[+x]
//│ Defined class R[+x]
//│ L: 'a -> L['a]
//│ R: 'a -> R['a]

// TODO flow-type
:e
let f x = if x is L y then y else 0
//│ ╔══[ERROR] Unsupported pattern shape:
//│ ║  l.12: 	let f x = if x is L y then y else 0
//│ ╙──      	                  ^^^
//│ ╔══[ERROR] identifier not found: y
//│ ║  l.12: 	let f x = if x is L y then y else 0
//│ ╙──      	                           ^
//│ f: error -> (error | int)



// TODO
// true and false
// :e
// 1 and 2

