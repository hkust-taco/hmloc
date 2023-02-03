
data Wine
//│ Defined class Wine
//│ Wine: Wine


let jesus = neg Wine => Wine
//│ jesus: ~Wine -> Wine

let w = jesus(water: "Evian")
//│ w: Wine


let jesus = (water: neg Wine) => Wine
//│ jesus: (water: ~Wine,) -> Wine

let w = jesus(water: "Evian")
//│ w: Wine

:e
jesus w
jesus(water: w)
//│ ╔══[ERROR] Type mismatch in application:
//│ ║  l.21: 	jesus w
//│ ║        	^^^^^^^
//│ ╟── reference of type `Wine` does not match type `~Wine`
//│ ║  l.14: 	let jesus = (water: neg Wine) => Wine
//│ ║        	                                 ^^^^
//│ ╟── but it flows into reference with expected type `~Wine`
//│ ║  l.21: 	jesus w
//│ ║        	      ^
//│ ╟── Note: constraint arises from binding:
//│ ║  l.14: 	let jesus = (water: neg Wine) => Wine
//│ ║        	             ^^^^^^^^^^^^^^^
//│ ╟── from application:
//│ ║  l.14: 	let jesus = (water: neg Wine) => Wine
//│ ╙──      	                    ^^^^^^^^
//│ res: Wine
//│ ╔══[ERROR] Type mismatch in application:
//│ ║  l.22: 	jesus(water: w)
//│ ║        	^^^^^^^^^^^^^^^
//│ ╟── reference of type `Wine` does not match type `~Wine`
//│ ║  l.14: 	let jesus = (water: neg Wine) => Wine
//│ ║        	                                 ^^^^
//│ ╟── but it flows into reference with expected type `~Wine`
//│ ║  l.22: 	jesus(water: w)
//│ ║        	             ^
//│ ╟── Note: constraint arises from parameter type:
//│ ║  l.14: 	let jesus = (water: neg Wine) => Wine
//│ ╙──      	                    ^^^^^^^^
//│ res: Wine











(0 | 1) & neg 0
//│ res: nothing

(0 | 1) & neg 0 as 1
//│ res: int

:e
(0 | 1) & neg 0 as 0
//│ res: int




(0 | 1) & neg 0 & neg 1 as "wat"
//│ res: string

:e
neg 0 as 1
//│ ╔══[ERROR] Type mismatch in 'as' binding:
//│ ║  l.80: 	neg 0 as 1
//│ ║        	^^^^^^^^^^
//│ ╟── application of type `~int` is not an instance of `int`
//│ ║  l.80: 	neg 0 as 1
//│ ║        	^^^^^
//│ ╟── Note: constraint arises from integer literal:
//│ ║  l.80: 	neg 0 as 1
//│ ╙──      	         ^
//│ res: int




1 as neg 0
//│ ╔══[ERROR] Type mismatch in 'as' binding:
//│ ║  l.95: 	1 as neg 0
//│ ║        	^^^^^^^^^^
//│ ╟── integer literal of type `int` does not match type `~int`
//│ ║  l.95: 	1 as neg 0
//│ ║        	^
//│ ╟── Note: constraint arises from application:
//│ ║  l.95: 	1 as neg 0
//│ ╙──      	     ^^^^^
//│ res: ~int
