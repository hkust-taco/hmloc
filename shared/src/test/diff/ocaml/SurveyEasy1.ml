let boolToBit x = if x then "0" else 1
//│ ╔══[ERROR] Type `string` does not match `int`
//│ ║  
//│ ╟──        (string) ---> (?a) <--- (int) 
//│ ║  
//│ ╟── (string) is the type of this `then` branch and it flows into `?a`
//│ ║  l.1:	let boolToBit x = if x then "0" else 1
//│ ║      	                            ^^^
//│ ╟── (?a) is assumed as the type of this if-then-else expression
//│ ║  l.1:	let boolToBit x = if x then "0" else 1
//│ ║      	                  ^^^^^^^^^^^^^^^^^^^^
//│ ╟── (int) is the type of this `else` branch and it flows into `?a`
//│ ║  l.1:	let boolToBit x = if x then "0" else 1
//│ ╙──    	                                     ^
