let boolToInt x = if x then "0" else 1
//│ ╔══[ERROR] Type `int` does not match `string`
//│ ║  
//│ ╟──        int ---> ?a <--- string 
//│ ║  
//│ ╟── [`int`] comes from this `else` branch and it flows into `?a`
//│ ║  l.1:	let boolToInt x = if x then "0" else 1
//│ ║      	                                     ^
//│ ╟── [`?a`] comes from this if-then-else expression
//│ ║  l.1:	let boolToInt x = if x then "0" else 1
//│ ║      	                  ^^^^^^^^^^^^^^^^^^^^
//│ ╟── [`string`] comes from this `then` branch and it flows into `?a`
//│ ║  l.1:	let boolToInt x = if x then "0" else 1
//│ ╙──    	                            ^^^
