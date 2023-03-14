let destructEither x = match x with
 | Left l -> l + 1
 | Right r -> r && true
//│ ╔══[ERROR] Type `bool` does not match `int`
//│ ║  
//│ ╟──        (bool) ---> (?a) <--- (int) 
//│ ║  
//│ ╟── (bool) comes from this type expression
//│ ║  builtin:	let (&&): bool -> bool -> bool
//│ ║          	                          ^^^^
//│ ╟── so this operator application has type `bool` and it flows into `?a`
//│ ║  l.3:	 | Right r -> r && true
//│ ║      	              ^^^^^^^^^^
//│ ╟── (?a) is assumed as the type of this match expression
//│ ║  l.1:	let destructEither x = match x with
//│ ║      	                       ^^^^^^^^^^^^
//│ ║      	 | Left l -> l + 1 ...
//│ ║      	 ^^^^^^^^^^^^^^^^^^^^^
//│ ╟── so this operator application has type `?a`. However `int` flows into `?a`
//│ ║  l.2:	 | Left l -> l + 1
//│ ║      	             ^^^^^
//│ ╟── (int) is assumed as the type of this type reference
//│ ║  builtin:	let (+): int -> int -> int
//│ ╙──        	                       ^^^
