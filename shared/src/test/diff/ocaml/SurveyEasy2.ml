let increment result = match result with
 | Left n -> n + 1
 | Right msg -> "ERROR: " ^ msg
 
(* (^): string -> string -> string  is a string concatenation operator *)
(* Remember Left and Right are the constructors of the either type *)
//│ ╔══[ERROR] Type `string` does not match `int`
//│ ║  
//│ ╟──        (string) ---> (?a) <--- (int) 
//│ ║  
//│ ╟── (string) comes from this type expression
//│ ║  builtin:	let (^): string -> string -> string
//│ ║          	                             ^^^^^^
//│ ╟── so this operator application has type `string` and it flows into `?a`
//│ ║  l.3:	 | Right msg -> "ERROR: " ^ msg
//│ ║      	                ^^^^^^^^^^^^^^^
//│ ╟── (?a) is assumed as the type of this match expression
//│ ║  l.1:	let increment result = match result with
//│ ║      	                       ^^^^^^^^^^^^^^^^^
//│ ║      	 | Left n -> n + 1 ...
//│ ║      	 ^^^^^^^^^^^^^^^^^^^^^
//│ ╟── so this operator application has type `?a`. However `int` flows into `?a`
//│ ║  l.2:	 | Left n -> n + 1
//│ ║      	             ^^^^^
//│ ╟── (int) is assumed as the type of this type reference
//│ ║  builtin:	let (+): int -> int -> int
//│ ╙──        	                       ^^^
