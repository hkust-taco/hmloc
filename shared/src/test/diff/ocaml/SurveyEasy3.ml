let check result condition =
 if condition then
   match result with
    | Left n -> "OK"
    | Right msg -> "ERROR: " ^ msg
 else result
 
(* (^): string -> string -> string is a string concatenation operator *)
(* Remember Left and Right are the constructors of the either type *)
//│ ╔══[ERROR] Type `(_, _) either` does not match `string`
//│ ║  
//│ ╟──        ((_, _) either) <--- (?a) ---> (?b) <--- (string) 
//│ ║  
//│ ╟── ((_, _) either) is assumed as the type of this pattern
//│ ║  l.4:	    | Left n -> "OK"
//│ ║      	      ^^^^^^
//│ ╟── so this reference has type `(_, _) either`. However `?a` flows into `(_, _) either`
//│ ║  l.3:	   match result with
//│ ║      	         ^^^^^^
//│ ╟── (?a) is assumed as the type of this variable
//│ ║  l.1:	let check result condition =
//│ ║      	          ^^^^^^
//│ ╟── so this `else` branch has type `?a` and it flows into `?b`
//│ ║  l.6:	 else result
//│ ║      	      ^^^^^^
//│ ╟── (?b) is assumed as the type of this if-then-else expression
//│ ║  l.2:	 if condition then
//│ ║      	 ^^^^^^^^^^^^^^^^^
//│ ║      	   match result with ...
//│ ║      	   ^^^^^^^^^^^^^^^^^^^^^
//│ ╟── so this match expression has type `?b`
//│ ║  l.3:	   match result with
//│ ║      	   ^^^^^^^^^^^^^^^^^
//│ ║      	    | Left n -> "OK" ...
//│ ║      	    ^^^^^^^^^^^^^^^^^^^^
//│ ╟── so this operator application has type `?b`. However `string` flows into `?b`
//│ ║  l.5:	    | Right msg -> "ERROR: " ^ msg
//│ ║      	                   ^^^^^^^^^^^^^^^
//│ ╟── (string) is assumed as the type of this type reference
//│ ║  builtin:	let (^): string -> string -> string
//│ ╙──        	                             ^^^^^^
