let destructEither x y = if y then
  match x with
    | Left l -> l && true
    | Right r -> r || false
  else x
//│ ╔══[ERROR] Type `bool` does not match `(_, _) either`
//│ ║  
//│ ╟──        bool ---> ?a <--- ?b ---> (_, _) either 
//│ ║  
//│ ╟── [`bool`] comes from this type expression
//│ ║  builtin:	let (||): bool -> bool -> bool
//│ ║          	                          ^^^^
//│ ╟── so this operator application has type `bool`
//│ ║  l.4:	    | Right r -> r || false
//│ ║      	                 ^^^^^^^^^^
//│ ╟── so this match expression has type `bool` and it flows into `?a`
//│ ║  l.2:	  match x with
//│ ║      	  ^^^^^^^^^^^^
//│ ║      	    | Left l -> l && true ...
//│ ║      	    ^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── [`?a`] comes from this if-then-else expression
//│ ║  l.1:	let destructEither x y = if y then
//│ ║      	                         ^^^^^^^^^
//│ ║      	  match x with ...
//│ ║      	  ^^^^^^^^^^^^^^^^
//│ ╟── so this `else` branch has type `?a`. However `?b` flows into `?a`
//│ ║  l.5:	  else x
//│ ║      	       ^^
//│ ╟── [`?b`] comes from this variable
//│ ║  l.1:	let destructEither x y = if y then
//│ ║      	                   ^
//│ ╟── so this reference has type `?b` and it flows into `(_, _) either`
//│ ║  l.2:	  match x with
//│ ║      	        ^
//│ ╟── [`(_, _) either`] comes from this pattern
//│ ║  l.3:	    | Left l -> l && true
//│ ╙──    	      ^^^^
