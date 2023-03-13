let destructEither x y = if y then
  match x with
    | Left l -> l && true
    | Right r -> r || false
  else x
//│ ╔══[ERROR] Type `(_, _) either` does not match `bool`
//│ ║  
//│ ╟──        (_, _) either <--- ?a ---> ?b <--- bool 
//│ ║  
//│ ╟── [`(_, _) either`] comes from this pattern
//│ ║  l.3:	    | Left l -> l && true
//│ ║      	      ^^^^
//│ ╟── so this reference has type `(_, _) either`. However `?a` flows into `(_, _) either`
//│ ║  l.2:	  match x with
//│ ║      	        ^
//│ ╟── [`?a`] comes from this variable
//│ ║  l.1:	let destructEither x y = if y then
//│ ║      	                   ^
//│ ╟── so this `else` branch has type `?a` and it flows into `?b`
//│ ║  l.5:	  else x
//│ ║      	       ^^
//│ ╟── [`?b`] comes from this if-then-else expression
//│ ║  l.1:	let destructEither x y = if y then
//│ ║      	                         ^^^^^^^^^
//│ ║      	  match x with ...
//│ ║      	  ^^^^^^^^^^^^^^^^
//│ ╟── so this match expression has type `?b`
//│ ║  l.2:	  match x with
//│ ║      	  ^^^^^^^^^^^^
//│ ║      	    | Left l -> l && true ...
//│ ║      	    ^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── so this operator application has type `?b`. However `bool` flows into `?b`
//│ ║  l.4:	    | Right r -> r || false
//│ ║      	                 ^^^^^^^^^^
//│ ╟── [`bool`] comes from this type reference
//│ ║  builtin:	let (||): bool -> bool -> bool
//│ ╙──        	                          ^^^^
