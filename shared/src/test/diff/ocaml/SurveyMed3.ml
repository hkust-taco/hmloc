let rec digitsOfInt n =
  if n <= 0 then [] else (digitsOfInt (n / 10)) @ [n mod 10]
  
let rec addNumbs n = match n with | [] -> 0 | h::t -> h + (addNumbs t)
  
let digits n = digitsOfInt (abs n)
  
let rec additivePersistence n =
  match digits n with
  | [] -> 0
  | h::t -> if (addNumbs (h :: t)) >= 10 then false else true
//│ ╔══[ERROR] Type `bool` does not match `int`
//│ ║  
//│ ╟──        (bool) ---> (?a) <--- (int) 
//│ ║  
//│ ╟── (bool) is assumed as the type of this `else` branch
//│ ║  l.11:	  | h::t -> if (addNumbs (h :: t)) >= 10 then false else true
//│ ║       	                                                         ^^^^^
//│ ╟── so this if-then-else expression has type `bool` and it flows into `?a`
//│ ║  l.11:	  | h::t -> if (addNumbs (h :: t)) >= 10 then false else true
//│ ║       	            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── (?a) is assumed as the type of this match expression
//│ ║  l.9:	  match digits n with
//│ ║      	  ^^^^^^^^^^^^^^^^^^^
//│ ║      	  | [] -> 0 ...
//│ ║      	  ^^^^^^^^^^^^^
//│ ╟── (int) is the type of this integer literal and it flows into `?a`
//│ ║  l.10:	  | [] -> 0
//│ ╙──     	          ^
