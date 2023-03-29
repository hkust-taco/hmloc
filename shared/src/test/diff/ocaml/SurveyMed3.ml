let rec digitsOfInt n =
  if n <= 0 then [] else (digitsOfInt (n / 10)) @ [n mod 10]
  
let rec addNumbs n = match n with
 | [] -> 0
 | h::t -> h + (addNumbs t)
  
let digits n = digitsOfInt (abs n)
  
let rec additivePersistence n =
  match digits n with
  | [] -> 0
  | h::t -> if (addNumbs (h :: t)) >= 10 then false else true
 
(* (@): 'a list -> 'a list -> 'a list is a list concatenation operator *)
//│ ╔══[ERROR] Type `bool` does not match `int`
//│ ║  
//│ ╟──        (bool) ---> (?a) <--- (int) 
//│ ║  
//│ ╟── (bool) is assumed as the type of this `else` branch
//│ ║  l.13:	  | h::t -> if (addNumbs (h :: t)) >= 10 then false else true
//│ ║       	                                                         ^^^^
//│ ╟── so this if-then-else expression has type `bool` and it flows into `?a`
//│ ║  l.13:	  | h::t -> if (addNumbs (h :: t)) >= 10 then false else true
//│ ║       	            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── (?a) is assumed as the type of this match expression
//│ ║  l.11:	  match digits n with
//│ ║       	  ^^^^^^^^^^^^^^^^^^^
//│ ║      	  | [] -> 0 ...
//│ ║      	  ^^^^^^^^^^^^^
//│ ╟── (int) is the type of this integer literal and it flows into `?a`
//│ ║  l.12:	  | [] -> 0
//│ ╙──     	          ^
