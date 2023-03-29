let rec assoc (d,k,l) =
  match l with
  | [] -> d
  | h::t -> let (f,s) = h in if k = f then s else assoc d k t
//│ ╔══[ERROR] Type `_ * _ * _` does not match `_ -> _`
//│ ║  
//│ ╟──        (?a * _ * _) <--- (?a) ---> (_ -> _) 
//│ ║  
//│ ╟── (?a * _ * _) is assumed as the type of this tuple literal
//│ ║  l.1:	let rec assoc (d,k,l) =
//│ ║      	              ^^^^^^^
//│ ╟── so this reference has type `?a * _ * _`. However `?a` flows into `?a * _ * _`
//│ ║  l.4:	  | h::t -> let (f,s) = h in if k = f then s else assoc d k t
//│ ║      	                                                        ^
//│ ╟── (?a) is assumed as the type of this variable
//│ ║  l.1:	let rec assoc (d,k,l) =
//│ ║      	               ^
//│ ╟── so this reference has type `?a`
//│ ║  l.3:	  | [] -> d
//│ ║      	          ^
//│ ╟── so this match expression has type `?a` and it flows into `_ -> _`
//│ ║  l.2:	  match l with
//│ ║      	  ^^^^^^^^^^^^
//│ ║      	  | [] -> d ...
//│ ║      	  ^^^^^^^^^^^^^
//│ ╟── (_ -> _) is assumed as the type of this application
//│ ║  l.4:	  | h::t -> let (f,s) = h in if k = f then s else assoc d k t
//│ ╙──    	                                                  ^^^^^^^

