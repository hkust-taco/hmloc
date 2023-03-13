let rec assoc (d,k,l) =
  match l with
  | [] -> d
  | h::t -> let (f,s) = h in if k = f then s h else assoc d k t
//│ ╔══[ERROR] Type `_ -> _` does not match `_ * _ * _`
//│ ║  
//│ ╟──        _ -> _ <--- ?a ---> ?a * _ * _ 
//│ ║  
//│ ╟── [`_ -> _`] comes from this application
//│ ║  l.4:	  | h::t -> let (f,s) = h in if k = f then s h else assoc d k t
//│ ║      	                                                    ^^^^^^^
//│ ╟── so this match expression has type `_ -> _`
//│ ║  l.2:	  match l with
//│ ║      	  ^^^^^^^^^^^^
//│ ║      	  | [] -> d ...
//│ ║      	  ^^^^^^^^^^^^^
//│ ╟── so this reference has type `_ -> _`. However `?a` flows into `_ -> _`
//│ ║  l.3:	  | [] -> d
//│ ║      	          ^
//│ ╟── [`?a`] comes from this variable
//│ ║  l.1:	let rec assoc (d,k,l) =
//│ ║      	               ^
//│ ╟── so this reference has type `?a` and it flows into `?a * _ * _`
//│ ║  l.4:	  | h::t -> let (f,s) = h in if k = f then s h else assoc d k t
//│ ║      	                                                          ^
//│ ╟── [`?a * _ * _`] comes from this tuple literal
//│ ║  l.1:	let rec assoc (d,k,l) =
//│ ╙──    	              ^^^^^^^
