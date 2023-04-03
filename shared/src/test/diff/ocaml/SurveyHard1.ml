let rec sepConcat sep sl =
  match sl with
  | [] -> ""
  | h::t ->
      let f a x = a ^ (sep ^ x) in
      let base = h in let l = t in List.fold_left f base l
 
let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
 
(* (^): string -> string -> string is a string concatenation operator *)
(* List.fold_left: ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a *)
//│ ╔══[ERROR] Type `string` does not match `(_ * _) -> _`
//│ ║  
//│ ╟── `string` comes from this type expression
//│ ║  builtin:	let (^): string -> string -> string
//│ ║          	                             ^^^^^^
//│ ╟── so this operator application has type `string`
//│ ║  l.5:	      let f a x = a ^ (sep ^ x) in
//│ ║      	                  ^^^^^^^^^^^^^
//│ ╟── so this application has type `string`
//│ ║  l.6:	      let base = h in let l = t in List.fold_left f base l
//│ ║      	                                   ^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── so this match expression has type `string`
//│ ║  l.2:	  match sl with
//│ ║      	  ^^^^^^^^^^^^^
//│ ║      	  | [] -> "" ...
//│ ║      	  ^^^^^^^^^^^^^^
//│ ╟── so this application has type `string`. However it flows into `(_ * _) -> _`
//│ ║  l.8:	let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│ ╙──    	                               ^^^^^^^^^^^^^^^^^^^^^^^
//│ ╔══[ERROR] Type `_ list` does not match `(_ -> _) -> _ list -> _ list`
//│ ║  
//│ ╟── this pattern has type `_ list`
//│ ║  l.4:	  | h::t ->
//│ ║      	    ^^^^
//│ ╟── so this reference has type `_ list`
//│ ║  l.2:	  match sl with
//│ ║      	        ^^
//│ ╟── so this variable has type `_ list`. However it flows into `(_ -> _) -> _ list -> _ list`
//│ ║  l.1:	let rec sepConcat sep sl =
//│ ║      	                      ^^
//│ ╟── because this reference has type `(_ -> _) -> _ list -> _ list`
//│ ║  l.8:	let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│ ║      	                                              ^^^^^^^^
//│ ╟── because `(_ -> _) -> _ list -> _ list` comes from this type expression
//│ ║  builtin:	let List.map: ('a -> 'b) -> 'a list -> 'b list
//│ ╙──        	               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
