let rec sepConcat sep sl =
  match sl with
  | [] -> ""
  | h::t ->
      let f a x = a ^ (sep ^ x) in
      let base = h in let l = t in List.fold_left f base l
  
let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│ ╔══[ERROR] Type `(_ * _) -> _` does not match `string`
//│ ║  
//│ ╟── this application has type `(_ * _) -> _`
//│ ║  l.8:	let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│ ║      	                               ^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── but this application has type `string`
//│ ║  l.8:	let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│ ║      	                               ^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── because this match expression has type `string`
//│ ║  l.2:	  match sl with
//│ ║      	  ^^^^^^^^^^^^^
//│ ║      	  | [] -> "" ...
//│ ║      	  ^^^^^^^^^^^^^^
//│ ╟── because this application has type `string`
//│ ║  l.6:	      let base = h in let l = t in List.fold_left f base l
//│ ║      	                                   ^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── because this operator application has type `string`
//│ ║  l.5:	      let f a x = a ^ (sep ^ x) in
//│ ║      	                  ^^^^^^^^^^^^^
//│ ╟── because `string` comes from this type expression
//│ ║  builtin:	let (^): string -> string -> string
//│ ╙──        	                             ^^^^^^
//│ ╔══[ERROR] Type `_ list` does not match `(_ -> _) -> _ list -> _ list`
//│ ║  
//│ ╟── this pattern has type `_ list`
//│ ║  l.3:	  | [] -> ""
//│ ║      	    ^^
//│ ╟── so this reference has type `_ list`
//│ ║  l.2:	  match sl with
//│ ║      	        ^^
//│ ╟── so this variable has type `_ list`
//│ ║  l.1:	let rec sepConcat sep sl =
//│ ║      	                      ^^
//│ ╟── but this variable has type `(_ -> _) -> _ list -> _ list`
//│ ║  l.1:	let rec sepConcat sep sl =
//│ ║      	                      ^^
//│ ╟── because this reference has type `(_ -> _) -> _ list -> _ list`
//│ ║  l.8:	let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│ ║      	                                              ^^^^^^^^
//│ ╟── because `(_ -> _) -> _ list -> _ list` comes from this type expression
//│ ║  builtin:	let List.map: ('a -> 'b) -> 'a list -> 'b list
//│ ╙──        	               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
