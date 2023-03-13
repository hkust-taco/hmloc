let rec sepConcat sep sl =
  match sl with
  | [] -> ""
  | h::t ->
      let f a x = a ^ (sep ^ x) in
      let base = h in let l = t in List.fold_left f base l
  
let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│ ╔══[ERROR] Type `(_ * _) -> _` does not match `string`
//│ ║  
//│ ╟──        (_ * _) -> _ <--- ?a ---> string 
//│ ║  
//│ ╟── [`(_ * _) -> _`] comes from this application
//│ ║  l.8:	let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│ ║      	                               ^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── so this match expression has type `(_ * _) -> _`. However `?a` flows into `(_ * _) -> _`
//│ ║  l.2:	  match sl with
//│ ║      	  ^^^^^^^^^^^^^
//│ ║      	  | [] -> "" ...
//│ ║      	  ^^^^^^^^^^^^^^
//│ ╟── [`?a`] comes from this application
//│ ║  l.6:	      let base = h in let l = t in List.fold_left f base l
//│ ║      	                                   ^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── so this variable has type `?a`
//│ ║  l.5:	      let f a x = a ^ (sep ^ x) in
//│ ║      	            ^
//│ ╟── so this reference has type `?a` and it flows into `string`
//│ ║  l.5:	      let f a x = a ^ (sep ^ x) in
//│ ║      	                  ^
//│ ╟── [`string`] comes from this type reference
//│ ║  builtin:	let (^): string -> string -> string
//│ ╙──        	         ^^^^^^
