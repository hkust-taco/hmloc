let rec clone x n = if n <= 0 then [] else x :: (clone x (n - 1))

let addTuple (l1,l2) = l1 + l2

let rec removeZero l =
  match l with | [] -> [] | h::t -> if h = 0 then removeZero t else h :: t

let padZero l1 l2 =
  if (List.length l1) > (List.length l2)
  then (l1, ((clone 0 ((List.length l1) - (List.length l2))) @ l2))
  else
    if (List.length l1) < (List.length l2)
    then (((clone 0 ((List.length l2) - (List.length l1))) @ l1), l2)
    else (l1, l2)
 
let bigAdd l1 l2 =
  let add (l1,l2) =
    let f a x = (a + x) mod 10 in
    let base = 0 in
    let args = List.map addTuple (List.combine l1 l2) in
    let (_,res) = List.fold_left f base args in res in
  removeZero (add (padZero l1 l2))
 
(* (@): 'a list -> 'a list -> 'a list is a list concatenation operator *)
(* List.length: 'a list -> int *)
(* List.fold_left: ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a *)
(* List.combine: 'a list -> 'b list -> ('a * 'b) list zips two lists together *)
(* List.map: ('a -> 'b) -> 'a list -> 'b list map elements of a list *)
//│ ╔══[ERROR] Type `_ list * _` does not match `_ * _`
//│ ║  
//│ ╟──        (_ list * _) ---> (?a) <--- (_ * _) 
//│ ║  
//│ ╟── (_ list * _) is the type of this tuple literal and it flows into `?a`
//│ ║  l.6:	    then (((clone 0 ((List.length l2) - (List.length l1))) @ l1), l2)
//│ ║      	         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── (?a) is assumed as the type of this if-then-else expression
//│ ║  l.5:	    if (List.length l1) < (List.length l2)
//│ ║      	    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ║      	    then (((clone 0 ((List.length l2) - (List.length l1))) @ l1), l2) ...
//│ ║      	    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── (_ * _) is the type of this tuple literal and it flows into `?a`
//│ ║  l.7:	    else (l1, l2)
//│ ╙──    	         ^^^^^^^^
//│ ╔══[ERROR] Type `_ * _ list` does not match `_ list * _`
//│ ║  
//│ ╟──        (_ * _ list) ---> (?a) <--- (_ list * _) 
//│ ║  
//│ ╟── (_ * _ list) is the type of this tuple literal and it flows into `?a`
//│ ║  l.3:	  then (l1, ((clone 0 ((List.length l1) - (List.length l2))) @ l2))
//│ ║      	       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── (?a) is assumed as the type of this if-then-else expression
//│ ║  l.2:	  if (List.length l1) > (List.length l2)
//│ ║      	  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ║      	  then (l1, ((clone 0 ((List.length l1) - (List.length l2))) @ l2)) ...
//│ ║      	  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── so this if-then-else expression has type `?a`. However `_ list * _` flows into `?a`
//│ ║  l.5:	    if (List.length l1) < (List.length l2)
//│ ║      	    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ║      	    then (((clone 0 ((List.length l2) - (List.length l1))) @ l1), l2) ...
//│ ║      	    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── (_ list * _) is assumed as the type of this tuple literal
//│ ║  l.6:	    then (((clone 0 ((List.length l2) - (List.length l1))) @ l1), l2)
//│ ╙──    	         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╔══[ERROR] Type `_ * _ list` does not match `_ * _`
//│ ║  
//│ ╟──        (_ * _ list) ---> (?a) <--- (_ * _) 
//│ ║  
//│ ╟── (_ * _ list) is the type of this tuple literal and it flows into `?a`
//│ ║  l.3:	  then (l1, ((clone 0 ((List.length l1) - (List.length l2))) @ l2))
//│ ║      	       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── (?a) is assumed as the type of this if-then-else expression
//│ ║  l.2:	  if (List.length l1) > (List.length l2)
//│ ║      	  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ║      	  then (l1, ((clone 0 ((List.length l1) - (List.length l2))) @ l2)) ...
//│ ║      	  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── so this if-then-else expression has type `?a`. However `_ * _` flows into `?a`
//│ ║  l.5:	    if (List.length l1) < (List.length l2)
//│ ║      	    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ║      	    then (((clone 0 ((List.length l2) - (List.length l1))) @ l1), l2) ...
//│ ║      	    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── (_ * _) is assumed as the type of this tuple literal
//│ ║  l.7:	    else (l1, l2)
//│ ╙──    	         ^^^^^^^^
//│ ╔══[ERROR] Type `_ * _` does not match `_ list * _`
//│ ║  
//│ ╟──        (_ * _) ---> (?a) <--- (_ list * _) 
//│ ║  
//│ ╟── (_ * _) is the type of this tuple literal and it flows into `?a`
//│ ║  l.7:	    else (l1, l2)
//│ ║      	         ^^^^^^^^
//│ ╟── (?a) is assumed as the type of this if-then-else expression
//│ ║  l.5:	    if (List.length l1) < (List.length l2)
//│ ║      	    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ║      	    then (((clone 0 ((List.length l2) - (List.length l1))) @ l1), l2) ...
//│ ║      	    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── (_ list * _) is the type of this tuple literal and it flows into `?a`
//│ ║  l.6:	    then (((clone 0 ((List.length l2) - (List.length l1))) @ l1), l2)
//│ ╙──    	         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╔══[ERROR] Type `_ list * _` does not match `_ * _ list`
//│ ║  
//│ ╟──        (_ list * _) ---> (?a) <--- (_ * _ list) 
//│ ║  
//│ ╟── (_ list * _) is assumed as the type of this tuple literal
//│ ║  l.6:	    then (((clone 0 ((List.length l2) - (List.length l1))) @ l1), l2)
//│ ║      	         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── so this if-then-else expression has type `_ list * _` and it flows into `?a`
//│ ║  l.5:	    if (List.length l1) < (List.length l2)
//│ ║      	    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ║      	    then (((clone 0 ((List.length l2) - (List.length l1))) @ l1), l2) ...
//│ ║      	    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── (?a) is assumed as the type of this if-then-else expression
//│ ║  l.2:	  if (List.length l1) > (List.length l2)
//│ ║      	  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ║      	  then (l1, ((clone 0 ((List.length l1) - (List.length l2))) @ l2)) ...
//│ ║      	  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── (_ * _ list) is the type of this tuple literal and it flows into `?a`
//│ ║  l.3:	  then (l1, ((clone 0 ((List.length l1) - (List.length l2))) @ l2))
//│ ╙──    	       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╔══[ERROR] Type `_ * _` does not match `_ * _ list`
//│ ║  
//│ ╟──        (_ * _) ---> (?a) <--- (_ * _ list) 
//│ ║  
//│ ╟── (_ * _) is assumed as the type of this tuple literal
//│ ║  l.7:	    else (l1, l2)
//│ ║      	         ^^^^^^^^
//│ ╟── so this if-then-else expression has type `_ * _` and it flows into `?a`
//│ ║  l.5:	    if (List.length l1) < (List.length l2)
//│ ║      	    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ║      	    then (((clone 0 ((List.length l2) - (List.length l1))) @ l1), l2) ...
//│ ║      	    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── (?a) is assumed as the type of this if-then-else expression
//│ ║  l.2:	  if (List.length l1) > (List.length l2)
//│ ║      	  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ║      	  then (l1, ((clone 0 ((List.length l1) - (List.length l2))) @ l2)) ...
//│ ║      	  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── (_ * _ list) is the type of this tuple literal and it flows into `?a`
//│ ║  l.3:	  then (l1, ((clone 0 ((List.length l1) - (List.length l2))) @ l2))
//│ ╙──    	       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╔══[ERROR] Type `int` does not match `_ * _`
//│ ║  
//│ ╟──        (int) <--- (?a) ---> (_ * _) 
//│ ║  
//│ ╟── (int) comes from this type expression
//│ ║  builtin:	let (+): int -> int -> int
//│ ║          	         ^^^
//│ ╟── so this reference has type `int`. However `?a` flows into `int`
//│ ║  l.11:	    let f a x = (a + x) mod 10 in
//│ ║       	                 ^
//│ ╟── (?a) is assumed as the type of this variable
//│ ║  l.11:	    let f a x = (a + x) mod 10 in
//│ ║       	          ^
//│ ╟── so this application has type `?a` and it flows into `_ * _`
//│ ║  l.14:	    let (_,res) = List.fold_left f base args in res in
//│ ║       	                  ^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── (_ * _) is assumed as the type of this pattern
//│ ║  l.14:	    let (_,res) = List.fold_left f base args in res in
//│ ╙──     	        ^^^^^^^

