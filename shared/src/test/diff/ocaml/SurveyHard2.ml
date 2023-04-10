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
//│ U max: 210, total: 1016
//│ UERR 1 errors
//│ L: 0 [int ~ [[[([α239'], [α240'],)]]], [[[int]]] <: α233', α233' <: [[[([α239'], [α240'],)]]]]

