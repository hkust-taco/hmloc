let rec clone x n = if n <= 0 then [] else x :: (clone x (n - 1))
   
let padZero l1 l2 =
  let numZeros = (List.length l1) - (List.length l2) in
  let absNumZeros = abs numZeros in
  if numZeros = 0
  then (l1, l2)
  else
    (let listZeros = clone 0 absNumZeros in
     if numZeros > 0 then (l1, (listZeros @ l2)) else ((listZeros @ l1), l2))
  
let rec removeZero l =
  match l with | [] -> [] | 0::t -> removeZero t | h::t -> l
  
let bigAdd l1 l2 =
  let add (l1,l2) =
    let f a x =
      let (carry,currentSum) = a in
      if x = []
      then (0, (carry :: currentSum))
      else
        (let (toSum1,toSum2) = x in
         let intermediateValue = (toSum1 + toSum2) + carry in
         let valueToAddToArray = intermediateValue mod 10 in
         let carry = intermediateValue / 10 in
         (carry, (valueToAddToArray :: currentSum))) in
    let base = (0, []) in
    let args = List.rev (List.combine l1 l2) in
    let (_,res) = List.fold_left f base args in res in
  removeZero (add (padZero l1 l2))
  
(* (@): 'a list -> 'a list -> 'a list is a list concatenation operator *)
(* List.length: 'a list -> int *)
(* List.fold_left: ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a *)
(* List.combine: 'a list -> 'b list -> ('a * 'b) list zips two lists together *)
(* List.rev: 'a list -> 'a list reverses a list *)
//│ [ERROR] Type `_ list` does not match `_ * _`
//│ 
//│         (_ list) ---> (?a) <--- (?b) ---> (_ * _)
//│ 
//│ ◉ (_ list) is here
//│ │  - l.19       if x = []
//│ │                      ^^
//│ ▼ 
//│ ◉ (?a) is assumed here
//│ ▲  - l.19       if x = []
//│ │                  ^
//│ │  - l.17     let f a x =
//│ │                     ^
//│ │ 
//│ ◉ (?b) is assumed here
//│ │  - l.17     let f a x =
//│ │                     ^
//│ │  - l.22         (let (toSum1,toSum2) = x in
//│ │                                        ^
//│ ▼ 
//│ ◉ (_ * _) is here
//│    - l.22         (let (toSum1,toSum2) = x in
//│                        ^^^^^^^^^^^^^^^
//│ [ERROR] Type `_ * _` does not match `_ list`
//│ 
//│         (_ * _) ~~~~ (?a) ~~~~ (?b) ~~~~ (?c) ---> (?d) <--- (_ list)
//│ 
//│   ◉ ((_ * _) list) is here
//│   │  - lib. let List.combine: 'a list -> 'b list -> ('a * 'b) list
//│   │                                                  ^^^^^^^^^^^^^
//│   │  - l.28     let args = List.rev (List.combine l1 l2) in
//│   │                                 ^^^^^^^^^^^^^^^^^^^^
//│   ▼ 
//│   ◉ (?a list) is here
//│      - lib. let List.rev: 'a list -> 'a list
//│                           ^^^^^^^
//│   ◉ (?a list) is here
//│   │  - lib. let List.rev: 'a list -> 'a list
//│   │                                  ^^^^^^^
//│   │  - l.28     let args = List.rev (List.combine l1 l2) in
//│   │                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   │  - l.29     let (_,res) = List.fold_left f base args in res in
//│   │                                                 ^^^^
//│   ▼ 
//│   ◉ (?b list) is here
//│      - lib. let List.fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
//│                                                            ^^^^^^^
//│     ◉ (_ -> ?b -> _) is here
//│     ▲  - lib. let List.fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
//│     │                               ^^^^^^^^^^^^^^
//│     │  - l.29     let (_,res) = List.fold_left f base args in res in
//│     │                                          ^
//│     │ 
//│     ◉ (_ -> ?c -> _) is here
//│        - l.17     let f a x =
//│                         ^^^^^
//│                     let (carry,currentSum) = a in ...
//│                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ◉ (?c) is assumed here
//│ │  - l.17     let f a x =
//│ │                     ^
//│ │  - l.19       if x = []
//│ │                  ^
//│ ▼ 
//│ ◉ (?d) is assumed here
//│ ▲  - l.19       if x = []
//│ │                      ^^
//│ │ 
//│ ◉ (_ list) is here
//│    - l.19       if x = []
//│                        ^^
//│ [ERROR] Type `_ list` does not match `_ * _`
//│ 
//│         (_ list) ---> (?a) <--- (?b) ~~~~ (?c) ~~~~ (?b) ---> (_ * _)
//│ 
//│ ◉ (_ list) is here
//│ │  - l.19       if x = []
//│ │                      ^^
//│ ▼ 
//│ ◉ (?a) is assumed here
//│ ▲  - l.19       if x = []
//│ │                  ^
//│ │ 
//│ ◉ (?b) is assumed here
//│    - l.17     let f a x =
//│                       ^
//│     ◉ (_ -> ?b -> _) is here
//│     │  - l.17     let f a x =
//│     │                   ^^^^^
//│     │               let (carry,currentSum) = a in ...
//│     │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│     │  - l.29     let (_,res) = List.fold_left f base args in res in
//│     │                                          ^
//│     ▼ 
//│     ◉ (_ -> ?c -> _) is here
//│        - lib. let List.fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
//│                                     ^^^^^^^^^^^^^^
//│     ◉ (_ -> ?c -> _) is here
//│     ▲  - lib. let List.fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
//│     │                               ^^^^^^^^^^^^^^
//│     │  - l.29     let (_,res) = List.fold_left f base args in res in
//│     │                                          ^
//│     │ 
//│     ◉ (_ -> ?b -> _) is here
//│        - l.17     let f a x =
//│                         ^^^^^
//│                     let (carry,currentSum) = a in ...
//│                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ◉ (?b) is assumed here
//│ │  - l.17     let f a x =
//│ │                     ^
//│ │  - l.22         (let (toSum1,toSum2) = x in
//│ │                                        ^
//│ ▼ 
//│ ◉ (_ * _) is here
//│    - l.22         (let (toSum1,toSum2) = x in
//│                        ^^^^^^^^^^^^^^^
//│ U max: 14, total: 256
//│ UERR 3 errors
//│ L: 2 [([α222''], [α223''],) ~ list['a240''], [([α222''], [α223''],) - list[([α222''], [α223''],)] ~ list[α247''] - α247'', L: 0 [list[([α222''], [α223''],)] ~ list[α247''], list[([α222''], [α223''],)] <: list[α247'']]], [α247'' - list[α247''] ~ list[α230''] - α230'', L: 0 [list[α247''] ~ list[α230''], list[α247''] <: list[α230'']]], [α230'' - (α230'' -> α229'') ~ (α236'' -> α241'') - α236'', L: 1 [(α230'' -> α229'') ~ (α236'' -> α241''), [(α230'' -> α229'') - (α229'' -> (α230'' -> α229'')) ~ (α231'' -> (α236'' -> α241'')) - (α236'' -> α241''), L: 0 [(α229'' -> (α230'' -> α229'')) ~ (α231'' -> (α236'' -> α241'')), (α229'' -> (α230'' -> α229'')) :> (α231'' -> (α236'' -> α241''))]]]], α236'' <: α239'', α239'' :> list['a240'']]
//│ L: 2 [list['a240''] ~ ([α237''], [α238''],), list['a240''] <: α239'', α239'' :> α236'', [α236'' - (α236'' -> α241'') ~ (α230'' -> α229'') - α230'', L: 1 [(α236'' -> α241'') ~ (α230'' -> α229''), [(α236'' -> α241'') - (α231'' -> (α236'' -> α241'')) ~ (α229'' -> (α230'' -> α229'')) - (α230'' -> α229''), L: 0 [(α231'' -> (α236'' -> α241'')) ~ (α229'' -> (α230'' -> α229'')), (α231'' -> (α236'' -> α241'')) <: (α229'' -> (α230'' -> α229''))]]]], [α230'' - (α230'' -> α229'') ~ (α236'' -> α241'') - α236'', L: 1 [(α230'' -> α229'') ~ (α236'' -> α241''), [(α230'' -> α229'') - (α229'' -> (α230'' -> α229'')) ~ (α231'' -> (α236'' -> α241'')) - (α236'' -> α241''), L: 0 [(α229'' -> (α230'' -> α229'')) ~ (α231'' -> (α236'' -> α241'')), (α229'' -> (α230'' -> α229'')) :> (α231'' -> (α236'' -> α241''))]]]], α236'' <: ([α237''], [α238''],)]
//│ L: 0 [list['a202'''] ~ ([α208'''], [α209'''],), list['a202'''] <: α200''', α200''' :> α196''', α196''' <: ([α208'''], [α209'''],)]
