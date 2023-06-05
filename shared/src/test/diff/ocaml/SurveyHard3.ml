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
//│ U max: 22, total: 446
//│ UERR 3 errors
//│ L: 0 [list['a208'''] ~ ([α214'''], [α215'''],), list['a208'''] <: α206''', α206''' :> α202''', α202''' <: ([α214'''], [α215'''],)]
//│ L: 2 [([α228''], [α229''],) ~ list['a246''], [([α228''], [α229''],) - list[([α228''], [α229''],)] ~ list[α253''] - α253'', L: 0 [list[([α228''], [α229''],)] ~ list[α253''], list[([α228''], [α229''],)] <: list[α253'']]], [α253'' - list[α253''] ~ list[α236''] - α236'', L: 0 [list[α253''] ~ list[α236''], list[α253''] <: list[α236'']]], [α236'' - (α236'' -> α235'') ~ (α242'' -> α247'') - α242'', L: 1 [(α236'' -> α235'') ~ (α242'' -> α247''), [(α236'' -> α235'') - (α235'' -> (α236'' -> α235'')) ~ (α237'' -> (α242'' -> α247'')) - (α242'' -> α247''), L: 0 [(α235'' -> (α236'' -> α235'')) ~ (α237'' -> (α242'' -> α247'')), (α235'' -> (α236'' -> α235'')) :> (α237'' -> (α242'' -> α247''))]]]], α242'' <: α245'', α245'' :> list['a246'']]
//│ L: 2 [list['a246''] ~ ([α243''], [α244''],), list['a246''] <: α245'', α245'' :> α242'', [α242'' - (α242'' -> α247'') ~ (α236'' -> α235'') - α236'', L: 1 [(α242'' -> α247'') ~ (α236'' -> α235''), [(α242'' -> α247'') - (α237'' -> (α242'' -> α247'')) ~ (α235'' -> (α236'' -> α235'')) - (α236'' -> α235''), L: 0 [(α237'' -> (α242'' -> α247'')) ~ (α235'' -> (α236'' -> α235'')), (α237'' -> (α242'' -> α247'')) <: (α235'' -> (α236'' -> α235''))]]]], [α236'' - (α236'' -> α235'') ~ (α242'' -> α247'') - α242'', L: 1 [(α236'' -> α235'') ~ (α242'' -> α247''), [(α236'' -> α235'') - (α235'' -> (α236'' -> α235'')) ~ (α237'' -> (α242'' -> α247'')) - (α242'' -> α247''), L: 0 [(α235'' -> (α236'' -> α235'')) ~ (α237'' -> (α242'' -> α247'')), (α235'' -> (α236'' -> α235'')) :> (α237'' -> (α242'' -> α247''))]]]], α242'' <: ([α243''], [α244''],)]
