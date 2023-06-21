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
//│ ◉ (_ list) comes from
//│ │  - l.19       if x = []
//│ │                      ^^
//│ ▼ 
//│ ◉ (?a) is assumed for
//│ ▲  - l.19       if x = []
//│ │                  ^
//│ │  - l.17     let f a x =
//│ │                     ^
//│ │ 
//│ ◉ (?b) is assumed for
//│ │  - l.17     let f a x =
//│ │                     ^
//│ │  - l.22         (let (toSum1,toSum2) = x in
//│ │                                        ^
//│ ▼ 
//│ ◉ (_ * _) comes from
//│    - l.22         (let (toSum1,toSum2) = x in
//│                        ^^^^^^^^^^^^^^^
//│ [ERROR] Type `_ * _` does not match `_ list`
//│ 
//│         (_ * _) ~~~~ (?a) ~~~~ (?b) ~~~~ (?c) ---> (?d) <--- (_ list)
//│ 
//│ ◉ (_ * _) comes from
//│    - lib. let List.combine: 'a list -> 'b list -> ('a * 'b) list
//│                                                    ^^^^^^^
//│   ◉ ((_ * _) list) comes from
//│   │  - lib. let List.combine: 'a list -> 'b list -> ('a * 'b) list
//│   │                                                  ^^^^^^^^^^^^^
//│   │  - l.28     let args = List.rev (List.combine l1 l2) in
//│   │                                 ^^^^^^^^^^^^^^^^^^^^
//│   ▼ 
//│   ◉ (?a list) comes from
//│      - lib. let List.rev: 'a list -> 'a list
//│                           ^^^^^^^
//│ ◉ (?a) is assumed for
//│   ◉ (?a list) comes from
//│   │  - lib. let List.rev: 'a list -> 'a list
//│   │                                  ^^^^^^^
//│   │  - l.28     let args = List.rev (List.combine l1 l2) in
//│   │                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   │  - l.29     let (_,res) = List.fold_left f base args in res in
//│   │                                                 ^^^^
//│   ▼ 
//│   ◉ (?b list) comes from
//│      - lib. let List.fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
//│                                                            ^^^^^^^
//│ ◉ (?b) is assumed for
//│   ◉ (?b -> _) comes from
//│      - lib. let List.fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
//│                                         ^^^^^^^^
//│     ◉ (_ -> ?b -> _) comes from
//│     ▲  - lib. let List.fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
//│     │                               ^^^^^^^^^^^^^^
//│     │  - l.29     let (_,res) = List.fold_left f base args in res in
//│     │                                          ^
//│     │ 
//│     ◉ (_ -> ?c -> _) comes from
//│        - l.17     let f a x =
//│                         ^^^^^
//│                     let (carry,currentSum) = a in ...
//│                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   ◉ (?c -> _) comes from
//│      - l.17     let f a x =
//│                         ^^^
//│                   let (carry,currentSum) = a in ...
//│                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ◉ (?c) is assumed for
//│ │  - l.17     let f a x =
//│ │                     ^
//│ │  - l.19       if x = []
//│ │                  ^
//│ ▼ 
//│ ◉ (?d) is assumed for
//│ ▲  - l.19       if x = []
//│ │                      ^^
//│ │ 
//│ ◉ (_ list) comes from
//│    - l.19       if x = []
//│                        ^^
//│ [ERROR] Type `_ list` does not match `_ * _`
//│ 
//│         (_ list) ---> (?a) <--- (?b) ~~~~ (?c) ~~~~ (?b) ---> (_ * _)
//│ 
//│ ◉ (_ list) comes from
//│ │  - l.19       if x = []
//│ │                      ^^
//│ ▼ 
//│ ◉ (?a) is assumed for
//│ ▲  - l.19       if x = []
//│ │                  ^
//│ │ 
//│ ◉ (?b) is assumed for
//│    - l.17     let f a x =
//│                       ^
//│   ◉ (?b -> _) comes from
//│      - l.17     let f a x =
//│                         ^^^
//│                   let (carry,currentSum) = a in ...
//│                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│     ◉ (_ -> ?b -> _) comes from
//│     │  - l.17     let f a x =
//│     │                   ^^^^^
//│     │               let (carry,currentSum) = a in ...
//│     │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│     │  - l.29     let (_,res) = List.fold_left f base args in res in
//│     │                                          ^
//│     ▼ 
//│     ◉ (_ -> ?c -> _) comes from
//│        - lib. let List.fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
//│                                     ^^^^^^^^^^^^^^
//│   ◉ (?c -> _) comes from
//│      - lib. let List.fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
//│                                         ^^^^^^^^
//│ ◉ (?c) is assumed for
//│   ◉ (?c -> _) comes from
//│      - lib. let List.fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
//│                                         ^^^^^^^^
//│     ◉ (_ -> ?c -> _) comes from
//│     ▲  - lib. let List.fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
//│     │                               ^^^^^^^^^^^^^^
//│     │  - l.29     let (_,res) = List.fold_left f base args in res in
//│     │                                          ^
//│     │ 
//│     ◉ (_ -> ?b -> _) comes from
//│        - l.17     let f a x =
//│                         ^^^^^
//│                     let (carry,currentSum) = a in ...
//│                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   ◉ (?b -> _) comes from
//│      - l.17     let f a x =
//│                         ^^^
//│                   let (carry,currentSum) = a in ...
//│                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ◉ (?b) is assumed for
//│ │  - l.17     let f a x =
//│ │                     ^
//│ │  - l.22         (let (toSum1,toSum2) = x in
//│ │                                        ^
//│ ▼ 
//│ ◉ (_ * _) comes from
//│    - l.22         (let (toSum1,toSum2) = x in
//│                        ^^^^^^^^^^^^^^^
//│ U max: 35, total: 551
//│ UERR 3 errors
//│ L: 2 [([α184''], [α185''],) ~ list['a199''], [([α184''], [α185''],) - list[([α184''], [α185''],)] ~ list[α207''] - α207'', L: 0 [list[([α184''], [α185''],)] ~ list[α207''], list[([α184''], [α185''],)] <: list[α207'']]], [α207'' - list[α207''] ~ list[α190''] - α190'', L: 0 [list[α207''] ~ list[α190''], list[α207''] <: list[α190'']]], [α190'' - (α190'' -> α189'') ~ (α196'' -> α201'') - α196'', L: 1 [(α190'' -> α189'') ~ (α196'' -> α201''), [(α190'' -> α189'') - (α189'' -> (α190'' -> α189'')) ~ (α191'' -> (α196'' -> α201'')) - (α196'' -> α201''), L: 0 [(α189'' -> (α190'' -> α189'')) ~ (α191'' -> (α196'' -> α201'')), (α189'' -> (α190'' -> α189'')) :> (α191'' -> (α196'' -> α201''))]]]], α196'' <: α200'', α200'' :> list['a199'']]
//│ L: 0 [list['a164'''] ~ ([α170'''], [α171'''],), list['a164'''] <: α162''', α162''' :> α158''', α158''' <: ([α170'''], [α171'''],)]
//│ L: 2 [list['a199''] ~ ([α197''], [α198''],), list['a199''] <: α200'', α200'' :> α196'', [α196'' - (α196'' -> α201'') ~ (α190'' -> α189'') - α190'', L: 1 [(α196'' -> α201'') ~ (α190'' -> α189''), [(α196'' -> α201'') - (α191'' -> (α196'' -> α201'')) ~ (α189'' -> (α190'' -> α189'')) - (α190'' -> α189''), L: 0 [(α191'' -> (α196'' -> α201'')) ~ (α189'' -> (α190'' -> α189'')), (α191'' -> (α196'' -> α201'')) <: (α189'' -> (α190'' -> α189''))]]]], [α190'' - (α190'' -> α189'') ~ (α196'' -> α201'') - α196'', L: 1 [(α190'' -> α189'') ~ (α196'' -> α201''), [(α190'' -> α189'') - (α189'' -> (α190'' -> α189'')) ~ (α191'' -> (α196'' -> α201'')) - (α196'' -> α201''), L: 0 [(α189'' -> (α190'' -> α189'')) ~ (α191'' -> (α196'' -> α201'')), (α189'' -> (α190'' -> α189'')) :> (α191'' -> (α196'' -> α201''))]]]], α196'' <: ([α197''], [α198''],)]

