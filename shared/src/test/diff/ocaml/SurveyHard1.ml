let rec sepConcat sep sl =
  match sl with
  | [] -> ""
  | h::t ->
      let f a x = a ^ (sep ^ x) in
      let base = h in let l = t in List.fold_left f base l
 
let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
 
(* (^): string -> string -> string is a string concatenation operator *)
(* List.fold_left: ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a *)
//│ [ERROR] Type `_ list` does not match `(_ -> _) -> _ list -> _ list`
//│ 
//│         (_ list) <--- 
//│ 
//│ ◉ (_ list) is here
//│ ▲  - l.3    | [] -> ""
//│ │             ^^
//│ │  - l.2    match sl with
//│ │                 ^^
//│ │ 
//│ ◉ (?a) is assumed here
//│    - l.1  let rec sepConcat sep sl =
//│                                 ^^
//│   ◉ (_ -> _) is here
//│   │  - l.1  let rec sepConcat sep sl =
//│   │                               ^^^^
//│   │           match sl with ...
//│   │           ^^^^^^^^^^^^^^^^^
//│   │  - l.8  let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│   │                                        ^^^^^^^^^^^^^^
//│   ▼ 
//│   ◉ (?b) is assumed here
//│   │  - l.8  let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│   │                                        ^^^^^^^^^^^^^^
//│   ▼ 
//│   ◉ (((_ -> _) -> _ list -> _ list) -> _) is here
//│      - l.8  let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│                                            ^^^^^^^^^^^^^^
//│ [ERROR] Type `string` does not match `(_ * _) -> _`
//│ 
//│         (string) ---> (?a) ---> ((_ * _) -> _)
//│ 
//│ ◉ (string) is here
//│ │  - lib. let (^): string -> string -> string
//│ │                                      ^^^^^^
//│ │  - l.5        let f a x = a ^ (sep ^ x) in
//│ │                           ^^^^^^^^^^^^^
//│ │  - l.6        let base = h in let l = t in List.fold_left f base l
//│ │                                            ^^^^^^^^^^^^^^^^^^^^^^^
//│ │  - l.2    match sl with
//│ │           ^^^^^^^^^^^^^
//│ │           | [] -> "" ...
//│ │           ^^^^^^^^^^^^^^
//│ │  - l.8  let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│ │                                        ^^^^^^^^^^^^^^^^^^^^^^^
//│ ▼ 
//│ ◉ (?a) is assumed here
//│ │  - l.8  let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│ │                                        ^^^^^^^^^^^^^^^^^^^^^^^
//│ ▼ 
//│ ◉ ((_ * _) -> _) is here
//│    - l.8  let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│                                          ^^^^^^^^^^^^^^^^^^^^^^^
//│ [ERROR] Type `(_ -> _) -> _ list -> _ list` does not match `_ list`
//│ 
//│         ((_ -> _) -> _ list -> _ list) ---> (?a) ---> (_ list)
//│ 
//│ ◉ ((_ -> _) -> _ list -> _ list) is here
//│ │  - lib. let List.map: ('a -> 'b) -> 'a list -> 'b list
//│ │                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │  - l.8  let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│ │                                                       ^^^^^^^^
//│ │  - l.1  let rec sepConcat sep sl =
//│ │                               ^^
//│ ▼ 
//│ ◉ (?a) is assumed here
//│ │  - l.1  let rec sepConcat sep sl =
//│ │                               ^^
//│ │  - l.2    match sl with
//│ │                 ^^
//│ ▼ 
//│ ◉ (_ list) is here
//│    - l.3    | [] -> ""
//│               ^^
//│ [ERROR] Type `string` does not match `(_ * _) -> _`
//│ 
//│         (string) ---> (?a) ---> ((_ * _) -> _)
//│ 
//│ ◉ (string) is here
//│ │  - l.3    | [] -> ""
//│ │                   ^^
//│ ▼ 
//│ ◉ (?a) is assumed here
//│    - l.2    match sl with
//│             ^^^^^^^^^^^^^
//│             | [] -> "" ...
//│             ^^^^^^^^^^^^^^
//│   ◉ (_ -> _) is here
//│   │  - l.1  let rec sepConcat sep sl =
//│   │                               ^^^^
//│   │           match sl with ...
//│   │           ^^^^^^^^^^^^^^^^^
//│   │  - l.8  let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│   │                                        ^^^^^^^^^^^^^^
//│   ▼ 
//│   ◉ (?b) is assumed here
//│   │  - l.8  let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│   │                                        ^^^^^^^^^^^^^^
//│   ▼ 
//│   ◉ (((_ -> _) -> _ list -> _ list) -> _) is here
//│      - l.8  let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│                                            ^^^^^^^^^^^^^^
//│ ◉ (?c) is assumed here
//│ │  - l.8  let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│ │                                        ^^^^^^^^^^^^^^^^^^^^^^^
//│ ▼ 
//│ ◉ ((_ * _) -> _) is here
//│    - l.8  let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│                                          ^^^^^^^^^^^^^^^^^^^^^^^
//│ [ERROR] Type `string` does not match `(_ * _) -> _`
//│ 
//│         (string) ---> (?a) ---> ((_ * _) -> _)
//│ 
//│ ◉ (string) is here
//│ │  - l.3    | [] -> ""
//│ │                   ^^
//│ ▼ 
//│ ◉ (?a) is assumed here
//│    - l.2    match sl with
//│             ^^^^^^^^^^^^^
//│             | [] -> "" ...
//│             ^^^^^^^^^^^^^^
//│     ◉ (_ -> _ -> _) is here
//│     │  - l.1  let rec sepConcat sep sl =
//│     │                           ^^^^^^^^
//│     │           match sl with ...
//│     │           ^^^^^^^^^^^^^^^^^
//│     │  - l.1  let rec sepConcat sep sl =
//│     │                 ^^^^^^^^^
//│     ▼ 
//│     ◉ (?sepConcat) is assumed here
//│     │  - l.1  let rec sepConcat sep sl =
//│     │                 ^^^^^^^^^
//│     ▼ 
//│     ◉ (string -> _) is here
//│        - l.8  let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│                                              ^^^^^^^^^
//│   ◉ (?b) is assumed here
//│   │  - l.8  let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│   │                                        ^^^^^^^^^^^^^^
//│   ▼ 
//│   ◉ (((_ -> _) -> _ list -> _ list) -> _) is here
//│      - l.8  let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│                                            ^^^^^^^^^^^^^^
//│ ◉ (?c) is assumed here
//│ │  - l.8  let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│ │                                        ^^^^^^^^^^^^^^^^^^^^^^^
//│ ▼ 
//│ ◉ ((_ * _) -> _) is here
//│    - l.8  let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│                                          ^^^^^^^^^^^^^^^^^^^^^^^
//│ [ERROR] Type `_ list` does not match `(_ -> _) -> _ list -> _ list`
//│ 
//│         (_ list) <--- 
//│ 
//│ ◉ (_ list) is here
//│ ▲  - l.3    | [] -> ""
//│ │             ^^
//│ │  - l.2    match sl with
//│ │                 ^^
//│ │ 
//│ ◉ (?a) is assumed here
//│    - l.1  let rec sepConcat sep sl =
//│                                 ^^
//│     ◉ (_ -> _ -> _) is here
//│     │  - l.1  let rec sepConcat sep sl =
//│     │                           ^^^^^^^^
//│     │           match sl with ...
//│     │           ^^^^^^^^^^^^^^^^^
//│     │  - l.1  let rec sepConcat sep sl =
//│     │                 ^^^^^^^^^
//│     ▼ 
//│     ◉ (?sepConcat) is assumed here
//│     │  - l.1  let rec sepConcat sep sl =
//│     │                 ^^^^^^^^^
//│     ▼ 
//│     ◉ (string -> _) is here
//│        - l.8  let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│                                              ^^^^^^^^^
//│   ◉ (?b) is assumed here
//│   │  - l.8  let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│   │                                        ^^^^^^^^^^^^^^
//│   ▼ 
//│   ◉ (((_ -> _) -> _ list -> _ list) -> _) is here
//│      - l.8  let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│                                            ^^^^^^^^^^^^^^
//│ U max: 56, total: 186
//│ UERR 6 errors
//│ L: 2 [string ~ [[(([α92'], [α93'],) -> α111')]], [[string]] <: α99', [α99' - (α97' -> α99') ~ ([((α106' -> α107') -> (list[α106'] -> list[α107']))] -> α109') - α109', [(α97' -> α99') - (α96' -> (α97' -> α99')) ~ (string -> α105') - α105', [[(α96' -> (α97' -> α99'))]] <: sepConcat95', sepConcat95' <: [[[(string -> α105')]]]], α105' <: [[([((α106' -> α107') -> (list[α106'] -> list[α107']))] -> α109')]]], α109' <: [[(([α92'], [α93'],) -> α111')]]]
//│ L: 1 [string ~ [[(([α92'], [α93'],) -> α111')]], [[string]] <: α99', [α99' - (α97' -> α99') ~ ([((α106' -> α107') -> (list[α106'] -> list[α107']))] -> α109') - α109', [[[(α97' -> α99')]]] <: α105', α105' <: [[([((α106' -> α107') -> (list[α106'] -> list[α107']))] -> α109')]]], α109' <: [[(([α92'], [α93'],) -> α111')]]]
//│ L: 0 [string ~ [[(([α92'], [α93'],) -> α111')]], [[[[[[[string]]]]]]] <: α109', α109' <: [[(([α92'], [α93'],) -> α111')]]]
//│ L: 0 [((α106' -> α107') -> (list[α106'] -> list[α107'])) ~ [[[[list['a98']]]]], [[[[((α106' -> α107') -> (list[α106'] -> list[α107']))]]]] <: α97', α97' <: [[[[list['a98']]]]]]
//│ L: 2 [[[[[list['a98']]]]] ~ ((α106' -> α107') -> (list[α106'] -> list[α107'])), [[[[list['a98']]]]] :> α97', [α97' - (α97' -> α99') ~ ([((α106' -> α107') -> (list[α106'] -> list[α107']))] -> α109') - [((α106' -> α107') -> (list[α106'] -> list[α107']))], [(α97' -> α99') - (α96' -> (α97' -> α99')) ~ (string -> α105') - α105', [[(α96' -> (α97' -> α99'))]] <: sepConcat95', sepConcat95' <: [[[(string -> α105')]]]], α105' <: [[([((α106' -> α107') -> (list[α106'] -> list[α107']))] -> α109')]]]]
//│ L: 1 [[[[[list['a98']]]]] ~ ((α106' -> α107') -> (list[α106'] -> list[α107'])), [[[[list['a98']]]]] :> α97', [α97' - (α97' -> α99') ~ ([((α106' -> α107') -> (list[α106'] -> list[α107']))] -> α109') - [((α106' -> α107') -> (list[α106'] -> list[α107']))], [[[(α97' -> α99')]]] <: α105', α105' <: [[([((α106' -> α107') -> (list[α106'] -> list[α107']))] -> α109')]]]]
