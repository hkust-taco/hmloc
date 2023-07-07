let rec sepConcat sep sl =
  match sl with
  | [] -> ""
  | h::t ->
      let f a x = a ^ (sep ^ x) in
      let base = h in let l = t in List.fold_left f base l
 
let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
 
(* (^): string -> string -> string is a string concatenation operator *)
(* List.fold_left: ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a *)
(* TODO: fix second last sequence string *)
//│ [ERROR] Type `_ list` does not match `(_ -> _) -> _ list -> _ list`
//│ 
//│         (_ list) <--- (?a) ~~~~ ((_ -> _) -> _ list -> _ list)
//│ 
//│ ◉ (_ list) comes from
//│ ▲  - l.3    | [] -> ""
//│ │             ^^
//│ │  - l.2    match sl with
//│ │                 ^^
//│ │ 
//│ ◉ (?a) is assumed for
//│    - l.1  let rec sepConcat sep sl =
//│                                 ^^
//│   ◉ (?a -> _) comes from
//│      - l.1  let rec sepConcat sep sl =
//│                                   ^^^^
//│               match sl with ...
//│               ^^^^^^^^^^^^^^^^^
//│     ◉ (_ -> ?a -> _) comes from
//│     │  - l.1  let rec sepConcat sep sl =
//│     │                           ^^^^^^^^
//│     │           match sl with ...
//│     │           ^^^^^^^^^^^^^^^^^
//│     │  - l.1  let rec sepConcat sep sl =
//│     │                 ^^^^^^^^^
//│     ▼ 
//│     ◉ (?sepConcat) is assumed for
//│     │  - l.1  let rec sepConcat sep sl =
//│     │                 ^^^^^^^^^
//│     ▼ 
//│     ◉ (string -> ?b) comes from
//│        - l.8  let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│                                              ^^^^^^^^^
//│   ◉ (?b) is assumed for
//│   │  - l.8  let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│   │                                        ^^^^^^^^^^^^^^
//│   ▼ 
//│   ◉ (((_ -> _) -> _ list -> _ list) -> _) comes from
//│      - l.8  let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│                                            ^^^^^^^^^^^^^^
//│ ◉ ((_ -> _) -> _ list -> _ list) comes from
//│    - l.8  let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│                                                         ^^^^^^^^
//│    - lib. let List.map: ('a -> 'b) -> 'a list -> 'b list
//│                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ [ERROR] Type `string` does not match `(_ * _) -> _`
//│ 
//│         (string) <--- (?a) ~~~~ (?b) ---> (?c) ~~~~ (?d) ---> ((_ * _) -> _)
//│ 
//│ ◉ (string) comes from
//│ ▲  - lib. let (^): string -> string -> string
//│ │                  ^^^^^^
//│ │  - l.5        let f a x = a ^ (sep ^ x) in
//│ │                           ^
//│ │ 
//│ ◉ (?a) is assumed for
//│    - l.5        let f a x = a ^ (sep ^ x) in
//│                       ^
//│   ◉ (?a -> _ -> string) comes from
//│   │  - l.5        let f a x = a ^ (sep ^ x) in
//│   │                     ^^^^^^^^^^^^^^^^^^^
//│   │  - l.6        let base = h in let l = t in List.fold_left f base l
//│   │                                                           ^
//│   ▼ 
//│   ◉ (?b -> _ -> ?b) comes from
//│      - lib. let List.fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
//│                                   ^^^^^^^^^^^^^^
//│ ◉ (?b) is assumed for
//│ │  - l.6        let base = h in let l = t in List.fold_left f base l
//│ │                                            ^^^^^^^^^^^^^^^^^^^^^^^
//│ ▼ 
//│ ◉ (?c) is assumed for
//│    - l.2    match sl with
//│             ^^^^^^^^^^^^^
//│             | [] -> "" ...
//│             ^^^^^^^^^^^^^^
//│   ◉ (_ -> ?c) comes from
//│      - l.1  let rec sepConcat sep sl =
//│                                   ^^^^
//│               match sl with ...
//│               ^^^^^^^^^^^^^^^^^
//│     ◉ (_ -> _ -> ?c) comes from
//│     │  - l.1  let rec sepConcat sep sl =
//│     │                           ^^^^^^^^
//│     │           match sl with ...
//│     │           ^^^^^^^^^^^^^^^^^
//│     │  - l.1  let rec sepConcat sep sl =
//│     │                 ^^^^^^^^^
//│     ▼ 
//│     ◉ (?sepConcat) is assumed for
//│     │  - l.1  let rec sepConcat sep sl =
//│     │                 ^^^^^^^^^
//│     ▼ 
//│     ◉ (string -> ?d) comes from
//│        - l.8  let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│                                              ^^^^^^^^^
//│   ◉ (?d) is assumed for
//│   │  - l.8  let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│   │                                        ^^^^^^^^^^^^^^
//│   ▼ 
//│   ◉ (((_ -> _) -> _ list -> _ list) -> ?e) comes from
//│      - l.8  let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│                                            ^^^^^^^^^^^^^^
//│ ◉ (?e) is assumed for
//│ │  - l.8  let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│ │                                        ^^^^^^^^^^^^^^^^^^^^^^^
//│ ▼ 
//│ ◉ ((_ * _) -> _) comes from
//│    - l.8  let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
//│                                          ^^^^^^^^^^^^^^^^^^^^^^^
//│ U max: 7, total: 76
//│ UERR 2 errors
//│ L: 2 [list['a59'] ~ ((α66' -> α67') -> (list[α66'] -> list[α67'])), list['a59'] :> α58', [α58' - (α58' -> α64') ~ ([((α66' -> α67') -> (list[α66'] -> list[α67']))] -> α68') - ((α66' -> α67') -> (list[α66'] -> list[α67'])), L: 1 [(α58' -> α64') ~ ([((α66' -> α67') -> (list[α66'] -> list[α67']))] -> α68'), [(α58' -> α64') - (α57' -> (α58' -> α64')) ~ ([string] -> α65') - α65', L: 0 [(α57' -> (α58' -> α64')) ~ ([string] -> α65'), (α57' -> (α58' -> α64')) <: sepConcat56', sepConcat56' <: ([string] -> α65')]], α65' <: ([((α66' -> α67') -> (list[α66'] -> list[α67']))] -> α68')]]]
//│ L: 2 [string ~ (([[α54']], [[α55']],) -> α69'), string :> α63', [α63' - (α63' -> (α61' -> [string])) ~ (α62' -> (α60' -> α62')) - α62', L: 0 [(α63' -> (α61' -> [string])) ~ (α62' -> (α60' -> α62')), (α63' -> (α61' -> [string])) <: (α62' -> (α60' -> α62'))]], α62' <: α64', [α64' - (α58' -> α64') ~ ([((α66' -> α67') -> (list[α66'] -> list[α67']))] -> α68') - α68', L: 1 [(α58' -> α64') ~ ([((α66' -> α67') -> (list[α66'] -> list[α67']))] -> α68'), [(α58' -> α64') - (α57' -> (α58' -> α64')) ~ ([string] -> α65') - α65', L: 0 [(α57' -> (α58' -> α64')) ~ ([string] -> α65'), (α57' -> (α58' -> α64')) <: sepConcat56', sepConcat56' <: ([string] -> α65')]], α65' <: ([((α66' -> α67') -> (list[α66'] -> list[α67']))] -> α68')]], α68' <: (([[α54']], [[α55']],) -> α69')]

