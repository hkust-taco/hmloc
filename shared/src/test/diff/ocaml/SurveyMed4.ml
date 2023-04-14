let rec assoc (d,k,l) =
  match l with
  | [] -> d
  | h::t -> let (f,s) = h in if k = f then s else assoc d k t
//│ [ERROR] Type `_ * _ * _` does not match `_ -> _`
//│ 
//│   ◉ ((_ * _ * _) -> _) is here
//│   │  - l.1  let rec assoc (d,k,l) =
//│   │                       ^^^^^^^^^
//│   │           match l with ...
//│   │           ^^^^^^^^^^^^^^^^
//│   │  - l.1  let rec assoc (d,k,l) =
//│   │                 ^^^^^
//│   ▼ 
//│   ◉ ('assoc) is assumed here
//│   │  - l.1  let rec assoc (d,k,l) =
//│   │                 ^^^^^
//│   ▼ 
//│   ◉ (_ -> _) is here
//│      - l.4    | h::t -> let (f,s) = h in if k = f then s else assoc d k t
//│                                                               ^^^^^
//│ ◉ ('a) is assumed here
//│ │  - l.1  let rec assoc (d,k,l) =
//│ │                        ^
//│ │  - l.3    | [] -> d
//│ │                   ^
//│ │  - l.2    match l with
//│ │           ^^^^^^^^^^^^
//│ │           | [] -> d ...
//│ │           ^^^^^^^^^^^^^
//│ ▼ 
//│ ◉ (_ -> _) is here
//│    - l.4    | h::t -> let (f,s) = h in if k = f then s else assoc d k t
//│                                                             ^^^^^^^
//│ [ERROR] Type `_ -> _` does not match `_ * _ * _`
//│ 
//│ ◉ (_ -> _) is here
//│ ▲  - l.4    | h::t -> let (f,s) = h in if k = f then s else assoc d k t
//│ │                                                           ^^^^^^^
//│ │  - l.2    match l with
//│ │           ^^^^^^^^^^^^
//│ │           | [] -> d ...
//│ │           ^^^^^^^^^^^^^
//│ │  - l.3    | [] -> d
//│ │                   ^
//│ │  - l.1  let rec assoc (d,k,l) =
//│ │                        ^
//│ │ 
//│ ◉ ('a) is assumed here
//│ │  - l.1  let rec assoc (d,k,l) =
//│ │                        ^
//│ │  - l.4    | h::t -> let (f,s) = h in if k = f then s else assoc d k t
//│ │                                                                 ^
//│ ▼ 
//│ ◉ ('a * _ * _) is here
//│    - l.1  let rec assoc (d,k,l) =
//│                         ^^^^^^^
//│ U max: 31, total: 90
//│ UERR 2 errors
//│ L: 1 [(α68', α69', α70',) ~ [[[[[[([α69'] -> α83')]]]]]], [(α68', α69', α70',) - ((α68', α69', α70',) -> α71') ~ ([α68'] -> α81') - [α68'], [[((α68', α69', α70',) -> α71')]] <: assoc67', assoc67' <: [[[([α68'] -> α81')]]]], α68' <: [[[[[[([α69'] -> α83')]]]]]]]
//│ L: 0 [[[[[[[([α69'] -> α83')]]]]]] ~ (α68', α69', α70',), [[[[[[([α69'] -> α83')]]]]]] :> α68', α68' <: [[[[(α68', α69', α70',)]]]]]

