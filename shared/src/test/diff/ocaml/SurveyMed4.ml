let rec assoc (d,k,l) =
  match l with
  | [] -> d
  | h::t -> let (f,s) = h in if k = f then s else assoc d k t
//│ [ERROR] Type `_ * _ * _` does not match `_ -> _`
//│ 
//│         (?a * _ * _) ~~~~ (?a) ---> (?b) ~~~~ (?c) ---> (_ -> _)
//│ 
//│ ◉ (?a * _ * _) comes from
//│    - l.1  let rec assoc (d,k,l) =
//│                         ^^^^^^^
//│   ◉ ((?a * _ * _) -> ?b) comes from
//│   │  - l.1  let rec assoc (d,k,l) =
//│   │                       ^^^^^^^^^
//│   │           match l with ...
//│   │           ^^^^^^^^^^^^^^^^
//│   │  - l.1  let rec assoc (d,k,l) =
//│   │                 ^^^^^
//│   ▼ 
//│   ◉ (?assoc) is assumed for
//│   │  - l.1  let rec assoc (d,k,l) =
//│   │                 ^^^^^
//│   ▼ 
//│   ◉ (?a -> ?c) comes from
//│      - l.4    | h::t -> let (f,s) = h in if k = f then s else assoc d k t
//│                                                               ^^^^^
//│ ◉ (?a) is assumed for
//│ │  - l.1  let rec assoc (d,k,l) =
//│ │                        ^
//│ │  - l.3    | [] -> d
//│ │                   ^
//│ ▼ 
//│ ◉ (?b) is assumed for
//│    - l.2    match l with
//│             ^^^^^^^^^^^^
//│             | [] -> d ...
//│             ^^^^^^^^^^^^^
//│   ◉ ((?a * _ * _) -> ?b) comes from
//│   │  - l.1  let rec assoc (d,k,l) =
//│   │                       ^^^^^^^^^
//│   │           match l with ...
//│   │           ^^^^^^^^^^^^^^^^
//│   │  - l.1  let rec assoc (d,k,l) =
//│   │                 ^^^^^
//│   ▼ 
//│   ◉ (?assoc) is assumed for
//│   │  - l.1  let rec assoc (d,k,l) =
//│   │                 ^^^^^
//│   ▼ 
//│   ◉ (?a -> ?c) comes from
//│      - l.4    | h::t -> let (f,s) = h in if k = f then s else assoc d k t
//│                                                               ^^^^^
//│ ◉ (?c) is assumed for
//│ │  - l.4    | h::t -> let (f,s) = h in if k = f then s else assoc d k t
//│ │                                                           ^^^^^^^
//│ ▼ 
//│ ◉ (_ -> _) comes from
//│    - l.4    | h::t -> let (f,s) = h in if k = f then s else assoc d k t
//│                                                             ^^^^^^^
//│ U max: 10, total: 34
//│ UERR 1 errors
//│ L: 1 [([α68'], [α69'], [α70'],) ~ ([α69'] -> α83'), [([α68'], [α69'], [α70'],) - (([α68'], [α69'], [α70'],) -> α71') ~ ([α68'] -> α81') - α68', L: 0 [(([α68'], [α69'], [α70'],) -> α71') ~ ([α68'] -> α81'), (([α68'], [α69'], [α70'],) -> α71') <: assoc67', assoc67' <: ([α68'] -> α81')]], α68' <: α71', [α71' - (([α68'], [α69'], [α70'],) -> α71') ~ ([α68'] -> α81') - α81', L: 0 [(([α68'], [α69'], [α70'],) -> α71') ~ ([α68'] -> α81'), (([α68'], [α69'], [α70'],) -> α71') <: assoc67', assoc67' <: ([α68'] -> α81')]], α81' <: ([α69'] -> α83')]

