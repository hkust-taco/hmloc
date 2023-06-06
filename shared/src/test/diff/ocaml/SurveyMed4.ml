let rec assoc (d,k,l) =
  match l with
  | [] -> d
  | h::t -> let (f,s) = h in if k = f then s else assoc d k t
//│ [ERROR] Type `_ * _ * _` does not match `_ -> _`
//│ 
//│         (?a * _ * _) ~~~~ (?a) ---> (?b) ~~~~ (?c) ---> (_ -> _)
//│ 
//│   ◉ ((?a * _ * _) -> ?b) is here
//│   │  - l.1  let rec assoc (d,k,l) =
//│   │                       ^^^^^^^^^
//│   │           match l with ...
//│   │           ^^^^^^^^^^^^^^^^
//│   │  - l.1  let rec assoc (d,k,l) =
//│   │                 ^^^^^
//│   ▼ 
//│   ◉ (?assoc) is assumed here
//│   │  - l.1  let rec assoc (d,k,l) =
//│   │                 ^^^^^
//│   ▼ 
//│   ◉ (?a -> ?c) is here
//│      - l.4    | h::t -> let (f,s) = h in if k = f then s else assoc d k t
//│                                                               ^^^^^
//│ ◉ (?a) is assumed here
//│ │  - l.1  let rec assoc (d,k,l) =
//│ │                        ^
//│ │  - l.3    | [] -> d
//│ │                   ^
//│ ▼ 
//│ ◉ (?b) is assumed here
//│    - l.2    match l with
//│             ^^^^^^^^^^^^
//│             | [] -> d ...
//│             ^^^^^^^^^^^^^
//│   ◉ ((?a * _ * _) -> ?b) is here
//│   │  - l.1  let rec assoc (d,k,l) =
//│   │                       ^^^^^^^^^
//│   │           match l with ...
//│   │           ^^^^^^^^^^^^^^^^
//│   │  - l.1  let rec assoc (d,k,l) =
//│   │                 ^^^^^
//│   ▼ 
//│   ◉ (?assoc) is assumed here
//│   │  - l.1  let rec assoc (d,k,l) =
//│   │                 ^^^^^
//│   ▼ 
//│   ◉ (?a -> ?c) is here
//│      - l.4    | h::t -> let (f,s) = h in if k = f then s else assoc d k t
//│                                                               ^^^^^
//│ ◉ (?c) is assumed here
//│ │  - l.4    | h::t -> let (f,s) = h in if k = f then s else assoc d k t
//│ │                                                           ^^^^^^^
//│ ▼ 
//│ ◉ (_ -> _) is here
//│    - l.4    | h::t -> let (f,s) = h in if k = f then s else assoc d k t
//│                                                             ^^^^^^^
//│ U max: 10, total: 34
//│ UERR 1 errors
//│ L: 1 [([α68'], [α69'], [α70'],) ~ ([α69'] -> α83'), [([α68'], [α69'], [α70'],) - (([α68'], [α69'], [α70'],) -> α71') ~ ([α68'] -> α81') - α68', L: 0 [(([α68'], [α69'], [α70'],) -> α71') ~ ([α68'] -> α81'), (([α68'], [α69'], [α70'],) -> α71') <: assoc67', assoc67' <: ([α68'] -> α81')]], α68' <: α71', [α71' - (([α68'], [α69'], [α70'],) -> α71') ~ ([α68'] -> α81') - α81', L: 0 [(([α68'], [α69'], [α70'],) -> α71') ~ ([α68'] -> α81'), (([α68'], [α69'], [α70'],) -> α71') <: assoc67', assoc67' <: ([α68'] -> α81')]], α81' <: ([α69'] -> α83')]

