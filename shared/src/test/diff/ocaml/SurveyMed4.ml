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
//│ L: 1 [([α43'], [α44'], [α45'],) ~ ([α44'] -> α58'), [([α43'], [α44'], [α45'],) - (([α43'], [α44'], [α45'],) -> α46') ~ ([α43'] -> α56') - α43', L: 0 [(([α43'], [α44'], [α45'],) -> α46') ~ ([α43'] -> α56'), (([α43'], [α44'], [α45'],) -> α46') <: assoc42', assoc42' <: ([α43'] -> α56')]], α43' <: α46', [α46' - (([α43'], [α44'], [α45'],) -> α46') ~ ([α43'] -> α56') - α56', L: 0 [(([α43'], [α44'], [α45'],) -> α46') ~ ([α43'] -> α56'), (([α43'], [α44'], [α45'],) -> α46') <: assoc42', assoc42' <: ([α43'] -> α56')]], α56' <: ([α44'] -> α58')]

