let rec digitsOfInt n =
  if n <= 0 then [] else (digitsOfInt (n / 10)) @ [n mod 10]
  
let rec addNumbs n = match n with
 | [] -> 0
 | h::t -> h + (addNumbs t)
  
let digits n = digitsOfInt (abs n)
  
let rec additivePersistence n =
  match digits n with
  | [] -> 0
  | h::t -> if (addNumbs (h :: t)) >= 10 then false else true
 
(* (@): 'a list -> 'a list -> 'a list is a list concatenation operator *)
//│ [ERROR] Type `int` does not match `bool`
//│ 
//│ ◉ (int) is here
//│ │  - l.12   | [] -> 0
//│ │                   ^
//│ │  - l.11   match digits n with
//│ │           ^^^^^^^^^^^^^^^^^^^
//│ │           | [] -> 0 ...
//│ │           ^^^^^^^^^^^^^
//│ ▼ 
//│ ◉ ('a) is assumed here
//│ ▲  - l.11   match digits n with
//│ │           ^^^^^^^^^^^^^^^^^^^
//│ │           | [] -> 0 ...
//│ │           ^^^^^^^^^^^^^
//│ │  - l.13   | h::t -> if (addNumbs (h :: t)) >= 10 then false else true
//│ │                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │ 
//│ ◉ (bool) is here
//│    - l.13   | h::t -> if (addNumbs (h :: t)) >= 10 then false else true
//│                                                                    ^^^^
//│ U max: 102, total: 300
//│ UERR 1 errors
//│ L: 0 [int ~ [[[[bool]]]], [[int]] <: α137', α137' :> [[[[bool]]]]]
