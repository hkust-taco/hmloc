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
//│ [ERROR] Type `bool` does not match `int`
//│ 
//│         (bool) ---> (?a) ---> (?b) <--- (int)
//│ 
//│ ◉ (bool) comes from
//│ │  - l.13   | h::t -> if (addNumbs (h :: t)) >= 10 then false else true
//│ │                                                       ^^^^^
//│ │  - l.13   | h::t -> if (addNumbs (h :: t)) >= 10 then false else true
//│ │                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ▼ 
//│ ◉ (?a) is assumed for
//│ │  - l.13   | h::t -> if (addNumbs (h :: t)) >= 10 then false else true
//│ │                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │  - l.11   match digits n with
//│ │           ^^^^^^^^^^^^^^^^^^^
//│ │           | [] -> 0 ...
//│ │           ^^^^^^^^^^^^^
//│ ▼ 
//│ ◉ (?b) is assumed for
//│ ▲  - l.11   match digits n with
//│ │           ^^^^^^^^^^^^^^^^^^^
//│ │           | [] -> 0 ...
//│ │           ^^^^^^^^^^^^^
//│ │ 
//│ ◉ (int) comes from
//│    - l.12   | [] -> 0
//│                     ^
//│ U max: 6, total: 137
//│ UERR 1 errors
//│ L: 0 [bool ~ int, bool <: α138', α138' <: α124', α124' :> int]
