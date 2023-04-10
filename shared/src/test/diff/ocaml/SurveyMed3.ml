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
//│ UERR 1 errors
//│ [int ~ [[[[bool]]]], [[int]] <: α137', [[[[bool]]]] <: α137']
