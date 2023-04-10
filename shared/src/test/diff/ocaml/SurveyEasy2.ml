let increment result = match result with
 | Left n -> n + 1
 | Right msg -> "ERROR: " ^ msg
 
(* (^): string -> string -> string  is a string concatenation operator *)
(* Remember Left and Right are the constructors of the either type *)
//│ U max: 5, total: 11
//│ UERR 1 errors
//│ L: 0 [int ~ [[[string]]], [[[int]]] <: α68', [[[string]]] <: α68']
