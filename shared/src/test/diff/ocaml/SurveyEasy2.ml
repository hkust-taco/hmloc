let increment result = match result with
 | Left n -> n + 1
 | Right msg -> "ERROR: " ^ msg
 
(* (^): string -> string -> string  is a string concatenation operator *)
(* Remember Left and Right are the constructors of the either type *)
//│ UERR 1 errors
//│ [int ~ [[[string]]], [[[int]]] <: α68', [[[string]]] <: α68']
