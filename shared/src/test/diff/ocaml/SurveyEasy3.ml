let check result condition =
 if condition then
   match result with
    | Left n -> "OK"
    | Right msg -> "ERROR: " ^ msg
 else result
 
(* (^): string -> string -> string is a string concatenation operator *)
(* Remember Left and Right are the constructors of the either type *)
//│ U max: 7, total: 17
//│ UERR 1 errors
//│ L: 0 [string ~ [[[[either['a71','b72']]]]], [[[[string]]]] <: α69', [[[α67']]] <: α69', α67' <: [[[[either['a71','b72']]]]]]
