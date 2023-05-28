let check result condition =
 if condition then
   match result with
    | Left n -> "OK"
    | Right msg -> "ERROR: " ^ msg
 else result
 
(* (^): string -> string -> string is a string concatenation operator *)
(* Remember Left and Right are the constructors of the either type *)
//│ [ERROR] Type `(_, _) either` does not match `string`
//│ 
//│         ((_, _) either) <--- (?a) ---> (?b) <--- (?c) <--- (string)
//│ 
//│ ◉ ((_, _) either) is here
//│ ▲  - l.4      | Left n -> "OK"
//│ │               ^^^^^^
//│ │  - l.3     match result with
//│ │                  ^^^^^^
//│ │  - l.1  let check result condition =
//│ │                   ^^^^^^
//│ │ 
//│ ◉ (?a) is assumed here
//│ │  - l.1  let check result condition =
//│ │                   ^^^^^^
//│ │  - l.6   else result
//│ │               ^^^^^^
//│ │  - l.2   if condition then
//│ │          ^^^^^^^^^^^^^^^^^
//│ │            match result with ...
//│ │            ^^^^^^^^^^^^^^^^^^^^^
//│ ▼ 
//│ ◉ (?b) is assumed here
//│ ▲  - l.2   if condition then
//│ │          ^^^^^^^^^^^^^^^^^
//│ │            match result with ...
//│ │            ^^^^^^^^^^^^^^^^^^^^^
//│ │  - l.3     match result with
//│ │            ^^^^^^^^^^^^^^^^^
//│ │             | Left n -> "OK" ...
//│ │             ^^^^^^^^^^^^^^^^^^^^
//│ │ 
//│ ◉ (?c) is assumed here
//│ ▲  - l.3     match result with
//│ │            ^^^^^^^^^^^^^^^^^
//│ │             | Left n -> "OK" ...
//│ │             ^^^^^^^^^^^^^^^^^^^^
//│ │ 
//│ ◉ (string) is here
//│    - l.4      | Left n -> "OK"
//│                           ^^^^
//│ U max: 1, total: 10
//│ UERR 1 errors
//│ L: 0 [either['a71','b72'] ~ string, either['a71','b72'] :> α67', α67' <: α69', α69' :> α70', α70' :> string]
