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
//│ ◉ ((_, _) either) comes from
//│ ▲  - l.4      | Left n -> "OK"
//│ │               ^^^^^^
//│ │  - l.3     match result with
//│ │                  ^^^^^^
//│ │  - l.1  let check result condition =
//│ │                   ^^^^^^
//│ │ 
//│ ◉ (?a) is assumed for
//│ │  - l.1  let check result condition =
//│ │                   ^^^^^^
//│ │  - l.6   else result
//│ │               ^^^^^^
//│ │  - l.2   if condition then
//│ │          ^^^^^^^^^^^^^^^^^
//│ │            match result with ...
//│ │            ^^^^^^^^^^^^^^^^^^^^^
//│ ▼ 
//│ ◉ (?b) is assumed for
//│ ▲  - l.2   if condition then
//│ │          ^^^^^^^^^^^^^^^^^
//│ │            match result with ...
//│ │            ^^^^^^^^^^^^^^^^^^^^^
//│ │  - l.3     match result with
//│ │            ^^^^^^^^^^^^^^^^^
//│ │             | Left n -> "OK" ...
//│ │             ^^^^^^^^^^^^^^^^^^^^
//│ │ 
//│ ◉ (?c) is assumed for
//│ ▲  - l.3     match result with
//│ │            ^^^^^^^^^^^^^^^^^
//│ │             | Left n -> "OK" ...
//│ │             ^^^^^^^^^^^^^^^^^^^^
//│ │ 
//│ ◉ (string) comes from
//│    - l.4      | Left n -> "OK"
//│                           ^^^^
//│ U max: 2, total: 11
//│ UERR 1 errors
//│ L: 0 [either['a68','b69'] ~ string, either['a68','b69'] :> α64', α64' <: α66', α66' :> α67', α67' :> string]
