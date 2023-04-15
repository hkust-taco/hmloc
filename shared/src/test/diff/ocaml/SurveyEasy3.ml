let check result condition =
 if condition then
   match result with
    | Left n -> "OK"
    | Right msg -> "ERROR: " ^ msg
 else result
 
(* (^): string -> string -> string is a string concatenation operator *)
(* Remember Left and Right are the constructors of the either type *)
//│ [ERROR] Type `string` does not match `(_, _) either`
//│ 
//│         (string) ---> (?a) <--- (?b) ---> ((_, _) either)
//│ 
//│ ◉ (string) is here
//│ │  - lib. let (^): string -> string -> string
//│ │                                      ^^^^^^
//│ │  - l.5      | Right msg -> "ERROR: " ^ msg
//│ │                            ^^^^^^^^^^^^^^^
//│ │  - l.3     match result with
//│ │            ^^^^^^^^^^^^^^^^^
//│ │             | Left n -> "OK" ...
//│ │             ^^^^^^^^^^^^^^^^^^^^
//│ │  - l.2   if condition then
//│ │          ^^^^^^^^^^^^^^^^^
//│ │            match result with ...
//│ │            ^^^^^^^^^^^^^^^^^^^^^
//│ ▼ 
//│ ◉ (?a) is assumed here
//│ ▲  - l.2   if condition then
//│ │          ^^^^^^^^^^^^^^^^^
//│ │            match result with ...
//│ │            ^^^^^^^^^^^^^^^^^^^^^
//│ │  - l.6   else result
//│ │               ^^^^^^
//│ │  - l.1  let check result condition =
//│ │                   ^^^^^^
//│ │ 
//│ ◉ (?b) is assumed here
//│ │  - l.1  let check result condition =
//│ │                   ^^^^^^
//│ │  - l.3     match result with
//│ │                  ^^^^^^
//│ ▼ 
//│ ◉ ((_, _) either) is here
//│    - l.4      | Left n -> "OK"
//│                 ^^^^^^
//│ U max: 7, total: 17
//│ UERR 1 errors
//│ L: 0 [string ~ [[[[either['a71','b72']]]]], [[[[string]]]] <: α69', α69' :> [[[α67']]], α67' <: [[[[either['a71','b72']]]]]]
