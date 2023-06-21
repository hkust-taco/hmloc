let increment result = match result with
 | Left n -> n + 1
 | Right msg -> "ERROR: " ^ msg
 
(* (^): string -> string -> string  is a string concatenation operator *)
(* Remember Left and Right are the constructors of the either type *)
//│ [ERROR] Type `string` does not match `int`
//│ 
//│         (string) ---> (?a) <--- (int)
//│ 
//│ ◉ (string) comes from
//│ │  - lib. let (^): string -> string -> string
//│ │                                      ^^^^^^
//│ │  - l.3   | Right msg -> "ERROR: " ^ msg
//│ │                         ^^^^^^^^^^^^^^^
//│ │  - l.1  let increment result = match result with
//│ │                                ^^^^^^^^^^^^^^^^^
//│ │          | Left n -> n + 1 ...
//│ │          ^^^^^^^^^^^^^^^^^^^^^
//│ ▼ 
//│ ◉ (?a) is assumed for
//│ ▲  - l.1  let increment result = match result with
//│ │                                ^^^^^^^^^^^^^^^^^
//│ │          | Left n -> n + 1 ...
//│ │          ^^^^^^^^^^^^^^^^^^^^^
//│ │  - l.2   | Left n -> n + 1
//│ │                      ^^^^^
//│ │ 
//│ ◉ (int) comes from
//│    - lib. let (+): int -> int -> int
//│                                  ^^^
//│ U max: 1, total: 8
//│ UERR 1 errors
//│ L: 0 [string ~ int, string <: α43', α43' :> int]
