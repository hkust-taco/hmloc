let increment result = match result with
 | Left n -> n + 1
 | Right msg -> "ERROR: " ^ msg
 
(* (^): string -> string -> string  is a string concatenation operator *)
(* Remember Left and Right are the constructors of the either type *)
//│ [ERROR] Type `int` does not match `string`
//│ 
//│         (int) ---> (?a) <--- (string)
//│ 
//│ ◉ (int) is here
//│ │  - lib. let (+): int -> int -> int
//│ │                                ^^^
//│ │  - l.2   | Left n -> n + 1
//│ │                      ^^^^^
//│ │  - l.1  let increment result = match result with
//│ │                                ^^^^^^^^^^^^^^^^^
//│ │          | Left n -> n + 1 ...
//│ │          ^^^^^^^^^^^^^^^^^^^^^
//│ ▼ 
//│ ◉ (?a) is assumed here
//│ ▲  - l.1  let increment result = match result with
//│ │                                ^^^^^^^^^^^^^^^^^
//│ │          | Left n -> n + 1 ...
//│ │          ^^^^^^^^^^^^^^^^^^^^^
//│ │  - l.3   | Right msg -> "ERROR: " ^ msg
//│ │                         ^^^^^^^^^^^^^^^
//│ │ 
//│ ◉ (string) is here
//│    - lib. let (^): string -> string -> string
//│                                        ^^^^^^
//│ U max: 5, total: 11
//│ UERR 1 errors
//│ L: 0 [int ~ [[[string]]], [[[int]]] <: α68', α68' :> [[[string]]]]
