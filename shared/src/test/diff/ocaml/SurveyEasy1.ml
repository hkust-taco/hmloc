let boolToBit x = if x then "0" else 1
//│ [ERROR] Type `int` does not match `string`
//│ 
//│         (int) ---> (?a) <--- (string)
//│ 
//│ ◉ (int) is here
//│ │  - l.1  let boolToBit x = if x then "0" else 1
//│ │                                              ^
//│ │  - l.1  let boolToBit x = if x then "0" else 1
//│ │                           ^^^^^^^^^^^^^^^^^^^^
//│ ▼ 
//│ ◉ (?a) is assumed here
//│ ▲  - l.1  let boolToBit x = if x then "0" else 1
//│ │                           ^^^^^^^^^^^^^^^^^^^^
//│ │ 
//│ ◉ (string) is here
//│    - l.1  let boolToBit x = if x then "0" else 1
//│                                       ^^^
//│ U max: 1, total: 4
//│ UERR 1 errors
//│ L: 0 [int ~ string, int <: α68', α68' :> string]
