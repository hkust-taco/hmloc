let boolToBit x = if x then "0" else 1
//│ [ERROR] Type `string` does not match `int`
//│ 
//│ ◉ (string) is here
//│ │  - l.1  let boolToBit x = if x then "0" else 1
//│ │                                     ^^^
//│ │  - l.1  let boolToBit x = if x then "0" else 1
//│ │                           ^^^^^^^^^^^^^^^^^^^^
//│ ▼ 
//│ ◉ ('a) is assumed here
//│ ▲  - l.1  let boolToBit x = if x then "0" else 1
//│ │                           ^^^^^^^^^^^^^^^^^^^^
//│ │ 
//│ ◉ (int) is here
//│    - l.1  let boolToBit x = if x then "0" else 1
//│                                                ^
//│ U max: 4, total: 7
//│ UERR 1 errors
//│ L: 0 [string ~ [[int]], [[string]] <: α68', α68' :> [[int]]]
