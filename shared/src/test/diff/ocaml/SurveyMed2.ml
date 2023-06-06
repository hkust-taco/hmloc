let remainder x y = if (x * y) > 10 then (x * y) mod 10 else 0
  
let strings_of l = List.map string_of_int
  
let y = [1; 2; 3]
  
let rec mulByDigit i l =
  match List.rev l with
  | [] -> []
  | h::t -> [remainder strings_of y] @ (mulByDigit i t)
//│ [ERROR] Type `_ -> _ list -> _ list` does not match `int`
//│ 
//│         (_ -> _ list -> _ list) ---> (?a) ---> (int)
//│ 
//│ ◉ (_ -> _ list -> _ list) is here
//│ │  - l.3  let strings_of l = List.map string_of_int
//│ │                        ^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │  - l.10   | h::t -> [remainder strings_of y] @ (mulByDigit i t)
//│ │                                ^^^^^^^^^^
//│ │  - l.1  let remainder x y = if (x * y) > 10 then (x * y) mod 10 else 0
//│ │                       ^
//│ ▼ 
//│ ◉ (?a) is assumed here
//│ │  - l.1  let remainder x y = if (x * y) > 10 then (x * y) mod 10 else 0
//│ │                       ^
//│ │  - l.1  let remainder x y = if (x * y) > 10 then (x * y) mod 10 else 0
//│ │                                 ^
//│ ▼ 
//│ ◉ (int) is here
//│    - lib. let ( * ): int -> int -> int
//│                      ^^^
//│ [ERROR] Type `_ list` does not match `int`
//│ 
//│         (_ list) ---> (?a) ---> (int)
//│ 
//│ ◉ (_ list) is here
//│ │  - l.5  let y = [1; 2; 3]
//│ │                 ^^^^^^^^^
//│ │  - l.10   | h::t -> [remainder strings_of y] @ (mulByDigit i t)
//│ │                                           ^
//│ │  - l.1  let remainder x y = if (x * y) > 10 then (x * y) mod 10 else 0
//│ │                         ^
//│ ▼ 
//│ ◉ (?a) is assumed here
//│ │  - l.1  let remainder x y = if (x * y) > 10 then (x * y) mod 10 else 0
//│ │                         ^
//│ │  - l.1  let remainder x y = if (x * y) > 10 then (x * y) mod 10 else 0
//│ │                                     ^
//│ ▼ 
//│ ◉ (int) is here
//│    - lib. let ( * ): int -> int -> int
//│                             ^^^
//│ U max: 7, total: 100
//│ UERR 2 errors
//│ L: 0 [list['a108'] ~ int, list['a108'] <: α102', α102' <: int]
//│ L: 0 [(α104' -> [(list[α105'] -> list[α106'])]) ~ int, (α104' -> [(list[α105'] -> list[α106'])]) <: α101', α101' <: int]

