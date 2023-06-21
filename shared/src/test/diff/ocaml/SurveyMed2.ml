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
//│ ◉ (_ -> _ list -> _ list) comes from
//│ │  - l.3  let strings_of l = List.map string_of_int
//│ │                        ^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │  - l.10   | h::t -> [remainder strings_of y] @ (mulByDigit i t)
//│ │                                ^^^^^^^^^^
//│ │  - l.1  let remainder x y = if (x * y) > 10 then (x * y) mod 10 else 0
//│ │                       ^
//│ ▼ 
//│ ◉ (?a) is assumed for
//│ │  - l.1  let remainder x y = if (x * y) > 10 then (x * y) mod 10 else 0
//│ │                       ^
//│ │  - l.1  let remainder x y = if (x * y) > 10 then (x * y) mod 10 else 0
//│ │                                 ^
//│ ▼ 
//│ ◉ (int) comes from
//│    - lib. let ( * ): int -> int -> int
//│                      ^^^
//│ [ERROR] Type `_ list` does not match `int`
//│ 
//│         (_ list) ---> (?a) ---> (int)
//│ 
//│ ◉ (_ list) comes from
//│ │  - l.5  let y = [1; 2; 3]
//│ │                 ^^^^^^^^^
//│ │  - l.10   | h::t -> [remainder strings_of y] @ (mulByDigit i t)
//│ │                                           ^
//│ │  - l.1  let remainder x y = if (x * y) > 10 then (x * y) mod 10 else 0
//│ │                         ^
//│ ▼ 
//│ ◉ (?a) is assumed for
//│ │  - l.1  let remainder x y = if (x * y) > 10 then (x * y) mod 10 else 0
//│ │                         ^
//│ │  - l.1  let remainder x y = if (x * y) > 10 then (x * y) mod 10 else 0
//│ │                                     ^
//│ ▼ 
//│ ◉ (int) comes from
//│    - lib. let ( * ): int -> int -> int
//│                             ^^^
//│ U max: 7, total: 100
//│ UERR 2 errors
//│ L: 0 [(α101' -> [(list[α102'] -> list[α103'])]) ~ int, (α101' -> [(list[α102'] -> list[α103'])]) <: α98', α98' <: int]
//│ L: 0 [list['a105'] ~ int, list['a105'] <: α99', α99' <: int]

