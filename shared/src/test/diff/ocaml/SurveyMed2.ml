let remainder x y = if (x * y) > 10 then (x * y) mod 10 else 0
  
let strings_of l = List.map string_of_int
  
let y = [1; 2; 3]
  
let rec mulByDigit i l =
  match List.rev l with
  | [] -> []
  | h::t -> [remainder strings_of y] @ (mulByDigit i t)
//│ U max: 54, total: 148
//│ UERR 2 errors
//│ L: 0 [list['a117'] ~ [[[int]]], [[[[list['a117']]]]] <: α111', α111' <: [[[int]]]]
//│ L: 0 [(α113' -> [(list[α114'] -> list[α115'])]) ~ [[[int]]], [[[(α113' -> [(list[α114'] -> list[α115'])])]]] <: α110', α110' <: [[[int]]]]

