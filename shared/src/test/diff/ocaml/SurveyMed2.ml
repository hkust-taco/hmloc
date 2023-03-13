let remainder x y = if (x * y) > 10 then mod (x * y) 10 else 0
  
let x l = List.map string_of_int
  
let y = [1, 2, 3]
  
let rec mulByDigit i l =
  match List.rev l with
  | [] -> []
  | h::t -> [remainder x y] @ (mulByDigit i t)
