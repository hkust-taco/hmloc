let remainder x y = if (x * y) > 10 then (x * y) mod 10 else 0
  
let strings_of l = List.map string_of_int
  
let y = [1; 2; 3]
  
let rec mulByDigit i l =
  match List.rev l with
  | [] -> []
  | h::t -> [remainder strings_of y] @ (mulByDigit i t)

