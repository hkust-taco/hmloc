let rec sepConcat sep sl =
  match sl with
  | [] -> ""
  | h::t ->
      let f a x = a ^ (sep ^ x) in
      let base = h in let l = t in List.fold_left f base l
 
let stringOfList f l = "[" ^ ((sepConcat "; " List.map (f, l)) ^ "]")
 
(* (^): string -> string -> string is a string concatenation operator *)
(* List.fold_left: ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a *)
//│ U max: 56, total: 194
//│ UERR 6 errors
//│ L: 1 [string ~ [[(([α92'], [α93'],) -> α111')]], [[string]] <: α99', [α97' - (α97' -> α99') ~ ([((α106' -> α107') -> (list[α106'] -> list[α107']))] -> α109') - [((α106' -> α107') -> (list[α106'] -> list[α107']))], [[[(α97' -> α99')]]] <: α105', α105' <: [[([((α106' -> α107') -> (list[α106'] -> list[α107']))] -> α109')]]], α109' <: [[(([α92'], [α93'],) -> α111')]]]
//│ L: 2 [string ~ [[(([α92'], [α93'],) -> α111')]], [[string]] <: α99', [α97' - (α97' -> α99') ~ ([((α106' -> α107') -> (list[α106'] -> list[α107']))] -> α109') - [((α106' -> α107') -> (list[α106'] -> list[α107']))], [α96' - (α96' -> (α97' -> α99')) ~ (string -> α105') - string, [[(α96' -> (α97' -> α99'))]] <: sepConcat95', sepConcat95' <: [[[(string -> α105')]]]], α105' <: [[([((α106' -> α107') -> (list[α106'] -> list[α107']))] -> α109')]]], α109' <: [[(([α92'], [α93'],) -> α111')]]]
//│ L: 2 [[[[[list['a98']]]]] ~ ((α106' -> α107') -> (list[α106'] -> list[α107'])), α97' <: [[[[list['a98']]]]], [α97' - (α97' -> α99') ~ ([((α106' -> α107') -> (list[α106'] -> list[α107']))] -> α109') - [((α106' -> α107') -> (list[α106'] -> list[α107']))], [α96' - (α96' -> (α97' -> α99')) ~ (string -> α105') - string, [[(α96' -> (α97' -> α99'))]] <: sepConcat95', sepConcat95' <: [[[(string -> α105')]]]], α105' <: [[([((α106' -> α107') -> (list[α106'] -> list[α107']))] -> α109')]]]]
//│ L: 0 [string ~ [[(([α92'], [α93'],) -> α111')]], [[[[[[[string]]]]]]] <: α109', α109' <: [[(([α92'], [α93'],) -> α111')]]]
//│ L: 0 [((α106' -> α107') -> (list[α106'] -> list[α107'])) ~ [[[[list['a98']]]]], [[[[((α106' -> α107') -> (list[α106'] -> list[α107']))]]]] <: α97', α97' <: [[[[list['a98']]]]]]
//│ L: 1 [[[[[list['a98']]]]] ~ ((α106' -> α107') -> (list[α106'] -> list[α107'])), α97' <: [[[[list['a98']]]]], [α97' - (α97' -> α99') ~ ([((α106' -> α107') -> (list[α106'] -> list[α107']))] -> α109') - [((α106' -> α107') -> (list[α106'] -> list[α107']))], [[[(α97' -> α99')]]] <: α105', α105' <: [[([((α106' -> α107') -> (list[α106'] -> list[α107']))] -> α109')]]]]
