:p
:w
data type Either l r of
  Left l
  Right r
//│ Defined type alias Either[+l, +r]
//│ Defined class Left[+l, ±r]
//│ Defined class Right[±l, +r]
//│ ╔══[WARNING] Type definition Left has bivariant type parameters:
//│ ║  l.4: 	  Left l
//│ ║       	  ^^^^
//│ ╟── r is irrelevant and may be removed
//│ ║  l.3: 	data type Either l r of
//│ ╙──     	                   ^
//│ ╔══[WARNING] Type definition Right has bivariant type parameters:
//│ ║  l.5: 	  Right r
//│ ║       	  ^^^^^
//│ ╟── l is irrelevant and may be removed
//│ ║  l.3: 	data type Either l r of
//│ ╙──     	                 ^
//│ Left: 'a -> Left['a, ?]
//│ Right: 'a -> Right[?, 'a]





:e
data type Either2 (l: _) (r: _) of
  Left2 l
  Right2 r
//│ ╔══[ERROR] illegal datatype type parameter shape: '(' {l: _,} ')'
//│ ║  l.29: 	data type Either2 (l: _) (r: _) of
//│ ╙──      	                  ^^^^^^
//│ ╔══[ERROR] illegal datatype type parameter shape: '(' {r: _,} ')'
//│ ║  l.29: 	data type Either2 (l: _) (r: _) of
//│ ╙──      	                         ^^^^^^
//│ ╔══[ERROR] type identifier not found: l
//│ ║  l.30: 	  Left2 l
//│ ╙──      	        ^
//│ ╔══[ERROR] type identifier not found: r
//│ ║  l.31: 	  Right2 r
//│ ╙──      	         ^
//│ Defined type alias Either2
//│ Defined class Left2[+l]
//│ Defined class Right2[+r]
//│ Left2: 'a -> Left2['a]
//│ Right2: 'a -> Right2['a]





let l = Left 1
let r = Right "ok"
let e = if _ then l else r
//│ l: Left[int, ?]
//│ r: Right[?, string]
//│ e: Left[int, ?] | Right[?, string]

:e // TODO
e as Either Int String
//│ ╔══[ERROR] Unsupported pattern shape:
//│ ║  l.62: 	e as Either Int String
//│ ╙──      	     ^^^^^^^^^^^^^^^^^
//│ res: error


// TODO
// e as (_: Either Int String)
// e as (_: Either (L: Int) (R: String))

:e
e as Either
//│ ╔══[ERROR] identifier not found: Either
//│ ║  l.74: 	e as Either
//│ ╙──      	     ^^^^^^
//│ res: error



