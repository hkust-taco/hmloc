let wrap x = x :: []
 
let test z cond = if cond
 then wrap z
 else wrap true
 
let rec check cond =
 test (if cond then false else check (not cond)) cond
//│ ╔══[ERROR] Type `bool` does not match `_ list`
//│ ║  
//│ ╟──        (bool) ---> (?a) <--- (_ list) 
//│ ║  
//│ ╟── (bool) is the type of this `then` branch and it flows into `?a`
//│ ║  l.8:	 test (if cond then false else check (not cond)) cond
//│ ║      	                    ^^^^^
//│ ╟── (?a) is assumed as the type of this if-then-else expression
//│ ║  l.8:	 test (if cond then false else check (not cond)) cond
//│ ║      	      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── so this `else` branch has type `?a`
//│ ║  l.8:	 test (if cond then false else check (not cond)) cond
//│ ║      	                               ^^^^^^^^^^^^^^^^
//│ ╟── so this application has type `?a`
//│ ║  l.8:	 test (if cond then false else check (not cond)) cond
//│ ║      	 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── so this if-then-else expression has type `?a`
//│ ║  l.3:	let test z cond = if cond
//│ ║      	                  ^^^^^^^
//│ ║      	 then wrap z ...
//│ ║      	 ^^^^^^^^^^^^^^^
//│ ╟── so this `then` branch has type `?a`. However `_ list` flows into `?a`
//│ ║  l.4:	 then wrap z
//│ ║      	      ^^^^^^
//│ ╟── (_ list) is assumed as the type of this operator application
//│ ║  l.1:	let wrap x = x :: []
//│ ╙──    	             ^^^^^^^
//│ ╔══[ERROR] Type `bool` does not match `_ list`
//│ ║  
//│ ╟──        (bool) ---> (?a) <--- (_ list) 
//│ ║  
//│ ╟── (bool) is the type of this `then` branch and it flows into `?a`
//│ ║  l.8:	 test (if cond then false else check (not cond)) cond
//│ ║      	                    ^^^^^
//│ ╟── (?a) is assumed as the type of this if-then-else expression
//│ ║  l.8:	 test (if cond then false else check (not cond)) cond
//│ ║      	      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── so this `else` branch has type `?a`
//│ ║  l.8:	 test (if cond then false else check (not cond)) cond
//│ ║      	                               ^^^^^^^^^^^^^^^^
//│ ╟── so this application has type `?a`
//│ ║  l.8:	 test (if cond then false else check (not cond)) cond
//│ ║      	 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── so this if-then-else expression has type `?a`
//│ ║  l.3:	let test z cond = if cond
//│ ║      	                  ^^^^^^^
//│ ║      	 then wrap z ...
//│ ║      	 ^^^^^^^^^^^^^^^
//│ ╟── so this `else` branch has type `?a`. However `_ list` flows into `?a`
//│ ║  l.5:	 else wrap true
//│ ║      	      ^^^^^^^^^
//│ ╟── (_ list) is assumed as the type of this operator application
//│ ║  l.1:	let wrap x = x :: []
//│ ╙──    	             ^^^^^^^

