let wrap x = x :: []
let test z = if true then wrap z else wrap true
let rec t = test (if true then 1 else t)
//│ ╔══[ERROR] Type `_ list` does not match `int`
//│ ║  
//│ ╟──        _ list ---> ?a <--- int 
//│ ║  
//│ ╟── [`_ list`] comes from this application
//│ ║  l.1:	let wrap x = x :: []
//│ ║      	             ^^^^^^^
//│ ╟── so this `else` branch has type `_ list`
//│ ║  l.2:	let test z = if true then wrap z else wrap true
//│ ║      	                                      ^^^^^^^^^
//│ ╟── so this if-then-else expression has type `_ list`
//│ ║  l.2:	let test z = if true then wrap z else wrap true
//│ ║      	             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── so this application has type `_ list`
//│ ║  l.3:	let rec t = test (if true then 1 else t)
//│ ║      	            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── so this recursive binding has type `_ list`
//│ ║  l.3:	let rec t = test (if true then 1 else t)
//│ ║      	        ^
//│ ╟── so this `else` branch has type `_ list` and it flows into `?a`
//│ ║  l.3:	let rec t = test (if true then 1 else t)
//│ ║      	                                      ^
//│ ╟── [`?a`] comes from this if-then-else expression
//│ ║  l.3:	let rec t = test (if true then 1 else t)
//│ ║      	                 ^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── [`int`] comes from this `then` branch and it flows into `?a`
//│ ║  l.3:	let rec t = test (if true then 1 else t)
//│ ╙──    	                               ^
//│ ╔══[ERROR] Type `_ list` does not match `int`
//│ ║  
//│ ╟──        _ list ---> ?a <--- int 
//│ ║  
//│ ╟── [`_ list`] comes from this application
//│ ║  l.1:	let wrap x = x :: []
//│ ║      	             ^^^^^^^
//│ ╟── so this `then` branch has type `_ list`
//│ ║  l.2:	let test z = if true then wrap z else wrap true
//│ ║      	                          ^^^^^^
//│ ╟── so this if-then-else expression has type `_ list`
//│ ║  l.2:	let test z = if true then wrap z else wrap true
//│ ║      	             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── so this application has type `_ list`
//│ ║  l.3:	let rec t = test (if true then 1 else t)
//│ ║      	            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── so this recursive binding has type `_ list`
//│ ║  l.3:	let rec t = test (if true then 1 else t)
//│ ║      	        ^
//│ ╟── so this `else` branch has type `_ list` and it flows into `?a`
//│ ║  l.3:	let rec t = test (if true then 1 else t)
//│ ║      	                                      ^
//│ ╟── [`?a`] comes from this if-then-else expression
//│ ║  l.3:	let rec t = test (if true then 1 else t)
//│ ║      	                 ^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── [`int`] comes from this `then` branch and it flows into `?a`
//│ ║  l.3:	let rec t = test (if true then 1 else t)
//│ ╙──    	                               ^
