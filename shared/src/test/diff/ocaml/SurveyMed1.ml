let wrap x = x :: []
let test z = if true then wrap z else wrap true
let rec t = test (if true then 1 else t)
//│ ╔══[ERROR] Type `int` does not match `_ list`
//│ ║  
//│ ╟──        int ---> ?a <--- _ list 
//│ ║  
//│ ╟── [`int`] comes from this `then` branch and it flows into `?a`
//│ ║  l.3:	let rec t = test (if true then 1 else t)
//│ ║      	                               ^
//│ ╟── [`?a`] comes from this if-then-else expression
//│ ║  l.3:	let rec t = test (if true then 1 else t)
//│ ║      	                 ^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── so this `else` branch has type `?a`
//│ ║  l.3:	let rec t = test (if true then 1 else t)
//│ ║      	                                      ^
//│ ╟── so this recursive binding has type `?a`
//│ ║  l.3:	let rec t = test (if true then 1 else t)
//│ ║      	        ^
//│ ╟── so this application has type `?a`
//│ ║  l.3:	let rec t = test (if true then 1 else t)
//│ ║      	            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── so this if-then-else expression has type `?a`
//│ ║  l.2:	let test z = if true then wrap z else wrap true
//│ ║      	             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── so this `else` branch has type `?a`. However `_ list` flows into `?a`
//│ ║  l.2:	let test z = if true then wrap z else wrap true
//│ ║      	                                      ^^^^^^^^^
//│ ╟── [`_ list`] comes from this application
//│ ║  l.1:	let wrap x = x :: []
//│ ╙──    	             ^^^^^^^
//│ ╔══[ERROR] Type `int` does not match `_ list`
//│ ║  
//│ ╟──        int ---> ?a <--- _ list 
//│ ║  
//│ ╟── [`int`] comes from this `then` branch and it flows into `?a`
//│ ║  l.3:	let rec t = test (if true then 1 else t)
//│ ║      	                               ^
//│ ╟── [`?a`] comes from this if-then-else expression
//│ ║  l.3:	let rec t = test (if true then 1 else t)
//│ ║      	                 ^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── so this `else` branch has type `?a`
//│ ║  l.3:	let rec t = test (if true then 1 else t)
//│ ║      	                                      ^
//│ ╟── so this recursive binding has type `?a`
//│ ║  l.3:	let rec t = test (if true then 1 else t)
//│ ║      	        ^
//│ ╟── so this application has type `?a`
//│ ║  l.3:	let rec t = test (if true then 1 else t)
//│ ║      	            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── so this if-then-else expression has type `?a`
//│ ║  l.2:	let test z = if true then wrap z else wrap true
//│ ║      	             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ╟── so this `then` branch has type `?a`. However `_ list` flows into `?a`
//│ ║  l.2:	let test z = if true then wrap z else wrap true
//│ ║      	                          ^^^^^^
//│ ╟── [`_ list`] comes from this application
//│ ║  l.1:	let wrap x = x :: []
//│ ╙──    	             ^^^^^^^
