let wrap x = x :: []
 
let test z cond = if cond
 then wrap z
 else wrap true
 
let rec check cond =
 test (if cond then false else check (not cond)) cond
//│ [ERROR] Type `_ list` does not match `bool`
//│ 
//│         (?a list) ---> (?a0) ~~~~ (?a) <--- (bool)
//│ 
//│ ◉ (?a list) is here
//│ │  - l.1  let wrap x = x :: []
//│ │                      ^^^^^^^
//│ │  - l.5   else wrap true
//│ │               ^^^^^^^^^
//│ │  - l.3  let test z cond = if cond
//│ │                           ^^^^^^^
//│ │          then wrap z ...
//│ │          ^^^^^^^^^^^^^^^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │                                        ^^^^^^^^^^^^^^^^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ │  - l.4   then wrap z
//│ │                    ^
//│ │  - l.1  let wrap x = x :: []
//│ │                  ^
//│ ▼ 
//│ ◉ (?a0) is assumed here
//│    - l.1  let wrap x = x :: []
//│                        ^
//│   ◉ (_ list) is here
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │  - l.4   then wrap z
//│   │               ^^^^^^
//│   ▼ 
//│   ◉ (?b) is assumed here
//│      - l.3  let test z cond = if cond
//│                               ^^^^^^^
//│              then wrap z ...
//│              ^^^^^^^^^^^^^^^
//│     ◉ (_ -> _) is here
//│     │  - l.7  let rec check cond =
//│     │                       ^^^^^^
//│     │          test (if cond then false else check (not cond)) cond
//│     │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│     │  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     ▼ 
//│     ◉ (?check) is assumed here
//│     │  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     ▼ 
//│     ◉ (bool -> _) is here
//│        - l.8   test (if cond then false else check (not cond)) cond
//│                                              ^^^^^
//│   ◉ (?c) is assumed here
//│   ▲  - l.8   test (if cond then false else check (not cond)) cond
//│   │                                        ^^^^^^^^^^^^^^^^
//│   │  - l.8   test (if cond then false else check (not cond)) cond
//│   │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   │  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   │  - l.5   else wrap true
//│   │               ^^^^^^^^^
//│   │ 
//│   ◉ (_ list) is here
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a) is assumed here
//│ ▲  - l.1  let wrap x = x :: []
//│ │                      ^
//│ │  - l.1  let wrap x = x :: []
//│ │                  ^
//│ │ 
//│ ◉ (bool) is here
//│    - l.5   else wrap true
//│                      ^^^^
//│ [ERROR] Type `bool` does not match `_ list`
//│ 
//│         (bool) ---> (?a) <--- (_ list)
//│ 
//│ ◉ (bool) is here
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │                             ^^^^^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ▼ 
//│ ◉ (?a) is assumed here
//│ ▲  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │                                        ^^^^^^^^^^^^^^^^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │  - l.3  let test z cond = if cond
//│ │                           ^^^^^^^
//│ │          then wrap z ...
//│ │          ^^^^^^^^^^^^^^^
//│ │  - l.4   then wrap z
//│ │               ^^^^^^
//│ │ 
//│ ◉ (_ list) is here
//│    - l.1  let wrap x = x :: []
//│                        ^^^^^^^
//│ [ERROR] Type `bool` does not match `_ list`
//│ 
//│         (bool) ---> (?a) ~~~~ (?a0) <--- (?a list)
//│ 
//│ ◉ (bool) is here
//│ │  - l.5   else wrap true
//│ │                    ^^^^
//│ │  - l.1  let wrap x = x :: []
//│ │                  ^
//│ ▼ 
//│ ◉ (?a) is assumed here
//│    - l.1  let wrap x = x :: []
//│                        ^
//│   ◉ (_ list) is here
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │  - l.5   else wrap true
//│   │               ^^^^^^^^^
//│   │  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   │  - l.8   test (if cond then false else check (not cond)) cond
//│   │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   │  - l.8   test (if cond then false else check (not cond)) cond
//│   │                                        ^^^^^^^^^^^^^^^^
//│   ▼ 
//│   ◉ (?b) is assumed here
//│   ▲  - l.8   test (if cond then false else check (not cond)) cond
//│   │                                        ^^^^^^^^^^^^^^^^
//│   │  - l.8   test (if cond then false else check (not cond)) cond
//│   │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   │  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   │  - l.4   then wrap z
//│   │               ^^^^^^
//│   │ 
//│   ◉ (_ list) is here
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a0) is assumed here
//│ ▲  - l.1  let wrap x = x :: []
//│ │                      ^
//│ │  - l.1  let wrap x = x :: []
//│ │                  ^
//│ │  - l.4   then wrap z
//│ │                    ^
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │                                        ^^^^^^^^^^^^^^^^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │  - l.3  let test z cond = if cond
//│ │                           ^^^^^^^
//│ │          then wrap z ...
//│ │          ^^^^^^^^^^^^^^^
//│ │  - l.5   else wrap true
//│ │               ^^^^^^^^^
//│ │ 
//│ ◉ (?a list) is here
//│    - l.1  let wrap x = x :: []
//│                        ^^^^^^^
//│ [ERROR] Type `_ list` does not match `bool`
//│ 
//│         (?a list) ---> (?a) ~~~~ (?a0) <--- (bool)
//│ 
//│ ◉ (?a list) is here
//│ │  - l.1  let wrap x = x :: []
//│ │                      ^^^^^^^
//│ │  - l.4   then wrap z
//│ │               ^^^^^^
//│ │  - l.3  let test z cond = if cond
//│ │                           ^^^^^^^
//│ │          then wrap z ...
//│ │          ^^^^^^^^^^^^^^^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │                                        ^^^^^^^^^^^^^^^^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ │  - l.4   then wrap z
//│ │                    ^
//│ │  - l.1  let wrap x = x :: []
//│ │                  ^
//│ ▼ 
//│ ◉ (?a) is assumed here
//│    - l.1  let wrap x = x :: []
//│                        ^
//│   ◉ (_ list) is here
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │  - l.4   then wrap z
//│   │               ^^^^^^
//│   ▼ 
//│   ◉ (?b) is assumed here
//│      - l.3  let test z cond = if cond
//│                               ^^^^^^^
//│              then wrap z ...
//│              ^^^^^^^^^^^^^^^
//│     ◉ (_ -> _) is here
//│     │  - l.7  let rec check cond =
//│     │                       ^^^^^^
//│     │          test (if cond then false else check (not cond)) cond
//│     │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│     │  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     ▼ 
//│     ◉ (?check) is assumed here
//│     │  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     ▼ 
//│     ◉ (bool -> _) is here
//│        - l.8   test (if cond then false else check (not cond)) cond
//│                                              ^^^^^
//│   ◉ (?c) is assumed here
//│   ▲  - l.8   test (if cond then false else check (not cond)) cond
//│   │                                        ^^^^^^^^^^^^^^^^
//│   │  - l.8   test (if cond then false else check (not cond)) cond
//│   │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   │  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   │  - l.5   else wrap true
//│   │               ^^^^^^^^^
//│   │ 
//│   ◉ (_ list) is here
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a0) is assumed here
//│ ▲  - l.1  let wrap x = x :: []
//│ │                      ^
//│ │  - l.1  let wrap x = x :: []
//│ │                  ^
//│ │ 
//│ ◉ (bool) is here
//│    - l.5   else wrap true
//│                      ^^^^
//│ [ERROR] Type `bool` does not match `_ list`
//│ 
//│         (bool) ---> (?a) ~~~~ (?a0) <--- (?a0 list)
//│ 
//│ ◉ (bool) is here
//│ │  - l.5   else wrap true
//│ │                    ^^^^
//│ │  - l.1  let wrap x = x :: []
//│ │                  ^
//│ ▼ 
//│ ◉ (?a) is assumed here
//│    - l.1  let wrap x = x :: []
//│                        ^
//│   ◉ (_ list) is here
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │  - l.5   else wrap true
//│   │               ^^^^^^^^^
//│   │  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   │  - l.8   test (if cond then false else check (not cond)) cond
//│   │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   │  - l.8   test (if cond then false else check (not cond)) cond
//│   │                                        ^^^^^^^^^^^^^^^^
//│   ▼ 
//│   ◉ (?b) is assumed here
//│   ▲  - l.8   test (if cond then false else check (not cond)) cond
//│   │                                        ^^^^^^^^^^^^^^^^
//│   │  - l.8   test (if cond then false else check (not cond)) cond
//│   │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   │  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   │  - l.4   then wrap z
//│   │               ^^^^^^
//│   │ 
//│   ◉ (_ list) is here
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a0) is assumed here
//│ ▲  - l.1  let wrap x = x :: []
//│ │                      ^
//│ │  - l.1  let wrap x = x :: []
//│ │                  ^
//│ │  - l.4   then wrap z
//│ │                    ^
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │                                        ^^^^^^^^^^^^^^^^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │  - l.3  let test z cond = if cond
//│ │                           ^^^^^^^
//│ │          then wrap z ...
//│ │          ^^^^^^^^^^^^^^^
//│ │  - l.4   then wrap z
//│ │               ^^^^^^
//│ │ 
//│ ◉ (?a0 list) is here
//│    - l.1  let wrap x = x :: []
//│                        ^^^^^^^
//│ [ERROR] Type `bool` does not match `_ list`
//│ 
//│         (bool) ---> (?a) <--- (_ list)
//│ 
//│ ◉ (bool) is here
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │                             ^^^^^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ▼ 
//│ ◉ (?a) is assumed here
//│ ▲  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │                                        ^^^^^^^^^^^^^^^^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │  - l.3  let test z cond = if cond
//│ │                           ^^^^^^^
//│ │          then wrap z ...
//│ │          ^^^^^^^^^^^^^^^
//│ │  - l.5   else wrap true
//│ │               ^^^^^^^^^
//│ │ 
//│ ◉ (_ list) is here
//│    - l.1  let wrap x = x :: []
//│                        ^^^^^^^
//│ U max: 59, total: 154
//│ UERR 6 errors
//│ L: 2 [list['a106'] ~ bool, list['a106'] <: 'a101', ['a101' - list['a101'] ~ list['a106'] - 'a106', L: 1 [list['a101'] ~ list['a106'], list['a101'] <: α105', [α105' - (α99' -> [α105']) ~ ([bool] -> α112') - α112', L: 0 [(α99' -> [α105']) ~ ([bool] -> α112'), (α99' -> [α105']) <: check98', check98' <: ([bool] -> α112')]], α112' :> list['a106']]], 'a106' :> bool]
//│ L: 1 [bool ~ list['a106'], bool <: 'a106', ['a106' - list['a106'] ~ list['a101'] - 'a101', L: 0 [list['a106'] ~ list['a101'], list['a106'] <: α112', α112' :> list['a101']]], 'a101' :> list['a106']]
//│ L: 2 [list['a101'] ~ bool, list['a101'] <: 'a101', ['a101' - list['a101'] ~ list['a106'] - 'a106', L: 1 [list['a101'] ~ list['a106'], list['a101'] <: α105', [α105' - (α99' -> [α105']) ~ ([bool] -> α112') - α112', L: 0 [(α99' -> [α105']) ~ ([bool] -> α112'), (α99' -> [α105']) <: check98', check98' <: ([bool] -> α112')]], α112' :> list['a106']]], 'a106' :> bool]
//│ L: 1 [bool ~ list['a101'], bool <: 'a106', ['a106' - list['a106'] ~ list['a101'] - 'a101', L: 0 [list['a106'] ~ list['a101'], list['a106'] <: α112', α112' :> list['a101']]], 'a101' :> list['a101']]
//│ L: 0 [bool ~ list['a101'], bool <: α109', α109' :> list['a101']]
//│ L: 0 [bool ~ list['a106'], bool <: α109', α109' :> list['a106']]

