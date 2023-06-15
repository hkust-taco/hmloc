let wrap x = x :: []
 
let test z cond = if cond
 then wrap z
 else wrap true
 
let rec check cond =
 test (if cond then false else check (not cond)) cond
//│ [ERROR] Type `_ list` does not match `bool`
//│ 
//│         (_ list) ---> (?b) ~~~~ (?c) ---> (?d) ---> (?e) ---> (?f) ~~~~ (?a) ~~~~ (?f) <--- (?e) <--- (?d) <--- (bool)
//│ 
//│ ◉ (_ list) is here
//│ │  - l.1  let wrap x = x :: []
//│ │                      ^^^^^^^
//│ │  - l.5   else wrap true
//│ │               ^^^^^^^^^
//│ ▼ 
//│ ◉ (?b) is assumed here
//│    - l.3  let test z cond = if cond
//│                             ^^^^^^^
//│            then wrap z ...
//│            ^^^^^^^^^^^^^^^
//│   ◉ (_ -> ?b) is here
//│   │  - l.7  let rec check cond =
//│   │                       ^^^^^^
//│   │          test (if cond then false else check (not cond)) cond
//│   │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   │  - l.7  let rec check cond =
//│   │                 ^^^^^
//│   ▼ 
//│   ◉ (?check) is assumed here
//│   │  - l.7  let rec check cond =
//│   │                 ^^^^^
//│   ▼ 
//│   ◉ (bool -> ?c) is here
//│      - l.8   test (if cond then false else check (not cond)) cond
//│                                            ^^^^^
//│ ◉ (?c) is assumed here
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │                                        ^^^^^^^^^^^^^^^^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ▼ 
//│ ◉ (?d) is assumed here
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ ▼ 
//│ ◉ (?e) is assumed here
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ │  - l.4   then wrap z
//│ │                    ^
//│ ▼ 
//│ ◉ (?f) is assumed here
//│    - l.1  let wrap x = x :: []
//│                    ^
//│   ◉ (?f * _ list) is here
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   ▼ 
//│   ◉ (?a * ?a list) is here
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a) is assumed here
//│   ◉ (?a * ?a list) is here
//│   ▲  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │ 
//│   ◉ (?f * _ list) is here
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?f) is assumed here
//│ ▲  - l.1  let wrap x = x :: []
//│ │                  ^
//│ │  - l.4   then wrap z
//│ │                    ^
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ │ 
//│ ◉ (?e) is assumed here
//│ ▲  - l.3  let test z cond = if cond
//│ │                  ^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │ 
//│ ◉ (?d) is assumed here
//│ ▲  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │ 
//│ ◉ (bool) is here
//│    - l.8   test (if cond then false else check (not cond)) cond
//│                               ^^^^^
//│ [ERROR] Type `_ list` does not match `bool`
//│ 
//│         (?a list) ---> (?b) ~~~~ (?c) ---> (?d) ---> (?e) ---> (?f) ~~~~ (?a) ~~~~ (?f) <--- (?e) <--- (?d) <--- (bool)
//│ 
//│ ◉ (?a list) is here
//│ │  - l.1  let wrap x = x :: []
//│ │                      ^^^^^^^
//│ │  - l.4   then wrap z
//│ │               ^^^^^^
//│ ▼ 
//│ ◉ (?b) is assumed here
//│    - l.3  let test z cond = if cond
//│                             ^^^^^^^
//│            then wrap z ...
//│            ^^^^^^^^^^^^^^^
//│   ◉ (_ -> ?b) is here
//│   │  - l.7  let rec check cond =
//│   │                       ^^^^^^
//│   │          test (if cond then false else check (not cond)) cond
//│   │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   │  - l.7  let rec check cond =
//│   │                 ^^^^^
//│   ▼ 
//│   ◉ (?check) is assumed here
//│   │  - l.7  let rec check cond =
//│   │                 ^^^^^
//│   ▼ 
//│   ◉ (bool -> ?c) is here
//│      - l.8   test (if cond then false else check (not cond)) cond
//│                                            ^^^^^
//│ ◉ (?c) is assumed here
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │                                        ^^^^^^^^^^^^^^^^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ▼ 
//│ ◉ (?d) is assumed here
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ ▼ 
//│ ◉ (?e) is assumed here
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ │  - l.4   then wrap z
//│ │                    ^
//│ ▼ 
//│ ◉ (?f) is assumed here
//│    - l.1  let wrap x = x :: []
//│                    ^
//│   ◉ (?f * _ list) is here
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   ▼ 
//│   ◉ (?a * ?a list) is here
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a) is assumed here
//│   ◉ (?a * ?a list) is here
//│   ▲  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │ 
//│   ◉ (?f * _ list) is here
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?f) is assumed here
//│ ▲  - l.1  let wrap x = x :: []
//│ │                  ^
//│ │  - l.4   then wrap z
//│ │                    ^
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ │ 
//│ ◉ (?e) is assumed here
//│ ▲  - l.3  let test z cond = if cond
//│ │                  ^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │ 
//│ ◉ (?d) is assumed here
//│ ▲  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │ 
//│ ◉ (bool) is here
//│    - l.8   test (if cond then false else check (not cond)) cond
//│                               ^^^^^
//│ [ERROR] Type `_ list` does not match `bool`
//│ 
//│         (?a list) ---> (?b) ~~~~ (?c) ---> (?d) ---> (?e) ---> (?f) ~~~~ (?a0) ~~~~ (?a) ~~~~ (?a1) ~~~~ (?a) ~~~~ (?g) ~~~~ (?a) ~~~~ (?a1) ~~~~ (?a) ~~~~ (?a0) ~~~~ (?f) <--- (?e) <--- (?d) <--- (bool)
//│ 
//│ ◉ (?a list) is here
//│ │  - l.1  let wrap x = x :: []
//│ │                      ^^^^^^^
//│ │  - l.5   else wrap true
//│ │               ^^^^^^^^^
//│ ▼ 
//│ ◉ (?b) is assumed here
//│    - l.3  let test z cond = if cond
//│                             ^^^^^^^
//│            then wrap z ...
//│            ^^^^^^^^^^^^^^^
//│   ◉ (_ -> ?b) is here
//│   │  - l.7  let rec check cond =
//│   │                       ^^^^^^
//│   │          test (if cond then false else check (not cond)) cond
//│   │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   │  - l.7  let rec check cond =
//│   │                 ^^^^^
//│   ▼ 
//│   ◉ (?check) is assumed here
//│   │  - l.7  let rec check cond =
//│   │                 ^^^^^
//│   ▼ 
//│   ◉ (bool -> ?c) is here
//│      - l.8   test (if cond then false else check (not cond)) cond
//│                                            ^^^^^
//│ ◉ (?c) is assumed here
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │                                        ^^^^^^^^^^^^^^^^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ▼ 
//│ ◉ (?d) is assumed here
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ ▼ 
//│ ◉ (?e) is assumed here
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ │  - l.4   then wrap z
//│ │                    ^
//│ ▼ 
//│ ◉ (?f) is assumed here
//│    - l.1  let wrap x = x :: []
//│                    ^
//│   ◉ (?f * _ list) is here
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   ▼ 
//│   ◉ (?a0 * ?a0 list) is here
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a0) is assumed here
//│   ◉ (?a0 list) is here
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │  - l.4   then wrap z
//│   │               ^^^^^^
//│   │  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   ▼ 
//│   ◉ (?b) is assumed here
//│   ▲  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   │  - l.5   else wrap true
//│   │               ^^^^^^^^^
//│   │ 
//│   ◉ (?a list) is here
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a) is assumed here
//│   ◉ (?a list) is here
//│     ◉ (?a * ?a list) is here
//│     ▲  - l.1  let wrap x = x :: []
//│     │                      ^^^^^^^
//│     │ 
//│     ◉ (?g * ?a1 list) is here
//│        - l.1  let wrap x = x :: []
//│                            ^^^^^^^
//│   ◉ (?a1 list) is here
//│      - l.1  let wrap x = x :: []
//│                               ^^
//│ ◉ (?a1) is assumed here
//│   ◉ (?a1 list) is here
//│      - l.1  let wrap x = x :: []
//│                               ^^
//│     ◉ (?g * ?a1 list) is here
//│     │  - l.1  let wrap x = x :: []
//│     │                      ^^^^^^^
//│     ▼ 
//│     ◉ (?a * ?a list) is here
//│        - l.1  let wrap x = x :: []
//│                            ^^^^^^^
//│   ◉ (?a list) is here
//│ ◉ (?a) is assumed here
//│   ◉ (?a * ?a list) is here
//│   ▲  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │ 
//│   ◉ (?g * ?a1 list) is here
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?g) is assumed here
//│    - l.1  let wrap x = x :: []
//│                        ^
//│    - l.1  let wrap x = x :: []
//│                    ^
//│   ◉ (?g * ?a1 list) is here
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   ▼ 
//│   ◉ (?a * ?a list) is here
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a) is assumed here
//│   ◉ (?a list) is here
//│     ◉ (?a * ?a list) is here
//│     ▲  - l.1  let wrap x = x :: []
//│     │                      ^^^^^^^
//│     │ 
//│     ◉ (?g * ?a1 list) is here
//│        - l.1  let wrap x = x :: []
//│                            ^^^^^^^
//│   ◉ (?a1 list) is here
//│      - l.1  let wrap x = x :: []
//│                               ^^
//│ ◉ (?a1) is assumed here
//│   ◉ (?a1 list) is here
//│      - l.1  let wrap x = x :: []
//│                               ^^
//│     ◉ (?g * ?a1 list) is here
//│     │  - l.1  let wrap x = x :: []
//│     │                      ^^^^^^^
//│     ▼ 
//│     ◉ (?a * ?a list) is here
//│        - l.1  let wrap x = x :: []
//│                            ^^^^^^^
//│   ◉ (?a list) is here
//│ ◉ (?a) is assumed here
//│   ◉ (?a list) is here
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │  - l.5   else wrap true
//│   │               ^^^^^^^^^
//│   │  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   ▼ 
//│   ◉ (?b) is assumed here
//│   ▲  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   │  - l.4   then wrap z
//│   │               ^^^^^^
//│   │ 
//│   ◉ (?a0 list) is here
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a0) is assumed here
//│   ◉ (?a0 * ?a0 list) is here
//│   ▲  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │ 
//│   ◉ (?f * _ list) is here
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?f) is assumed here
//│ ▲  - l.1  let wrap x = x :: []
//│ │                  ^
//│ │  - l.4   then wrap z
//│ │                    ^
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ │ 
//│ ◉ (?e) is assumed here
//│ ▲  - l.3  let test z cond = if cond
//│ │                  ^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │ 
//│ ◉ (?d) is assumed here
//│ ▲  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │ 
//│ ◉ (bool) is here
//│    - l.8   test (if cond then false else check (not cond)) cond
//│                               ^^^^^
//│ [ERROR] Type `_ list` does not match `bool`
//│ 
//│         (?a list) ---> (?b) ~~~~ (?c) ---> (?d) ---> (?e) ---> (?f) ~~~~ (?a) ~~~~ (?a0) ~~~~ (?a1) ~~~~ (?a0) ~~~~ (?g) ~~~~ (?a0) ~~~~ (?a1) ~~~~ (?a0) ~~~~ (?a) ~~~~ (?f) <--- (?e) <--- (?d) <--- (bool)
//│ 
//│ ◉ (?a list) is here
//│ │  - l.1  let wrap x = x :: []
//│ │                      ^^^^^^^
//│ │  - l.4   then wrap z
//│ │               ^^^^^^
//│ ▼ 
//│ ◉ (?b) is assumed here
//│    - l.3  let test z cond = if cond
//│                             ^^^^^^^
//│            then wrap z ...
//│            ^^^^^^^^^^^^^^^
//│   ◉ (_ -> ?b) is here
//│   │  - l.7  let rec check cond =
//│   │                       ^^^^^^
//│   │          test (if cond then false else check (not cond)) cond
//│   │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   │  - l.7  let rec check cond =
//│   │                 ^^^^^
//│   ▼ 
//│   ◉ (?check) is assumed here
//│   │  - l.7  let rec check cond =
//│   │                 ^^^^^
//│   ▼ 
//│   ◉ (bool -> ?c) is here
//│      - l.8   test (if cond then false else check (not cond)) cond
//│                                            ^^^^^
//│ ◉ (?c) is assumed here
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │                                        ^^^^^^^^^^^^^^^^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ▼ 
//│ ◉ (?d) is assumed here
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ ▼ 
//│ ◉ (?e) is assumed here
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ │  - l.4   then wrap z
//│ │                    ^
//│ ▼ 
//│ ◉ (?f) is assumed here
//│    - l.1  let wrap x = x :: []
//│                    ^
//│   ◉ (?f * _ list) is here
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   ▼ 
//│   ◉ (?a * ?a list) is here
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a) is assumed here
//│   ◉ (?a list) is here
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │  - l.4   then wrap z
//│   │               ^^^^^^
//│   │  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   ▼ 
//│   ◉ (?b) is assumed here
//│   ▲  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   │  - l.5   else wrap true
//│   │               ^^^^^^^^^
//│   │ 
//│   ◉ (?a0 list) is here
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a0) is assumed here
//│   ◉ (?a0 list) is here
//│     ◉ (?a0 * ?a0 list) is here
//│     ▲  - l.1  let wrap x = x :: []
//│     │                      ^^^^^^^
//│     │ 
//│     ◉ (?g * ?a1 list) is here
//│        - l.1  let wrap x = x :: []
//│                            ^^^^^^^
//│   ◉ (?a1 list) is here
//│      - l.1  let wrap x = x :: []
//│                               ^^
//│ ◉ (?a1) is assumed here
//│   ◉ (?a1 list) is here
//│      - l.1  let wrap x = x :: []
//│                               ^^
//│     ◉ (?g * ?a1 list) is here
//│     │  - l.1  let wrap x = x :: []
//│     │                      ^^^^^^^
//│     ▼ 
//│     ◉ (?a0 * ?a0 list) is here
//│        - l.1  let wrap x = x :: []
//│                            ^^^^^^^
//│   ◉ (?a0 list) is here
//│ ◉ (?a0) is assumed here
//│   ◉ (?a0 * ?a0 list) is here
//│   ▲  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │ 
//│   ◉ (?g * ?a1 list) is here
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?g) is assumed here
//│    - l.1  let wrap x = x :: []
//│                        ^
//│    - l.1  let wrap x = x :: []
//│                    ^
//│   ◉ (?g * ?a1 list) is here
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   ▼ 
//│   ◉ (?a0 * ?a0 list) is here
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a0) is assumed here
//│   ◉ (?a0 list) is here
//│     ◉ (?a0 * ?a0 list) is here
//│     ▲  - l.1  let wrap x = x :: []
//│     │                      ^^^^^^^
//│     │ 
//│     ◉ (?g * ?a1 list) is here
//│        - l.1  let wrap x = x :: []
//│                            ^^^^^^^
//│   ◉ (?a1 list) is here
//│      - l.1  let wrap x = x :: []
//│                               ^^
//│ ◉ (?a1) is assumed here
//│   ◉ (?a1 list) is here
//│      - l.1  let wrap x = x :: []
//│                               ^^
//│     ◉ (?g * ?a1 list) is here
//│     │  - l.1  let wrap x = x :: []
//│     │                      ^^^^^^^
//│     ▼ 
//│     ◉ (?a0 * ?a0 list) is here
//│        - l.1  let wrap x = x :: []
//│                            ^^^^^^^
//│   ◉ (?a0 list) is here
//│ ◉ (?a0) is assumed here
//│   ◉ (?a0 list) is here
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │  - l.5   else wrap true
//│   │               ^^^^^^^^^
//│   │  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   ▼ 
//│   ◉ (?b) is assumed here
//│   ▲  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   │  - l.4   then wrap z
//│   │               ^^^^^^
//│   │ 
//│   ◉ (?a list) is here
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a) is assumed here
//│   ◉ (?a * ?a list) is here
//│   ▲  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │ 
//│   ◉ (?f * _ list) is here
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?f) is assumed here
//│ ▲  - l.1  let wrap x = x :: []
//│ │                  ^
//│ │  - l.4   then wrap z
//│ │                    ^
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ │ 
//│ ◉ (?e) is assumed here
//│ ▲  - l.3  let test z cond = if cond
//│ │                  ^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │ 
//│ ◉ (?d) is assumed here
//│ ▲  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │ 
//│ ◉ (bool) is here
//│    - l.8   test (if cond then false else check (not cond)) cond
//│                               ^^^^^
//│ [ERROR] Type `_ list` does not match `bool`
//│ 
//│         (?a list) ---> (?b) ~~~~ (?c) ---> (?d) ---> (?e) ---> (?f) ~~~~ (?a0) ~~~~ (?a) ~~~~ (?a0) ~~~~ (?a) ~~~~ (?a0) ~~~~ (?f) <--- (?e) <--- (?d) <--- (bool)
//│ 
//│ ◉ (?a list) is here
//│ │  - l.1  let wrap x = x :: []
//│ │                      ^^^^^^^
//│ │  - l.5   else wrap true
//│ │               ^^^^^^^^^
//│ ▼ 
//│ ◉ (?b) is assumed here
//│    - l.3  let test z cond = if cond
//│                             ^^^^^^^
//│            then wrap z ...
//│            ^^^^^^^^^^^^^^^
//│   ◉ (_ -> ?b) is here
//│   │  - l.7  let rec check cond =
//│   │                       ^^^^^^
//│   │          test (if cond then false else check (not cond)) cond
//│   │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   │  - l.7  let rec check cond =
//│   │                 ^^^^^
//│   ▼ 
//│   ◉ (?check) is assumed here
//│   │  - l.7  let rec check cond =
//│   │                 ^^^^^
//│   ▼ 
//│   ◉ (bool -> ?c) is here
//│      - l.8   test (if cond then false else check (not cond)) cond
//│                                            ^^^^^
//│ ◉ (?c) is assumed here
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │                                        ^^^^^^^^^^^^^^^^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ▼ 
//│ ◉ (?d) is assumed here
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ ▼ 
//│ ◉ (?e) is assumed here
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ │  - l.4   then wrap z
//│ │                    ^
//│ ▼ 
//│ ◉ (?f) is assumed here
//│    - l.1  let wrap x = x :: []
//│                    ^
//│   ◉ (?f * _ list) is here
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   ▼ 
//│   ◉ (?a0 * ?a0 list) is here
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a0) is assumed here
//│   ◉ (?a0 list) is here
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
//│     ◉ (_ -> ?b) is here
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
//│     ◉ (bool -> ?c) is here
//│        - l.8   test (if cond then false else check (not cond)) cond
//│                                              ^^^^^
//│   ◉ (?c) is assumed here
//│      - l.8   test (if cond then false else check (not cond)) cond
//│                                            ^^^^^^^^^^^^^^^^
//│     ◉ (bool -> ?c) is here
//│     ▲  - l.8   test (if cond then false else check (not cond)) cond
//│     │                                        ^^^^^
//│     │  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     │ 
//│     ◉ (?check) is assumed here
//│     ▲  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     │ 
//│     ◉ (_ -> ?b) is here
//│        - l.7  let rec check cond =
//│                             ^^^^^^
//│                test (if cond then false else check (not cond)) cond
//│                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   ◉ (?b) is assumed here
//│   ▲  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   │  - l.5   else wrap true
//│   │               ^^^^^^^^^
//│   │ 
//│   ◉ (?a list) is here
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a) is assumed here
//│   ◉ (?a list) is here
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │  - l.5   else wrap true
//│   │               ^^^^^^^^^
//│   ▼ 
//│   ◉ (?b) is assumed here
//│      - l.3  let test z cond = if cond
//│                               ^^^^^^^
//│              then wrap z ...
//│              ^^^^^^^^^^^^^^^
//│     ◉ (_ -> ?b) is here
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
//│     ◉ (bool -> ?c) is here
//│        - l.8   test (if cond then false else check (not cond)) cond
//│                                              ^^^^^
//│   ◉ (?c) is assumed here
//│      - l.8   test (if cond then false else check (not cond)) cond
//│                                            ^^^^^^^^^^^^^^^^
//│     ◉ (bool -> ?c) is here
//│     ▲  - l.8   test (if cond then false else check (not cond)) cond
//│     │                                        ^^^^^
//│     │  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     │ 
//│     ◉ (?check) is assumed here
//│     ▲  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     │ 
//│     ◉ (_ -> ?b) is here
//│        - l.7  let rec check cond =
//│                             ^^^^^^
//│                test (if cond then false else check (not cond)) cond
//│                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   ◉ (?b) is assumed here
//│   ▲  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   │  - l.4   then wrap z
//│   │               ^^^^^^
//│   │ 
//│   ◉ (?a0 list) is here
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a0) is assumed here
//│   ◉ (?a0 list) is here
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
//│     ◉ (_ -> ?b) is here
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
//│     ◉ (bool -> ?c) is here
//│        - l.8   test (if cond then false else check (not cond)) cond
//│                                              ^^^^^
//│   ◉ (?c) is assumed here
//│   │  - l.8   test (if cond then false else check (not cond)) cond
//│   │                                        ^^^^^^^^^^^^^^^^
//│   │  - l.8   test (if cond then false else check (not cond)) cond
//│   │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   ▼ 
//│   ◉ (?d) is assumed here
//│   │  - l.8   test (if cond then false else check (not cond)) cond
//│   │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   │  - l.3  let test z cond = if cond
//│   │                  ^
//│   ▼ 
//│   ◉ (?e) is assumed here
//│   │  - l.3  let test z cond = if cond
//│   │                  ^
//│   │  - l.4   then wrap z
//│   │                    ^
//│   ▼ 
//│   ◉ (?f) is assumed here
//│      - l.1  let wrap x = x :: []
//│                      ^
//│     ◉ (?f * _ list) is here
//│     │  - l.1  let wrap x = x :: []
//│     │                      ^^^^^^^
//│     ▼ 
//│     ◉ (?a0 * ?a0 list) is here
//│        - l.1  let wrap x = x :: []
//│                            ^^^^^^^
//│   ◉ (?a0) is assumed here
//│     ◉ (?a0 list) is here
//│     │  - l.1  let wrap x = x :: []
//│     │                      ^^^^^^^
//│     │  - l.4   then wrap z
//│     │               ^^^^^^
//│     ▼ 
//│     ◉ (?b) is assumed here
//│        - l.3  let test z cond = if cond
//│                                 ^^^^^^^
//│                then wrap z ...
//│                ^^^^^^^^^^^^^^^
//│       ◉ (_ -> ?b) is here
//│       │  - l.7  let rec check cond =
//│       │                       ^^^^^^
//│       │          test (if cond then false else check (not cond)) cond
//│       │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│       │  - l.7  let rec check cond =
//│       │                 ^^^^^
//│       ▼ 
//│       ◉ (?check) is assumed here
//│       │  - l.7  let rec check cond =
//│       │                 ^^^^^
//│       ▼ 
//│       ◉ (bool -> ?c) is here
//│          - l.8   test (if cond then false else check (not cond)) cond
//│                                                ^^^^^
//│     ◉ (?c) is assumed here
//│        - l.8   test (if cond then false else check (not cond)) cond
//│                                              ^^^^^^^^^^^^^^^^
//│       ◉ (bool -> ?c) is here
//│       ▲  - l.8   test (if cond then false else check (not cond)) cond
//│       │                                        ^^^^^
//│       │  - l.7  let rec check cond =
//│       │                 ^^^^^
//│       │ 
//│       ◉ (?check) is assumed here
//│       ▲  - l.7  let rec check cond =
//│       │                 ^^^^^
//│       │ 
//│       ◉ (_ -> ?b) is here
//│          - l.7  let rec check cond =
//│                               ^^^^^^
//│                  test (if cond then false else check (not cond)) cond
//│                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│     ◉ (?b) is assumed here
//│     ▲  - l.3  let test z cond = if cond
//│     │                           ^^^^^^^
//│     │          then wrap z ...
//│     │          ^^^^^^^^^^^^^^^
//│     │  - l.5   else wrap true
//│     │               ^^^^^^^^^
//│     │ 
//│     ◉ (?a list) is here
//│        - l.1  let wrap x = x :: []
//│                            ^^^^^^^
//│   ◉ (?a) is assumed here
//│     ◉ (?a list) is here
//│     │  - l.1  let wrap x = x :: []
//│     │                      ^^^^^^^
//│     │  - l.5   else wrap true
//│     │               ^^^^^^^^^
//│     │  - l.3  let test z cond = if cond
//│     │                           ^^^^^^^
//│     │          then wrap z ...
//│     │          ^^^^^^^^^^^^^^^
//│     ▼ 
//│     ◉ (?b) is assumed here
//│     ▲  - l.3  let test z cond = if cond
//│     │                           ^^^^^^^
//│     │          then wrap z ...
//│     │          ^^^^^^^^^^^^^^^
//│     │  - l.4   then wrap z
//│     │               ^^^^^^
//│     │ 
//│     ◉ (?a0 list) is here
//│        - l.1  let wrap x = x :: []
//│                            ^^^^^^^
//│   ◉ (?a0) is assumed here
//│     ◉ (?a0 * ?a0 list) is here
//│     ▲  - l.1  let wrap x = x :: []
//│     │                      ^^^^^^^
//│     │ 
//│     ◉ (?f * _ list) is here
//│        - l.1  let wrap x = x :: []
//│                            ^^^^^^^
//│   ◉ (?f) is assumed here
//│   ▲  - l.1  let wrap x = x :: []
//│   │                  ^
//│   │  - l.4   then wrap z
//│   │                    ^
//│   │  - l.3  let test z cond = if cond
//│   │                  ^
//│   │ 
//│   ◉ (?e) is assumed here
//│   ▲  - l.3  let test z cond = if cond
//│   │                  ^
//│   │  - l.8   test (if cond then false else check (not cond)) cond
//│   │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   │ 
//│   ◉ (?d) is assumed here
//│   ▲  - l.8   test (if cond then false else check (not cond)) cond
//│   │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   │ 
//│   ◉ (?c) is assumed here
//│      - l.8   test (if cond then false else check (not cond)) cond
//│                                            ^^^^^^^^^^^^^^^^
//│     ◉ (bool -> ?c) is here
//│     ▲  - l.8   test (if cond then false else check (not cond)) cond
//│     │                                        ^^^^^
//│     │  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     │ 
//│     ◉ (?check) is assumed here
//│     ▲  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     │ 
//│     ◉ (_ -> ?b) is here
//│        - l.7  let rec check cond =
//│                             ^^^^^^
//│                test (if cond then false else check (not cond)) cond
//│                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   ◉ (?b) is assumed here
//│   ▲  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   │  - l.5   else wrap true
//│   │               ^^^^^^^^^
//│   │ 
//│   ◉ (?a list) is here
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a) is assumed here
//│   ◉ (?a list) is here
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │  - l.5   else wrap true
//│   │               ^^^^^^^^^
//│   │  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   ▼ 
//│   ◉ (?b) is assumed here
//│   ▲  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   │  - l.4   then wrap z
//│   │               ^^^^^^
//│   │ 
//│   ◉ (?a0 list) is here
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a0) is assumed here
//│   ◉ (?a0 * ?a0 list) is here
//│   ▲  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │ 
//│   ◉ (?f * _ list) is here
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?f) is assumed here
//│ ▲  - l.1  let wrap x = x :: []
//│ │                  ^
//│ │  - l.4   then wrap z
//│ │                    ^
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ │ 
//│ ◉ (?e) is assumed here
//│ ▲  - l.3  let test z cond = if cond
//│ │                  ^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │ 
//│ ◉ (?d) is assumed here
//│ ▲  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │ 
//│ ◉ (bool) is here
//│    - l.8   test (if cond then false else check (not cond)) cond
//│                               ^^^^^
//│ [ERROR] Type `_ list` does not match `bool`
//│ 
//│         (?a list) ---> (?b) ~~~~ (?c) ---> (?d) ---> (?e) ---> (?f) ~~~~ (?a) ~~~~ (?a0) ~~~~ (?a) ~~~~ (?a0) ~~~~ (?a) ~~~~ (?f) <--- (?e) <--- (?d) <--- (bool)
//│ 
//│ ◉ (?a list) is here
//│ │  - l.1  let wrap x = x :: []
//│ │                      ^^^^^^^
//│ │  - l.4   then wrap z
//│ │               ^^^^^^
//│ ▼ 
//│ ◉ (?b) is assumed here
//│    - l.3  let test z cond = if cond
//│                             ^^^^^^^
//│            then wrap z ...
//│            ^^^^^^^^^^^^^^^
//│   ◉ (_ -> ?b) is here
//│   │  - l.7  let rec check cond =
//│   │                       ^^^^^^
//│   │          test (if cond then false else check (not cond)) cond
//│   │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   │  - l.7  let rec check cond =
//│   │                 ^^^^^
//│   ▼ 
//│   ◉ (?check) is assumed here
//│   │  - l.7  let rec check cond =
//│   │                 ^^^^^
//│   ▼ 
//│   ◉ (bool -> ?c) is here
//│      - l.8   test (if cond then false else check (not cond)) cond
//│                                            ^^^^^
//│ ◉ (?c) is assumed here
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │                                        ^^^^^^^^^^^^^^^^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ▼ 
//│ ◉ (?d) is assumed here
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ ▼ 
//│ ◉ (?e) is assumed here
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ │  - l.4   then wrap z
//│ │                    ^
//│ ▼ 
//│ ◉ (?f) is assumed here
//│    - l.1  let wrap x = x :: []
//│                    ^
//│   ◉ (?f * _ list) is here
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   ▼ 
//│   ◉ (?a * ?a list) is here
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a) is assumed here
//│   ◉ (?a list) is here
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
//│     ◉ (_ -> ?b) is here
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
//│     ◉ (bool -> ?c) is here
//│        - l.8   test (if cond then false else check (not cond)) cond
//│                                              ^^^^^
//│   ◉ (?c) is assumed here
//│      - l.8   test (if cond then false else check (not cond)) cond
//│                                            ^^^^^^^^^^^^^^^^
//│     ◉ (bool -> ?c) is here
//│     ▲  - l.8   test (if cond then false else check (not cond)) cond
//│     │                                        ^^^^^
//│     │  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     │ 
//│     ◉ (?check) is assumed here
//│     ▲  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     │ 
//│     ◉ (_ -> ?b) is here
//│        - l.7  let rec check cond =
//│                             ^^^^^^
//│                test (if cond then false else check (not cond)) cond
//│                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   ◉ (?b) is assumed here
//│   ▲  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   │  - l.5   else wrap true
//│   │               ^^^^^^^^^
//│   │ 
//│   ◉ (?a0 list) is here
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a0) is assumed here
//│   ◉ (?a0 list) is here
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │  - l.5   else wrap true
//│   │               ^^^^^^^^^
//│   ▼ 
//│   ◉ (?b) is assumed here
//│      - l.3  let test z cond = if cond
//│                               ^^^^^^^
//│              then wrap z ...
//│              ^^^^^^^^^^^^^^^
//│     ◉ (_ -> ?b) is here
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
//│     ◉ (bool -> ?c) is here
//│        - l.8   test (if cond then false else check (not cond)) cond
//│                                              ^^^^^
//│   ◉ (?c) is assumed here
//│      - l.8   test (if cond then false else check (not cond)) cond
//│                                            ^^^^^^^^^^^^^^^^
//│     ◉ (bool -> ?c) is here
//│     ▲  - l.8   test (if cond then false else check (not cond)) cond
//│     │                                        ^^^^^
//│     │  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     │ 
//│     ◉ (?check) is assumed here
//│     ▲  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     │ 
//│     ◉ (_ -> ?b) is here
//│        - l.7  let rec check cond =
//│                             ^^^^^^
//│                test (if cond then false else check (not cond)) cond
//│                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   ◉ (?b) is assumed here
//│   ▲  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   │  - l.4   then wrap z
//│   │               ^^^^^^
//│   │ 
//│   ◉ (?a list) is here
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a) is assumed here
//│   ◉ (?a list) is here
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
//│     ◉ (_ -> ?b) is here
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
//│     ◉ (bool -> ?c) is here
//│        - l.8   test (if cond then false else check (not cond)) cond
//│                                              ^^^^^
//│   ◉ (?c) is assumed here
//│   │  - l.8   test (if cond then false else check (not cond)) cond
//│   │                                        ^^^^^^^^^^^^^^^^
//│   │  - l.8   test (if cond then false else check (not cond)) cond
//│   │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   ▼ 
//│   ◉ (?d) is assumed here
//│   │  - l.8   test (if cond then false else check (not cond)) cond
//│   │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   │  - l.3  let test z cond = if cond
//│   │                  ^
//│   ▼ 
//│   ◉ (?e) is assumed here
//│   │  - l.3  let test z cond = if cond
//│   │                  ^
//│   │  - l.4   then wrap z
//│   │                    ^
//│   ▼ 
//│   ◉ (?f) is assumed here
//│      - l.1  let wrap x = x :: []
//│                      ^
//│     ◉ (?f * _ list) is here
//│     │  - l.1  let wrap x = x :: []
//│     │                      ^^^^^^^
//│     ▼ 
//│     ◉ (?a * ?a list) is here
//│        - l.1  let wrap x = x :: []
//│                            ^^^^^^^
//│   ◉ (?a) is assumed here
//│     ◉ (?a list) is here
//│     │  - l.1  let wrap x = x :: []
//│     │                      ^^^^^^^
//│     │  - l.4   then wrap z
//│     │               ^^^^^^
//│     ▼ 
//│     ◉ (?b) is assumed here
//│        - l.3  let test z cond = if cond
//│                                 ^^^^^^^
//│                then wrap z ...
//│                ^^^^^^^^^^^^^^^
//│       ◉ (_ -> ?b) is here
//│       │  - l.7  let rec check cond =
//│       │                       ^^^^^^
//│       │          test (if cond then false else check (not cond)) cond
//│       │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│       │  - l.7  let rec check cond =
//│       │                 ^^^^^
//│       ▼ 
//│       ◉ (?check) is assumed here
//│       │  - l.7  let rec check cond =
//│       │                 ^^^^^
//│       ▼ 
//│       ◉ (bool -> ?c) is here
//│          - l.8   test (if cond then false else check (not cond)) cond
//│                                                ^^^^^
//│     ◉ (?c) is assumed here
//│        - l.8   test (if cond then false else check (not cond)) cond
//│                                              ^^^^^^^^^^^^^^^^
//│       ◉ (bool -> ?c) is here
//│       ▲  - l.8   test (if cond then false else check (not cond)) cond
//│       │                                        ^^^^^
//│       │  - l.7  let rec check cond =
//│       │                 ^^^^^
//│       │ 
//│       ◉ (?check) is assumed here
//│       ▲  - l.7  let rec check cond =
//│       │                 ^^^^^
//│       │ 
//│       ◉ (_ -> ?b) is here
//│          - l.7  let rec check cond =
//│                               ^^^^^^
//│                  test (if cond then false else check (not cond)) cond
//│                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│     ◉ (?b) is assumed here
//│     ▲  - l.3  let test z cond = if cond
//│     │                           ^^^^^^^
//│     │          then wrap z ...
//│     │          ^^^^^^^^^^^^^^^
//│     │  - l.5   else wrap true
//│     │               ^^^^^^^^^
//│     │ 
//│     ◉ (?a0 list) is here
//│        - l.1  let wrap x = x :: []
//│                            ^^^^^^^
//│   ◉ (?a0) is assumed here
//│     ◉ (?a0 list) is here
//│     │  - l.1  let wrap x = x :: []
//│     │                      ^^^^^^^
//│     │  - l.5   else wrap true
//│     │               ^^^^^^^^^
//│     │  - l.3  let test z cond = if cond
//│     │                           ^^^^^^^
//│     │          then wrap z ...
//│     │          ^^^^^^^^^^^^^^^
//│     ▼ 
//│     ◉ (?b) is assumed here
//│     ▲  - l.3  let test z cond = if cond
//│     │                           ^^^^^^^
//│     │          then wrap z ...
//│     │          ^^^^^^^^^^^^^^^
//│     │  - l.4   then wrap z
//│     │               ^^^^^^
//│     │ 
//│     ◉ (?a list) is here
//│        - l.1  let wrap x = x :: []
//│                            ^^^^^^^
//│   ◉ (?a) is assumed here
//│     ◉ (?a * ?a list) is here
//│     ▲  - l.1  let wrap x = x :: []
//│     │                      ^^^^^^^
//│     │ 
//│     ◉ (?f * _ list) is here
//│        - l.1  let wrap x = x :: []
//│                            ^^^^^^^
//│   ◉ (?f) is assumed here
//│   ▲  - l.1  let wrap x = x :: []
//│   │                  ^
//│   │  - l.4   then wrap z
//│   │                    ^
//│   │  - l.3  let test z cond = if cond
//│   │                  ^
//│   │ 
//│   ◉ (?e) is assumed here
//│   ▲  - l.3  let test z cond = if cond
//│   │                  ^
//│   │  - l.8   test (if cond then false else check (not cond)) cond
//│   │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   │ 
//│   ◉ (?d) is assumed here
//│   ▲  - l.8   test (if cond then false else check (not cond)) cond
//│   │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   │ 
//│   ◉ (?c) is assumed here
//│      - l.8   test (if cond then false else check (not cond)) cond
//│                                            ^^^^^^^^^^^^^^^^
//│     ◉ (bool -> ?c) is here
//│     ▲  - l.8   test (if cond then false else check (not cond)) cond
//│     │                                        ^^^^^
//│     │  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     │ 
//│     ◉ (?check) is assumed here
//│     ▲  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     │ 
//│     ◉ (_ -> ?b) is here
//│        - l.7  let rec check cond =
//│                             ^^^^^^
//│                test (if cond then false else check (not cond)) cond
//│                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   ◉ (?b) is assumed here
//│   ▲  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   │  - l.5   else wrap true
//│   │               ^^^^^^^^^
//│   │ 
//│   ◉ (?a0 list) is here
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a0) is assumed here
//│   ◉ (?a0 list) is here
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │  - l.5   else wrap true
//│   │               ^^^^^^^^^
//│   │  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   ▼ 
//│   ◉ (?b) is assumed here
//│   ▲  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   │  - l.4   then wrap z
//│   │               ^^^^^^
//│   │ 
//│   ◉ (?a list) is here
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a) is assumed here
//│   ◉ (?a * ?a list) is here
//│   ▲  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │ 
//│   ◉ (?f * _ list) is here
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?f) is assumed here
//│ ▲  - l.1  let wrap x = x :: []
//│ │                  ^
//│ │  - l.4   then wrap z
//│ │                    ^
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ │ 
//│ ◉ (?e) is assumed here
//│ ▲  - l.3  let test z cond = if cond
//│ │                  ^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │ 
//│ ◉ (?d) is assumed here
//│ ▲  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │ 
//│ ◉ (bool) is here
//│    - l.8   test (if cond then false else check (not cond)) cond
//│                               ^^^^^
//│ U max: 39, total: 167
//│ UERR 6 errors
//│ L: 2 [list['a91'] ~ bool, list['a91'] <: α92', [α92' - (α86' -> [α92']) ~ ([bool] -> α99') - α99', L: 0 [(α86' -> [α92']) ~ ([bool] -> α99'), (α86' -> [α92']) <: check85', check85' <: ([bool] -> α99')]], α99' <: α96', α96' <: α87', α87' <: α94', [α94' - ([[α94']], [[list['a93']]],) ~ ('a91', list['a91'],) - 'a91', L: 0 [([[α94']], [[list['a93']]],) ~ ('a91', list['a91'],), ([[α94']], [[list['a93']]],) <: ('a91', list['a91'],)]], ['a91' - list['a91'] ~ list['a89'] - 'a89', L: 0 [list['a91'] ~ list['a89'], list['a91'] <: α92', α92' :> list['a89']]], ['a89' - list['a89'] ~ list['a90'] - 'a90', L: 1 [list['a89'] ~ list['a90'], [list['a89'] - ('a89', list['a89'],) ~ ([[α88']], [[list['a90']]],) - list['a90'], L: 0 [('a89', list['a89'],) ~ ([[α88']], [[list['a90']]],), ('a89', list['a89'],) :> ([[α88']], [[list['a90']]],)]]]], ['a90' - list['a90'] ~ list['a89'] - 'a89', L: 1 [list['a90'] ~ list['a89'], [list['a90'] - ([[α88']], [[list['a90']]],) ~ ('a89', list['a89'],) - list['a89'], L: 0 [([[α88']], [[list['a90']]],) ~ ('a89', list['a89'],), ([[α88']], [[list['a90']]],) <: ('a89', list['a89'],)]]]], ['a89' - ('a89', list['a89'],) ~ ([[α88']], [[list['a90']]],) - α88', L: 0 [('a89', list['a89'],) ~ ([[α88']], [[list['a90']]],), ('a89', list['a89'],) :> ([[α88']], [[list['a90']]],)]], [α88' - ([[α88']], [[list['a90']]],) ~ ('a89', list['a89'],) - 'a89', L: 0 [([[α88']], [[list['a90']]],) ~ ('a89', list['a89'],), ([[α88']], [[list['a90']]],) <: ('a89', list['a89'],)]], ['a89' - list['a89'] ~ list['a90'] - 'a90', L: 1 [list['a89'] ~ list['a90'], [list['a89'] - ('a89', list['a89'],) ~ ([[α88']], [[list['a90']]],) - list['a90'], L: 0 [('a89', list['a89'],) ~ ([[α88']], [[list['a90']]],), ('a89', list['a89'],) :> ([[α88']], [[list['a90']]],)]]]], ['a90' - list['a90'] ~ list['a89'] - 'a89', L: 1 [list['a90'] ~ list['a89'], [list['a90'] - ([[α88']], [[list['a90']]],) ~ ('a89', list['a89'],) - list['a89'], L: 0 [([[α88']], [[list['a90']]],) ~ ('a89', list['a89'],), ([[α88']], [[list['a90']]],) <: ('a89', list['a89'],)]]]], ['a89' - list['a89'] ~ list['a91'] - 'a91', L: 0 [list['a89'] ~ list['a91'], list['a89'] <: α92', α92' :> list['a91']]], ['a91' - ('a91', list['a91'],) ~ ([[α94']], [[list['a93']]],) - α94', L: 0 [('a91', list['a91'],) ~ ([[α94']], [[list['a93']]],), ('a91', list['a91'],) :> ([[α94']], [[list['a93']]],)]], α94' :> α87', α87' :> α96', α96' :> bool]
//│ L: 1 [list['a89'] ~ bool, list['a89'] <: α92', [α92' - (α86' -> [α92']) ~ ([bool] -> α99') - α99', L: 0 [(α86' -> [α92']) ~ ([bool] -> α99'), (α86' -> [α92']) <: check85', check85' <: ([bool] -> α99')]], α99' <: α96', α96' <: α87', α87' <: α94', [α94' - ([[α94']], [[list['a93']]],) ~ ('a91', list['a91'],) - 'a91', L: 0 [([[α94']], [[list['a93']]],) ~ ('a91', list['a91'],), ([[α94']], [[list['a93']]],) <: ('a91', list['a91'],)]], ['a91' - ('a91', list['a91'],) ~ ([[α94']], [[list['a93']]],) - α94', L: 0 [('a91', list['a91'],) ~ ([[α94']], [[list['a93']]],), ('a91', list['a91'],) :> ([[α94']], [[list['a93']]],)]], α94' :> α87', α87' :> α96', α96' :> bool]
//│ L: 2 [list['a89'] ~ bool, list['a89'] <: α92', [α92' - (α86' -> [α92']) ~ ([bool] -> α99') - α99', L: 0 [(α86' -> [α92']) ~ ([bool] -> α99'), (α86' -> [α92']) <: check85', check85' <: ([bool] -> α99')]], α99' <: α96', α96' <: α87', α87' <: α94', [α94' - ([[α94']], [[list['a93']]],) ~ ('a91', list['a91'],) - 'a91', L: 0 [([[α94']], [[list['a93']]],) ~ ('a91', list['a91'],), ([[α94']], [[list['a93']]],) <: ('a91', list['a91'],)]], ['a91' - list['a91'] ~ list['a89'] - 'a89', L: 0 [list['a91'] ~ list['a89'], list['a91'] <: α92', α92' :> list['a89']]], ['a89' - list['a89'] ~ list['a90'] - 'a90', L: 1 [list['a89'] ~ list['a90'], [list['a89'] - ('a89', list['a89'],) ~ ([[α88']], [[list['a90']]],) - list['a90'], L: 0 [('a89', list['a89'],) ~ ([[α88']], [[list['a90']]],), ('a89', list['a89'],) :> ([[α88']], [[list['a90']]],)]]]], ['a90' - list['a90'] ~ list['a89'] - 'a89', L: 1 [list['a90'] ~ list['a89'], [list['a90'] - ([[α88']], [[list['a90']]],) ~ ('a89', list['a89'],) - list['a89'], L: 0 [([[α88']], [[list['a90']]],) ~ ('a89', list['a89'],), ([[α88']], [[list['a90']]],) <: ('a89', list['a89'],)]]]], ['a89' - ('a89', list['a89'],) ~ ([[α88']], [[list['a90']]],) - α88', L: 0 [('a89', list['a89'],) ~ ([[α88']], [[list['a90']]],), ('a89', list['a89'],) :> ([[α88']], [[list['a90']]],)]], [α88' - ([[α88']], [[list['a90']]],) ~ ('a89', list['a89'],) - 'a89', L: 0 [([[α88']], [[list['a90']]],) ~ ('a89', list['a89'],), ([[α88']], [[list['a90']]],) <: ('a89', list['a89'],)]], ['a89' - list['a89'] ~ list['a90'] - 'a90', L: 1 [list['a89'] ~ list['a90'], [list['a89'] - ('a89', list['a89'],) ~ ([[α88']], [[list['a90']]],) - list['a90'], L: 0 [('a89', list['a89'],) ~ ([[α88']], [[list['a90']]],), ('a89', list['a89'],) :> ([[α88']], [[list['a90']]],)]]]], ['a90' - list['a90'] ~ list['a89'] - 'a89', L: 1 [list['a90'] ~ list['a89'], [list['a90'] - ([[α88']], [[list['a90']]],) ~ ('a89', list['a89'],) - list['a89'], L: 0 [([[α88']], [[list['a90']]],) ~ ('a89', list['a89'],), ([[α88']], [[list['a90']]],) <: ('a89', list['a89'],)]]]], ['a89' - list['a89'] ~ list['a91'] - 'a91', L: 0 [list['a89'] ~ list['a91'], list['a89'] <: α92', α92' :> list['a91']]], ['a91' - ('a91', list['a91'],) ~ ([[α94']], [[list['a93']]],) - α94', L: 0 [('a91', list['a91'],) ~ ([[α94']], [[list['a93']]],), ('a91', list['a91'],) :> ([[α94']], [[list['a93']]],)]], α94' :> α87', α87' :> α96', α96' :> bool]
//│ L: 3 [list['a89'] ~ bool, list['a89'] <: α92', [α92' - (α86' -> [α92']) ~ ([bool] -> α99') - α99', L: 0 [(α86' -> [α92']) ~ ([bool] -> α99'), (α86' -> [α92']) <: check85', check85' <: ([bool] -> α99')]], α99' <: α96', α96' <: α87', α87' <: α94', [α94' - ([[α94']], [[list['a93']]],) ~ ('a91', list['a91'],) - 'a91', L: 0 [([[α94']], [[list['a93']]],) ~ ('a91', list['a91'],), ([[α94']], [[list['a93']]],) <: ('a91', list['a91'],)]], ['a91' - list['a91'] ~ list['a89'] - 'a89', L: 1 [list['a91'] ~ list['a89'], list['a91'] <: α92', [α92' - (α86' -> [α92']) ~ ([bool] -> α99') - α99', L: 0 [(α86' -> [α92']) ~ ([bool] -> α99'), (α86' -> [α92']) <: check85', check85' <: ([bool] -> α99')]], [α99' - ([bool] -> α99') ~ (α86' -> [α92']) - α92', L: 0 [([bool] -> α99') ~ (α86' -> [α92']), ([bool] -> α99') :> check85', check85' :> (α86' -> [α92'])]], α92' :> list['a89']]], ['a89' - list['a89'] ~ list['a91'] - 'a91', L: 1 [list['a89'] ~ list['a91'], list['a89'] <: α92', [α92' - (α86' -> [α92']) ~ ([bool] -> α99') - α99', L: 0 [(α86' -> [α92']) ~ ([bool] -> α99'), (α86' -> [α92']) <: check85', check85' <: ([bool] -> α99')]], [α99' - ([bool] -> α99') ~ (α86' -> [α92']) - α92', L: 0 [([bool] -> α99') ~ (α86' -> [α92']), ([bool] -> α99') :> check85', check85' :> (α86' -> [α92'])]], α92' :> list['a91']]], ['a91' - list['a91'] ~ list['a89'] - 'a89', L: 2 [list['a91'] ~ list['a89'], list['a91'] <: α92', [α92' - (α86' -> [α92']) ~ ([bool] -> α99') - α99', L: 0 [(α86' -> [α92']) ~ ([bool] -> α99'), (α86' -> [α92']) <: check85', check85' <: ([bool] -> α99')]], α99' <: α96', α96' <: α87', α87' <: α94', [α94' - ([[α94']], [[list['a93']]],) ~ ('a91', list['a91'],) - 'a91', L: 0 [([[α94']], [[list['a93']]],) ~ ('a91', list['a91'],), ([[α94']], [[list['a93']]],) <: ('a91', list['a91'],)]], ['a91' - list['a91'] ~ list['a89'] - 'a89', L: 1 [list['a91'] ~ list['a89'], list['a91'] <: α92', [α92' - (α86' -> [α92']) ~ ([bool] -> α99') - α99', L: 0 [(α86' -> [α92']) ~ ([bool] -> α99'), (α86' -> [α92']) <: check85', check85' <: ([bool] -> α99')]], [α99' - ([bool] -> α99') ~ (α86' -> [α92']) - α92', L: 0 [([bool] -> α99') ~ (α86' -> [α92']), ([bool] -> α99') :> check85', check85' :> (α86' -> [α92'])]], α92' :> list['a89']]], ['a89' - list['a89'] ~ list['a91'] - 'a91', L: 0 [list['a89'] ~ list['a91'], list['a89'] <: α92', α92' :> list['a91']]], ['a91' - ('a91', list['a91'],) ~ ([[α94']], [[list['a93']]],) - α94', L: 0 [('a91', list['a91'],) ~ ([[α94']], [[list['a93']]],), ('a91', list['a91'],) :> ([[α94']], [[list['a93']]],)]], α94' :> α87', α87' :> α96', α96' :> α99', [α99' - ([bool] -> α99') ~ (α86' -> [α92']) - α92', L: 0 [([bool] -> α99') ~ (α86' -> [α92']), ([bool] -> α99') :> check85', check85' :> (α86' -> [α92'])]], α92' :> list['a89']]], ['a89' - list['a89'] ~ list['a91'] - 'a91', L: 0 [list['a89'] ~ list['a91'], list['a89'] <: α92', α92' :> list['a91']]], ['a91' - ('a91', list['a91'],) ~ ([[α94']], [[list['a93']]],) - α94', L: 0 [('a91', list['a91'],) ~ ([[α94']], [[list['a93']]],), ('a91', list['a91'],) :> ([[α94']], [[list['a93']]],)]], α94' :> α87', α87' :> α96', α96' :> bool]
//│ L: 3 [list['a91'] ~ bool, list['a91'] <: α92', [α92' - (α86' -> [α92']) ~ ([bool] -> α99') - α99', L: 0 [(α86' -> [α92']) ~ ([bool] -> α99'), (α86' -> [α92']) <: check85', check85' <: ([bool] -> α99')]], α99' <: α96', α96' <: α87', α87' <: α94', [α94' - ([[α94']], [[list['a93']]],) ~ ('a91', list['a91'],) - 'a91', L: 0 [([[α94']], [[list['a93']]],) ~ ('a91', list['a91'],), ([[α94']], [[list['a93']]],) <: ('a91', list['a91'],)]], ['a91' - list['a91'] ~ list['a89'] - 'a89', L: 1 [list['a91'] ~ list['a89'], list['a91'] <: α92', [α92' - (α86' -> [α92']) ~ ([bool] -> α99') - α99', L: 0 [(α86' -> [α92']) ~ ([bool] -> α99'), (α86' -> [α92']) <: check85', check85' <: ([bool] -> α99')]], [α99' - ([bool] -> α99') ~ (α86' -> [α92']) - α92', L: 0 [([bool] -> α99') ~ (α86' -> [α92']), ([bool] -> α99') :> check85', check85' :> (α86' -> [α92'])]], α92' :> list['a89']]], ['a89' - list['a89'] ~ list['a91'] - 'a91', L: 1 [list['a89'] ~ list['a91'], list['a89'] <: α92', [α92' - (α86' -> [α92']) ~ ([bool] -> α99') - α99', L: 0 [(α86' -> [α92']) ~ ([bool] -> α99'), (α86' -> [α92']) <: check85', check85' <: ([bool] -> α99')]], [α99' - ([bool] -> α99') ~ (α86' -> [α92']) - α92', L: 0 [([bool] -> α99') ~ (α86' -> [α92']), ([bool] -> α99') :> check85', check85' :> (α86' -> [α92'])]], α92' :> list['a91']]], ['a91' - list['a91'] ~ list['a89'] - 'a89', L: 2 [list['a91'] ~ list['a89'], list['a91'] <: α92', [α92' - (α86' -> [α92']) ~ ([bool] -> α99') - α99', L: 0 [(α86' -> [α92']) ~ ([bool] -> α99'), (α86' -> [α92']) <: check85', check85' <: ([bool] -> α99')]], α99' <: α96', α96' <: α87', α87' <: α94', [α94' - ([[α94']], [[list['a93']]],) ~ ('a91', list['a91'],) - 'a91', L: 0 [([[α94']], [[list['a93']]],) ~ ('a91', list['a91'],), ([[α94']], [[list['a93']]],) <: ('a91', list['a91'],)]], ['a91' - list['a91'] ~ list['a89'] - 'a89', L: 1 [list['a91'] ~ list['a89'], list['a91'] <: α92', [α92' - (α86' -> [α92']) ~ ([bool] -> α99') - α99', L: 0 [(α86' -> [α92']) ~ ([bool] -> α99'), (α86' -> [α92']) <: check85', check85' <: ([bool] -> α99')]], [α99' - ([bool] -> α99') ~ (α86' -> [α92']) - α92', L: 0 [([bool] -> α99') ~ (α86' -> [α92']), ([bool] -> α99') :> check85', check85' :> (α86' -> [α92'])]], α92' :> list['a89']]], ['a89' - list['a89'] ~ list['a91'] - 'a91', L: 0 [list['a89'] ~ list['a91'], list['a89'] <: α92', α92' :> list['a91']]], ['a91' - ('a91', list['a91'],) ~ ([[α94']], [[list['a93']]],) - α94', L: 0 [('a91', list['a91'],) ~ ([[α94']], [[list['a93']]],), ('a91', list['a91'],) :> ([[α94']], [[list['a93']]],)]], α94' :> α87', α87' :> α96', α96' :> α99', [α99' - ([bool] -> α99') ~ (α86' -> [α92']) - α92', L: 0 [([bool] -> α99') ~ (α86' -> [α92']), ([bool] -> α99') :> check85', check85' :> (α86' -> [α92'])]], α92' :> list['a89']]], ['a89' - list['a89'] ~ list['a91'] - 'a91', L: 0 [list['a89'] ~ list['a91'], list['a89'] <: α92', α92' :> list['a91']]], ['a91' - ('a91', list['a91'],) ~ ([[α94']], [[list['a93']]],) - α94', L: 0 [('a91', list['a91'],) ~ ([[α94']], [[list['a93']]],), ('a91', list['a91'],) :> ([[α94']], [[list['a93']]],)]], α94' :> α87', α87' :> α96', α96' :> bool]
//│ L: 1 [list['a91'] ~ bool, list['a91'] <: α92', [α92' - (α86' -> [α92']) ~ ([bool] -> α99') - α99', L: 0 [(α86' -> [α92']) ~ ([bool] -> α99'), (α86' -> [α92']) <: check85', check85' <: ([bool] -> α99')]], α99' <: α96', α96' <: α87', α87' <: α94', [α94' - ([[α94']], [[list['a93']]],) ~ ('a91', list['a91'],) - 'a91', L: 0 [([[α94']], [[list['a93']]],) ~ ('a91', list['a91'],), ([[α94']], [[list['a93']]],) <: ('a91', list['a91'],)]], ['a91' - ('a91', list['a91'],) ~ ([[α94']], [[list['a93']]],) - α94', L: 0 [('a91', list['a91'],) ~ ([[α94']], [[list['a93']]],), ('a91', list['a91'],) :> ([[α94']], [[list['a93']]],)]], α94' :> α87', α87' :> α96', α96' :> bool]


