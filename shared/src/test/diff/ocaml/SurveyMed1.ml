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
//│ ◉ (_ list) comes from
//│ │  - l.1  let wrap x = x :: []
//│ │                      ^^^^^^^
//│ │  - l.5   else wrap true
//│ │               ^^^^^^^^^
//│ ▼ 
//│ ◉ (?b) is assumed for
//│    - l.3  let test z cond = if cond
//│                             ^^^^^^^
//│            then wrap z ...
//│            ^^^^^^^^^^^^^^^
//│   ◉ (_ -> ?b) comes from
//│   │  - l.7  let rec check cond =
//│   │                       ^^^^^^
//│   │          test (if cond then false else check (not cond)) cond
//│   │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   │  - l.7  let rec check cond =
//│   │                 ^^^^^
//│   ▼ 
//│   ◉ (?check) is assumed for
//│   │  - l.7  let rec check cond =
//│   │                 ^^^^^
//│   ▼ 
//│   ◉ (bool -> ?c) comes from
//│      - l.8   test (if cond then false else check (not cond)) cond
//│                                            ^^^^^
//│ ◉ (?c) is assumed for
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │                                        ^^^^^^^^^^^^^^^^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ▼ 
//│ ◉ (?d) is assumed for
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ ▼ 
//│ ◉ (?e) is assumed for
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ │  - l.4   then wrap z
//│ │                    ^
//│ ▼ 
//│ ◉ (?f) is assumed for
//│    - l.1  let wrap x = x :: []
//│                    ^
//│   ◉ (?f * _ list) comes from
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   ▼ 
//│   ◉ (?a * ?a list) comes from
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a) is assumed for
//│   ◉ (?a * ?a list) comes from
//│   ▲  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │ 
//│   ◉ (?f * _ list) comes from
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?f) is assumed for
//│ ▲  - l.1  let wrap x = x :: []
//│ │                  ^
//│ │  - l.4   then wrap z
//│ │                    ^
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ │ 
//│ ◉ (?e) is assumed for
//│ ▲  - l.3  let test z cond = if cond
//│ │                  ^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │ 
//│ ◉ (?d) is assumed for
//│ ▲  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │ 
//│ ◉ (bool) comes from
//│    - l.8   test (if cond then false else check (not cond)) cond
//│                               ^^^^^
//│ [ERROR] Type `_ list` does not match `bool`
//│ 
//│         (?a list) ---> (?b) ~~~~ (?c) ---> (?d) ---> (?e) ---> (?f) ~~~~ (?a) ~~~~ (?f) <--- (?e) <--- (?d) <--- (bool)
//│ 
//│ ◉ (?a list) comes from
//│ │  - l.1  let wrap x = x :: []
//│ │                      ^^^^^^^
//│ │  - l.4   then wrap z
//│ │               ^^^^^^
//│ ▼ 
//│ ◉ (?b) is assumed for
//│    - l.3  let test z cond = if cond
//│                             ^^^^^^^
//│            then wrap z ...
//│            ^^^^^^^^^^^^^^^
//│   ◉ (_ -> ?b) comes from
//│   │  - l.7  let rec check cond =
//│   │                       ^^^^^^
//│   │          test (if cond then false else check (not cond)) cond
//│   │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   │  - l.7  let rec check cond =
//│   │                 ^^^^^
//│   ▼ 
//│   ◉ (?check) is assumed for
//│   │  - l.7  let rec check cond =
//│   │                 ^^^^^
//│   ▼ 
//│   ◉ (bool -> ?c) comes from
//│      - l.8   test (if cond then false else check (not cond)) cond
//│                                            ^^^^^
//│ ◉ (?c) is assumed for
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │                                        ^^^^^^^^^^^^^^^^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ▼ 
//│ ◉ (?d) is assumed for
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ ▼ 
//│ ◉ (?e) is assumed for
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ │  - l.4   then wrap z
//│ │                    ^
//│ ▼ 
//│ ◉ (?f) is assumed for
//│    - l.1  let wrap x = x :: []
//│                    ^
//│   ◉ (?f * _ list) comes from
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   ▼ 
//│   ◉ (?a * ?a list) comes from
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a) is assumed for
//│   ◉ (?a * ?a list) comes from
//│   ▲  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │ 
//│   ◉ (?f * _ list) comes from
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?f) is assumed for
//│ ▲  - l.1  let wrap x = x :: []
//│ │                  ^
//│ │  - l.4   then wrap z
//│ │                    ^
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ │ 
//│ ◉ (?e) is assumed for
//│ ▲  - l.3  let test z cond = if cond
//│ │                  ^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │ 
//│ ◉ (?d) is assumed for
//│ ▲  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │ 
//│ ◉ (bool) comes from
//│    - l.8   test (if cond then false else check (not cond)) cond
//│                               ^^^^^
//│ [ERROR] Type `_ list` does not match `bool`
//│ 
//│         (?a list) ---> (?b) ~~~~ (?c) ---> (?d) ---> (?e) ---> (?f) ~~~~ (?a0) ~~~~ (?a) ~~~~ (?a1) ~~~~ (?a) ~~~~ (?g) ~~~~ (?a) ~~~~ (?a1) ~~~~ (?a) ~~~~ (?a0) ~~~~ (?f) <--- (?e) <--- (?d) <--- (bool)
//│ 
//│ ◉ (?a list) comes from
//│ │  - l.1  let wrap x = x :: []
//│ │                      ^^^^^^^
//│ │  - l.5   else wrap true
//│ │               ^^^^^^^^^
//│ ▼ 
//│ ◉ (?b) is assumed for
//│    - l.3  let test z cond = if cond
//│                             ^^^^^^^
//│            then wrap z ...
//│            ^^^^^^^^^^^^^^^
//│   ◉ (_ -> ?b) comes from
//│   │  - l.7  let rec check cond =
//│   │                       ^^^^^^
//│   │          test (if cond then false else check (not cond)) cond
//│   │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   │  - l.7  let rec check cond =
//│   │                 ^^^^^
//│   ▼ 
//│   ◉ (?check) is assumed for
//│   │  - l.7  let rec check cond =
//│   │                 ^^^^^
//│   ▼ 
//│   ◉ (bool -> ?c) comes from
//│      - l.8   test (if cond then false else check (not cond)) cond
//│                                            ^^^^^
//│ ◉ (?c) is assumed for
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │                                        ^^^^^^^^^^^^^^^^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ▼ 
//│ ◉ (?d) is assumed for
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ ▼ 
//│ ◉ (?e) is assumed for
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ │  - l.4   then wrap z
//│ │                    ^
//│ ▼ 
//│ ◉ (?f) is assumed for
//│    - l.1  let wrap x = x :: []
//│                    ^
//│   ◉ (?f * _ list) comes from
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   ▼ 
//│   ◉ (?a0 * ?a0 list) comes from
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a0) is assumed for
//│   ◉ (?a0 list) comes from
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │  - l.4   then wrap z
//│   │               ^^^^^^
//│   │  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   ▼ 
//│   ◉ (?b) is assumed for
//│   ▲  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   │  - l.5   else wrap true
//│   │               ^^^^^^^^^
//│   │ 
//│   ◉ (?a list) comes from
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a) is assumed for
//│   ◉ (?a list) comes from
//│     ◉ (?a * ?a list) comes from
//│     ▲  - l.1  let wrap x = x :: []
//│     │                      ^^^^^^^
//│     │ 
//│     ◉ (?g * ?a1 list) comes from
//│        - l.1  let wrap x = x :: []
//│                            ^^^^^^^
//│   ◉ (?a1 list) comes from
//│      - l.1  let wrap x = x :: []
//│                               ^^
//│ ◉ (?a1) is assumed for
//│   ◉ (?a1 list) comes from
//│      - l.1  let wrap x = x :: []
//│                               ^^
//│     ◉ (?g * ?a1 list) comes from
//│     │  - l.1  let wrap x = x :: []
//│     │                      ^^^^^^^
//│     ▼ 
//│     ◉ (?a * ?a list) comes from
//│        - l.1  let wrap x = x :: []
//│                            ^^^^^^^
//│   ◉ (?a list) comes from
//│ ◉ (?a) is assumed for
//│   ◉ (?a * ?a list) comes from
//│   ▲  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │ 
//│   ◉ (?g * ?a1 list) comes from
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?g) is assumed for
//│    - l.1  let wrap x = x :: []
//│                        ^
//│    - l.1  let wrap x = x :: []
//│                    ^
//│   ◉ (?g * ?a1 list) comes from
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   ▼ 
//│   ◉ (?a * ?a list) comes from
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a) is assumed for
//│   ◉ (?a list) comes from
//│     ◉ (?a * ?a list) comes from
//│     ▲  - l.1  let wrap x = x :: []
//│     │                      ^^^^^^^
//│     │ 
//│     ◉ (?g * ?a1 list) comes from
//│        - l.1  let wrap x = x :: []
//│                            ^^^^^^^
//│   ◉ (?a1 list) comes from
//│      - l.1  let wrap x = x :: []
//│                               ^^
//│ ◉ (?a1) is assumed for
//│   ◉ (?a1 list) comes from
//│      - l.1  let wrap x = x :: []
//│                               ^^
//│     ◉ (?g * ?a1 list) comes from
//│     │  - l.1  let wrap x = x :: []
//│     │                      ^^^^^^^
//│     ▼ 
//│     ◉ (?a * ?a list) comes from
//│        - l.1  let wrap x = x :: []
//│                            ^^^^^^^
//│   ◉ (?a list) comes from
//│ ◉ (?a) is assumed for
//│   ◉ (?a list) comes from
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │  - l.5   else wrap true
//│   │               ^^^^^^^^^
//│   │  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   ▼ 
//│   ◉ (?b) is assumed for
//│   ▲  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   │  - l.4   then wrap z
//│   │               ^^^^^^
//│   │ 
//│   ◉ (?a0 list) comes from
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a0) is assumed for
//│   ◉ (?a0 * ?a0 list) comes from
//│   ▲  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │ 
//│   ◉ (?f * _ list) comes from
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?f) is assumed for
//│ ▲  - l.1  let wrap x = x :: []
//│ │                  ^
//│ │  - l.4   then wrap z
//│ │                    ^
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ │ 
//│ ◉ (?e) is assumed for
//│ ▲  - l.3  let test z cond = if cond
//│ │                  ^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │ 
//│ ◉ (?d) is assumed for
//│ ▲  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │ 
//│ ◉ (bool) comes from
//│    - l.8   test (if cond then false else check (not cond)) cond
//│                               ^^^^^
//│ [ERROR] Type `_ list` does not match `bool`
//│ 
//│         (?a list) ---> (?b) ~~~~ (?c) ---> (?d) ---> (?e) ---> (?f) ~~~~ (?a) ~~~~ (?a0) ~~~~ (?a1) ~~~~ (?a0) ~~~~ (?g) ~~~~ (?a0) ~~~~ (?a1) ~~~~ (?a0) ~~~~ (?a) ~~~~ (?f) <--- (?e) <--- (?d) <--- (bool)
//│ 
//│ ◉ (?a list) comes from
//│ │  - l.1  let wrap x = x :: []
//│ │                      ^^^^^^^
//│ │  - l.4   then wrap z
//│ │               ^^^^^^
//│ ▼ 
//│ ◉ (?b) is assumed for
//│    - l.3  let test z cond = if cond
//│                             ^^^^^^^
//│            then wrap z ...
//│            ^^^^^^^^^^^^^^^
//│   ◉ (_ -> ?b) comes from
//│   │  - l.7  let rec check cond =
//│   │                       ^^^^^^
//│   │          test (if cond then false else check (not cond)) cond
//│   │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   │  - l.7  let rec check cond =
//│   │                 ^^^^^
//│   ▼ 
//│   ◉ (?check) is assumed for
//│   │  - l.7  let rec check cond =
//│   │                 ^^^^^
//│   ▼ 
//│   ◉ (bool -> ?c) comes from
//│      - l.8   test (if cond then false else check (not cond)) cond
//│                                            ^^^^^
//│ ◉ (?c) is assumed for
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │                                        ^^^^^^^^^^^^^^^^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ▼ 
//│ ◉ (?d) is assumed for
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ ▼ 
//│ ◉ (?e) is assumed for
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ │  - l.4   then wrap z
//│ │                    ^
//│ ▼ 
//│ ◉ (?f) is assumed for
//│    - l.1  let wrap x = x :: []
//│                    ^
//│   ◉ (?f * _ list) comes from
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   ▼ 
//│   ◉ (?a * ?a list) comes from
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a) is assumed for
//│   ◉ (?a list) comes from
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │  - l.4   then wrap z
//│   │               ^^^^^^
//│   │  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   ▼ 
//│   ◉ (?b) is assumed for
//│   ▲  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   │  - l.5   else wrap true
//│   │               ^^^^^^^^^
//│   │ 
//│   ◉ (?a0 list) comes from
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a0) is assumed for
//│   ◉ (?a0 list) comes from
//│     ◉ (?a0 * ?a0 list) comes from
//│     ▲  - l.1  let wrap x = x :: []
//│     │                      ^^^^^^^
//│     │ 
//│     ◉ (?g * ?a1 list) comes from
//│        - l.1  let wrap x = x :: []
//│                            ^^^^^^^
//│   ◉ (?a1 list) comes from
//│      - l.1  let wrap x = x :: []
//│                               ^^
//│ ◉ (?a1) is assumed for
//│   ◉ (?a1 list) comes from
//│      - l.1  let wrap x = x :: []
//│                               ^^
//│     ◉ (?g * ?a1 list) comes from
//│     │  - l.1  let wrap x = x :: []
//│     │                      ^^^^^^^
//│     ▼ 
//│     ◉ (?a0 * ?a0 list) comes from
//│        - l.1  let wrap x = x :: []
//│                            ^^^^^^^
//│   ◉ (?a0 list) comes from
//│ ◉ (?a0) is assumed for
//│   ◉ (?a0 * ?a0 list) comes from
//│   ▲  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │ 
//│   ◉ (?g * ?a1 list) comes from
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?g) is assumed for
//│    - l.1  let wrap x = x :: []
//│                        ^
//│    - l.1  let wrap x = x :: []
//│                    ^
//│   ◉ (?g * ?a1 list) comes from
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   ▼ 
//│   ◉ (?a0 * ?a0 list) comes from
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a0) is assumed for
//│   ◉ (?a0 list) comes from
//│     ◉ (?a0 * ?a0 list) comes from
//│     ▲  - l.1  let wrap x = x :: []
//│     │                      ^^^^^^^
//│     │ 
//│     ◉ (?g * ?a1 list) comes from
//│        - l.1  let wrap x = x :: []
//│                            ^^^^^^^
//│   ◉ (?a1 list) comes from
//│      - l.1  let wrap x = x :: []
//│                               ^^
//│ ◉ (?a1) is assumed for
//│   ◉ (?a1 list) comes from
//│      - l.1  let wrap x = x :: []
//│                               ^^
//│     ◉ (?g * ?a1 list) comes from
//│     │  - l.1  let wrap x = x :: []
//│     │                      ^^^^^^^
//│     ▼ 
//│     ◉ (?a0 * ?a0 list) comes from
//│        - l.1  let wrap x = x :: []
//│                            ^^^^^^^
//│   ◉ (?a0 list) comes from
//│ ◉ (?a0) is assumed for
//│   ◉ (?a0 list) comes from
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │  - l.5   else wrap true
//│   │               ^^^^^^^^^
//│   │  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   ▼ 
//│   ◉ (?b) is assumed for
//│   ▲  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   │  - l.4   then wrap z
//│   │               ^^^^^^
//│   │ 
//│   ◉ (?a list) comes from
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a) is assumed for
//│   ◉ (?a * ?a list) comes from
//│   ▲  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │ 
//│   ◉ (?f * _ list) comes from
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?f) is assumed for
//│ ▲  - l.1  let wrap x = x :: []
//│ │                  ^
//│ │  - l.4   then wrap z
//│ │                    ^
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ │ 
//│ ◉ (?e) is assumed for
//│ ▲  - l.3  let test z cond = if cond
//│ │                  ^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │ 
//│ ◉ (?d) is assumed for
//│ ▲  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │ 
//│ ◉ (bool) comes from
//│    - l.8   test (if cond then false else check (not cond)) cond
//│                               ^^^^^
//│ [ERROR] Type `_ list` does not match `bool`
//│ 
//│         (?a list) ---> (?b) ~~~~ (?c) ---> (?d) ---> (?e) ---> (?f) ~~~~ (?a0) ~~~~ (?a) ~~~~ (?a0) ~~~~ (?a) ~~~~ (?a0) ~~~~ (?f) <--- (?e) <--- (?d) <--- (bool)
//│ 
//│ ◉ (?a list) comes from
//│ │  - l.1  let wrap x = x :: []
//│ │                      ^^^^^^^
//│ │  - l.5   else wrap true
//│ │               ^^^^^^^^^
//│ ▼ 
//│ ◉ (?b) is assumed for
//│    - l.3  let test z cond = if cond
//│                             ^^^^^^^
//│            then wrap z ...
//│            ^^^^^^^^^^^^^^^
//│   ◉ (_ -> ?b) comes from
//│   │  - l.7  let rec check cond =
//│   │                       ^^^^^^
//│   │          test (if cond then false else check (not cond)) cond
//│   │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   │  - l.7  let rec check cond =
//│   │                 ^^^^^
//│   ▼ 
//│   ◉ (?check) is assumed for
//│   │  - l.7  let rec check cond =
//│   │                 ^^^^^
//│   ▼ 
//│   ◉ (bool -> ?c) comes from
//│      - l.8   test (if cond then false else check (not cond)) cond
//│                                            ^^^^^
//│ ◉ (?c) is assumed for
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │                                        ^^^^^^^^^^^^^^^^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ▼ 
//│ ◉ (?d) is assumed for
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ ▼ 
//│ ◉ (?e) is assumed for
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ │  - l.4   then wrap z
//│ │                    ^
//│ ▼ 
//│ ◉ (?f) is assumed for
//│    - l.1  let wrap x = x :: []
//│                    ^
//│   ◉ (?f * _ list) comes from
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   ▼ 
//│   ◉ (?a0 * ?a0 list) comes from
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a0) is assumed for
//│   ◉ (?a0 list) comes from
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │  - l.4   then wrap z
//│   │               ^^^^^^
//│   ▼ 
//│   ◉ (?b) is assumed for
//│      - l.3  let test z cond = if cond
//│                               ^^^^^^^
//│              then wrap z ...
//│              ^^^^^^^^^^^^^^^
//│     ◉ (_ -> ?b) comes from
//│     │  - l.7  let rec check cond =
//│     │                       ^^^^^^
//│     │          test (if cond then false else check (not cond)) cond
//│     │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│     │  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     ▼ 
//│     ◉ (?check) is assumed for
//│     │  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     ▼ 
//│     ◉ (bool -> ?c) comes from
//│        - l.8   test (if cond then false else check (not cond)) cond
//│                                              ^^^^^
//│   ◉ (?c) is assumed for
//│      - l.8   test (if cond then false else check (not cond)) cond
//│                                            ^^^^^^^^^^^^^^^^
//│     ◉ (bool -> ?c) comes from
//│     ▲  - l.8   test (if cond then false else check (not cond)) cond
//│     │                                        ^^^^^
//│     │  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     │ 
//│     ◉ (?check) is assumed for
//│     ▲  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     │ 
//│     ◉ (_ -> ?b) comes from
//│        - l.7  let rec check cond =
//│                             ^^^^^^
//│                test (if cond then false else check (not cond)) cond
//│                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   ◉ (?b) is assumed for
//│   ▲  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   │  - l.5   else wrap true
//│   │               ^^^^^^^^^
//│   │ 
//│   ◉ (?a list) comes from
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a) is assumed for
//│   ◉ (?a list) comes from
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │  - l.5   else wrap true
//│   │               ^^^^^^^^^
//│   ▼ 
//│   ◉ (?b) is assumed for
//│      - l.3  let test z cond = if cond
//│                               ^^^^^^^
//│              then wrap z ...
//│              ^^^^^^^^^^^^^^^
//│     ◉ (_ -> ?b) comes from
//│     │  - l.7  let rec check cond =
//│     │                       ^^^^^^
//│     │          test (if cond then false else check (not cond)) cond
//│     │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│     │  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     ▼ 
//│     ◉ (?check) is assumed for
//│     │  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     ▼ 
//│     ◉ (bool -> ?c) comes from
//│        - l.8   test (if cond then false else check (not cond)) cond
//│                                              ^^^^^
//│   ◉ (?c) is assumed for
//│      - l.8   test (if cond then false else check (not cond)) cond
//│                                            ^^^^^^^^^^^^^^^^
//│     ◉ (bool -> ?c) comes from
//│     ▲  - l.8   test (if cond then false else check (not cond)) cond
//│     │                                        ^^^^^
//│     │  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     │ 
//│     ◉ (?check) is assumed for
//│     ▲  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     │ 
//│     ◉ (_ -> ?b) comes from
//│        - l.7  let rec check cond =
//│                             ^^^^^^
//│                test (if cond then false else check (not cond)) cond
//│                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   ◉ (?b) is assumed for
//│   ▲  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   │  - l.4   then wrap z
//│   │               ^^^^^^
//│   │ 
//│   ◉ (?a0 list) comes from
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a0) is assumed for
//│   ◉ (?a0 list) comes from
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │  - l.4   then wrap z
//│   │               ^^^^^^
//│   ▼ 
//│   ◉ (?b) is assumed for
//│      - l.3  let test z cond = if cond
//│                               ^^^^^^^
//│              then wrap z ...
//│              ^^^^^^^^^^^^^^^
//│     ◉ (_ -> ?b) comes from
//│     │  - l.7  let rec check cond =
//│     │                       ^^^^^^
//│     │          test (if cond then false else check (not cond)) cond
//│     │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│     │  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     ▼ 
//│     ◉ (?check) is assumed for
//│     │  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     ▼ 
//│     ◉ (bool -> ?c) comes from
//│        - l.8   test (if cond then false else check (not cond)) cond
//│                                              ^^^^^
//│   ◉ (?c) is assumed for
//│   │  - l.8   test (if cond then false else check (not cond)) cond
//│   │                                        ^^^^^^^^^^^^^^^^
//│   │  - l.8   test (if cond then false else check (not cond)) cond
//│   │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   ▼ 
//│   ◉ (?d) is assumed for
//│   │  - l.8   test (if cond then false else check (not cond)) cond
//│   │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   │  - l.3  let test z cond = if cond
//│   │                  ^
//│   ▼ 
//│   ◉ (?e) is assumed for
//│   │  - l.3  let test z cond = if cond
//│   │                  ^
//│   │  - l.4   then wrap z
//│   │                    ^
//│   ▼ 
//│   ◉ (?f) is assumed for
//│      - l.1  let wrap x = x :: []
//│                      ^
//│     ◉ (?f * _ list) comes from
//│     │  - l.1  let wrap x = x :: []
//│     │                      ^^^^^^^
//│     ▼ 
//│     ◉ (?a0 * ?a0 list) comes from
//│        - l.1  let wrap x = x :: []
//│                            ^^^^^^^
//│   ◉ (?a0) is assumed for
//│     ◉ (?a0 list) comes from
//│     │  - l.1  let wrap x = x :: []
//│     │                      ^^^^^^^
//│     │  - l.4   then wrap z
//│     │               ^^^^^^
//│     ▼ 
//│     ◉ (?b) is assumed for
//│        - l.3  let test z cond = if cond
//│                                 ^^^^^^^
//│                then wrap z ...
//│                ^^^^^^^^^^^^^^^
//│       ◉ (_ -> ?b) comes from
//│       │  - l.7  let rec check cond =
//│       │                       ^^^^^^
//│       │          test (if cond then false else check (not cond)) cond
//│       │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│       │  - l.7  let rec check cond =
//│       │                 ^^^^^
//│       ▼ 
//│       ◉ (?check) is assumed for
//│       │  - l.7  let rec check cond =
//│       │                 ^^^^^
//│       ▼ 
//│       ◉ (bool -> ?c) comes from
//│          - l.8   test (if cond then false else check (not cond)) cond
//│                                                ^^^^^
//│     ◉ (?c) is assumed for
//│        - l.8   test (if cond then false else check (not cond)) cond
//│                                              ^^^^^^^^^^^^^^^^
//│       ◉ (bool -> ?c) comes from
//│       ▲  - l.8   test (if cond then false else check (not cond)) cond
//│       │                                        ^^^^^
//│       │  - l.7  let rec check cond =
//│       │                 ^^^^^
//│       │ 
//│       ◉ (?check) is assumed for
//│       ▲  - l.7  let rec check cond =
//│       │                 ^^^^^
//│       │ 
//│       ◉ (_ -> ?b) comes from
//│          - l.7  let rec check cond =
//│                               ^^^^^^
//│                  test (if cond then false else check (not cond)) cond
//│                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│     ◉ (?b) is assumed for
//│     ▲  - l.3  let test z cond = if cond
//│     │                           ^^^^^^^
//│     │          then wrap z ...
//│     │          ^^^^^^^^^^^^^^^
//│     │  - l.5   else wrap true
//│     │               ^^^^^^^^^
//│     │ 
//│     ◉ (?a list) comes from
//│        - l.1  let wrap x = x :: []
//│                            ^^^^^^^
//│   ◉ (?a) is assumed for
//│     ◉ (?a list) comes from
//│     │  - l.1  let wrap x = x :: []
//│     │                      ^^^^^^^
//│     │  - l.5   else wrap true
//│     │               ^^^^^^^^^
//│     │  - l.3  let test z cond = if cond
//│     │                           ^^^^^^^
//│     │          then wrap z ...
//│     │          ^^^^^^^^^^^^^^^
//│     ▼ 
//│     ◉ (?b) is assumed for
//│     ▲  - l.3  let test z cond = if cond
//│     │                           ^^^^^^^
//│     │          then wrap z ...
//│     │          ^^^^^^^^^^^^^^^
//│     │  - l.4   then wrap z
//│     │               ^^^^^^
//│     │ 
//│     ◉ (?a0 list) comes from
//│        - l.1  let wrap x = x :: []
//│                            ^^^^^^^
//│   ◉ (?a0) is assumed for
//│     ◉ (?a0 * ?a0 list) comes from
//│     ▲  - l.1  let wrap x = x :: []
//│     │                      ^^^^^^^
//│     │ 
//│     ◉ (?f * _ list) comes from
//│        - l.1  let wrap x = x :: []
//│                            ^^^^^^^
//│   ◉ (?f) is assumed for
//│   ▲  - l.1  let wrap x = x :: []
//│   │                  ^
//│   │  - l.4   then wrap z
//│   │                    ^
//│   │  - l.3  let test z cond = if cond
//│   │                  ^
//│   │ 
//│   ◉ (?e) is assumed for
//│   ▲  - l.3  let test z cond = if cond
//│   │                  ^
//│   │  - l.8   test (if cond then false else check (not cond)) cond
//│   │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   │ 
//│   ◉ (?d) is assumed for
//│   ▲  - l.8   test (if cond then false else check (not cond)) cond
//│   │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   │ 
//│   ◉ (?c) is assumed for
//│      - l.8   test (if cond then false else check (not cond)) cond
//│                                            ^^^^^^^^^^^^^^^^
//│     ◉ (bool -> ?c) comes from
//│     ▲  - l.8   test (if cond then false else check (not cond)) cond
//│     │                                        ^^^^^
//│     │  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     │ 
//│     ◉ (?check) is assumed for
//│     ▲  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     │ 
//│     ◉ (_ -> ?b) comes from
//│        - l.7  let rec check cond =
//│                             ^^^^^^
//│                test (if cond then false else check (not cond)) cond
//│                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   ◉ (?b) is assumed for
//│   ▲  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   │  - l.5   else wrap true
//│   │               ^^^^^^^^^
//│   │ 
//│   ◉ (?a list) comes from
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a) is assumed for
//│   ◉ (?a list) comes from
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │  - l.5   else wrap true
//│   │               ^^^^^^^^^
//│   │  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   ▼ 
//│   ◉ (?b) is assumed for
//│   ▲  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   │  - l.4   then wrap z
//│   │               ^^^^^^
//│   │ 
//│   ◉ (?a0 list) comes from
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a0) is assumed for
//│   ◉ (?a0 * ?a0 list) comes from
//│   ▲  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │ 
//│   ◉ (?f * _ list) comes from
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?f) is assumed for
//│ ▲  - l.1  let wrap x = x :: []
//│ │                  ^
//│ │  - l.4   then wrap z
//│ │                    ^
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ │ 
//│ ◉ (?e) is assumed for
//│ ▲  - l.3  let test z cond = if cond
//│ │                  ^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │ 
//│ ◉ (?d) is assumed for
//│ ▲  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │ 
//│ ◉ (bool) comes from
//│    - l.8   test (if cond then false else check (not cond)) cond
//│                               ^^^^^
//│ [ERROR] Type `_ list` does not match `bool`
//│ 
//│         (?a list) ---> (?b) ~~~~ (?c) ---> (?d) ---> (?e) ---> (?f) ~~~~ (?a) ~~~~ (?a0) ~~~~ (?a) ~~~~ (?a0) ~~~~ (?a) ~~~~ (?f) <--- (?e) <--- (?d) <--- (bool)
//│ 
//│ ◉ (?a list) comes from
//│ │  - l.1  let wrap x = x :: []
//│ │                      ^^^^^^^
//│ │  - l.4   then wrap z
//│ │               ^^^^^^
//│ ▼ 
//│ ◉ (?b) is assumed for
//│    - l.3  let test z cond = if cond
//│                             ^^^^^^^
//│            then wrap z ...
//│            ^^^^^^^^^^^^^^^
//│   ◉ (_ -> ?b) comes from
//│   │  - l.7  let rec check cond =
//│   │                       ^^^^^^
//│   │          test (if cond then false else check (not cond)) cond
//│   │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   │  - l.7  let rec check cond =
//│   │                 ^^^^^
//│   ▼ 
//│   ◉ (?check) is assumed for
//│   │  - l.7  let rec check cond =
//│   │                 ^^^^^
//│   ▼ 
//│   ◉ (bool -> ?c) comes from
//│      - l.8   test (if cond then false else check (not cond)) cond
//│                                            ^^^^^
//│ ◉ (?c) is assumed for
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │                                        ^^^^^^^^^^^^^^^^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ ▼ 
//│ ◉ (?d) is assumed for
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ ▼ 
//│ ◉ (?e) is assumed for
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ │  - l.4   then wrap z
//│ │                    ^
//│ ▼ 
//│ ◉ (?f) is assumed for
//│    - l.1  let wrap x = x :: []
//│                    ^
//│   ◉ (?f * _ list) comes from
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   ▼ 
//│   ◉ (?a * ?a list) comes from
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a) is assumed for
//│   ◉ (?a list) comes from
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │  - l.4   then wrap z
//│   │               ^^^^^^
//│   ▼ 
//│   ◉ (?b) is assumed for
//│      - l.3  let test z cond = if cond
//│                               ^^^^^^^
//│              then wrap z ...
//│              ^^^^^^^^^^^^^^^
//│     ◉ (_ -> ?b) comes from
//│     │  - l.7  let rec check cond =
//│     │                       ^^^^^^
//│     │          test (if cond then false else check (not cond)) cond
//│     │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│     │  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     ▼ 
//│     ◉ (?check) is assumed for
//│     │  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     ▼ 
//│     ◉ (bool -> ?c) comes from
//│        - l.8   test (if cond then false else check (not cond)) cond
//│                                              ^^^^^
//│   ◉ (?c) is assumed for
//│      - l.8   test (if cond then false else check (not cond)) cond
//│                                            ^^^^^^^^^^^^^^^^
//│     ◉ (bool -> ?c) comes from
//│     ▲  - l.8   test (if cond then false else check (not cond)) cond
//│     │                                        ^^^^^
//│     │  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     │ 
//│     ◉ (?check) is assumed for
//│     ▲  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     │ 
//│     ◉ (_ -> ?b) comes from
//│        - l.7  let rec check cond =
//│                             ^^^^^^
//│                test (if cond then false else check (not cond)) cond
//│                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   ◉ (?b) is assumed for
//│   ▲  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   │  - l.5   else wrap true
//│   │               ^^^^^^^^^
//│   │ 
//│   ◉ (?a0 list) comes from
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a0) is assumed for
//│   ◉ (?a0 list) comes from
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │  - l.5   else wrap true
//│   │               ^^^^^^^^^
//│   ▼ 
//│   ◉ (?b) is assumed for
//│      - l.3  let test z cond = if cond
//│                               ^^^^^^^
//│              then wrap z ...
//│              ^^^^^^^^^^^^^^^
//│     ◉ (_ -> ?b) comes from
//│     │  - l.7  let rec check cond =
//│     │                       ^^^^^^
//│     │          test (if cond then false else check (not cond)) cond
//│     │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│     │  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     ▼ 
//│     ◉ (?check) is assumed for
//│     │  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     ▼ 
//│     ◉ (bool -> ?c) comes from
//│        - l.8   test (if cond then false else check (not cond)) cond
//│                                              ^^^^^
//│   ◉ (?c) is assumed for
//│      - l.8   test (if cond then false else check (not cond)) cond
//│                                            ^^^^^^^^^^^^^^^^
//│     ◉ (bool -> ?c) comes from
//│     ▲  - l.8   test (if cond then false else check (not cond)) cond
//│     │                                        ^^^^^
//│     │  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     │ 
//│     ◉ (?check) is assumed for
//│     ▲  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     │ 
//│     ◉ (_ -> ?b) comes from
//│        - l.7  let rec check cond =
//│                             ^^^^^^
//│                test (if cond then false else check (not cond)) cond
//│                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   ◉ (?b) is assumed for
//│   ▲  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   │  - l.4   then wrap z
//│   │               ^^^^^^
//│   │ 
//│   ◉ (?a list) comes from
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a) is assumed for
//│   ◉ (?a list) comes from
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │  - l.4   then wrap z
//│   │               ^^^^^^
//│   ▼ 
//│   ◉ (?b) is assumed for
//│      - l.3  let test z cond = if cond
//│                               ^^^^^^^
//│              then wrap z ...
//│              ^^^^^^^^^^^^^^^
//│     ◉ (_ -> ?b) comes from
//│     │  - l.7  let rec check cond =
//│     │                       ^^^^^^
//│     │          test (if cond then false else check (not cond)) cond
//│     │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│     │  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     ▼ 
//│     ◉ (?check) is assumed for
//│     │  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     ▼ 
//│     ◉ (bool -> ?c) comes from
//│        - l.8   test (if cond then false else check (not cond)) cond
//│                                              ^^^^^
//│   ◉ (?c) is assumed for
//│   │  - l.8   test (if cond then false else check (not cond)) cond
//│   │                                        ^^^^^^^^^^^^^^^^
//│   │  - l.8   test (if cond then false else check (not cond)) cond
//│   │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   ▼ 
//│   ◉ (?d) is assumed for
//│   │  - l.8   test (if cond then false else check (not cond)) cond
//│   │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   │  - l.3  let test z cond = if cond
//│   │                  ^
//│   ▼ 
//│   ◉ (?e) is assumed for
//│   │  - l.3  let test z cond = if cond
//│   │                  ^
//│   │  - l.4   then wrap z
//│   │                    ^
//│   ▼ 
//│   ◉ (?f) is assumed for
//│      - l.1  let wrap x = x :: []
//│                      ^
//│     ◉ (?f * _ list) comes from
//│     │  - l.1  let wrap x = x :: []
//│     │                      ^^^^^^^
//│     ▼ 
//│     ◉ (?a * ?a list) comes from
//│        - l.1  let wrap x = x :: []
//│                            ^^^^^^^
//│   ◉ (?a) is assumed for
//│     ◉ (?a list) comes from
//│     │  - l.1  let wrap x = x :: []
//│     │                      ^^^^^^^
//│     │  - l.4   then wrap z
//│     │               ^^^^^^
//│     ▼ 
//│     ◉ (?b) is assumed for
//│        - l.3  let test z cond = if cond
//│                                 ^^^^^^^
//│                then wrap z ...
//│                ^^^^^^^^^^^^^^^
//│       ◉ (_ -> ?b) comes from
//│       │  - l.7  let rec check cond =
//│       │                       ^^^^^^
//│       │          test (if cond then false else check (not cond)) cond
//│       │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│       │  - l.7  let rec check cond =
//│       │                 ^^^^^
//│       ▼ 
//│       ◉ (?check) is assumed for
//│       │  - l.7  let rec check cond =
//│       │                 ^^^^^
//│       ▼ 
//│       ◉ (bool -> ?c) comes from
//│          - l.8   test (if cond then false else check (not cond)) cond
//│                                                ^^^^^
//│     ◉ (?c) is assumed for
//│        - l.8   test (if cond then false else check (not cond)) cond
//│                                              ^^^^^^^^^^^^^^^^
//│       ◉ (bool -> ?c) comes from
//│       ▲  - l.8   test (if cond then false else check (not cond)) cond
//│       │                                        ^^^^^
//│       │  - l.7  let rec check cond =
//│       │                 ^^^^^
//│       │ 
//│       ◉ (?check) is assumed for
//│       ▲  - l.7  let rec check cond =
//│       │                 ^^^^^
//│       │ 
//│       ◉ (_ -> ?b) comes from
//│          - l.7  let rec check cond =
//│                               ^^^^^^
//│                  test (if cond then false else check (not cond)) cond
//│                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│     ◉ (?b) is assumed for
//│     ▲  - l.3  let test z cond = if cond
//│     │                           ^^^^^^^
//│     │          then wrap z ...
//│     │          ^^^^^^^^^^^^^^^
//│     │  - l.5   else wrap true
//│     │               ^^^^^^^^^
//│     │ 
//│     ◉ (?a0 list) comes from
//│        - l.1  let wrap x = x :: []
//│                            ^^^^^^^
//│   ◉ (?a0) is assumed for
//│     ◉ (?a0 list) comes from
//│     │  - l.1  let wrap x = x :: []
//│     │                      ^^^^^^^
//│     │  - l.5   else wrap true
//│     │               ^^^^^^^^^
//│     │  - l.3  let test z cond = if cond
//│     │                           ^^^^^^^
//│     │          then wrap z ...
//│     │          ^^^^^^^^^^^^^^^
//│     ▼ 
//│     ◉ (?b) is assumed for
//│     ▲  - l.3  let test z cond = if cond
//│     │                           ^^^^^^^
//│     │          then wrap z ...
//│     │          ^^^^^^^^^^^^^^^
//│     │  - l.4   then wrap z
//│     │               ^^^^^^
//│     │ 
//│     ◉ (?a list) comes from
//│        - l.1  let wrap x = x :: []
//│                            ^^^^^^^
//│   ◉ (?a) is assumed for
//│     ◉ (?a * ?a list) comes from
//│     ▲  - l.1  let wrap x = x :: []
//│     │                      ^^^^^^^
//│     │ 
//│     ◉ (?f * _ list) comes from
//│        - l.1  let wrap x = x :: []
//│                            ^^^^^^^
//│   ◉ (?f) is assumed for
//│   ▲  - l.1  let wrap x = x :: []
//│   │                  ^
//│   │  - l.4   then wrap z
//│   │                    ^
//│   │  - l.3  let test z cond = if cond
//│   │                  ^
//│   │ 
//│   ◉ (?e) is assumed for
//│   ▲  - l.3  let test z cond = if cond
//│   │                  ^
//│   │  - l.8   test (if cond then false else check (not cond)) cond
//│   │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   │ 
//│   ◉ (?d) is assumed for
//│   ▲  - l.8   test (if cond then false else check (not cond)) cond
//│   │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   │ 
//│   ◉ (?c) is assumed for
//│      - l.8   test (if cond then false else check (not cond)) cond
//│                                            ^^^^^^^^^^^^^^^^
//│     ◉ (bool -> ?c) comes from
//│     ▲  - l.8   test (if cond then false else check (not cond)) cond
//│     │                                        ^^^^^
//│     │  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     │ 
//│     ◉ (?check) is assumed for
//│     ▲  - l.7  let rec check cond =
//│     │                 ^^^^^
//│     │ 
//│     ◉ (_ -> ?b) comes from
//│        - l.7  let rec check cond =
//│                             ^^^^^^
//│                test (if cond then false else check (not cond)) cond
//│                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│   ◉ (?b) is assumed for
//│   ▲  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   │  - l.5   else wrap true
//│   │               ^^^^^^^^^
//│   │ 
//│   ◉ (?a0 list) comes from
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a0) is assumed for
//│   ◉ (?a0 list) comes from
//│   │  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │  - l.5   else wrap true
//│   │               ^^^^^^^^^
//│   │  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   ▼ 
//│   ◉ (?b) is assumed for
//│   ▲  - l.3  let test z cond = if cond
//│   │                           ^^^^^^^
//│   │          then wrap z ...
//│   │          ^^^^^^^^^^^^^^^
//│   │  - l.4   then wrap z
//│   │               ^^^^^^
//│   │ 
//│   ◉ (?a list) comes from
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?a) is assumed for
//│   ◉ (?a * ?a list) comes from
//│   ▲  - l.1  let wrap x = x :: []
//│   │                      ^^^^^^^
//│   │ 
//│   ◉ (?f * _ list) comes from
//│      - l.1  let wrap x = x :: []
//│                          ^^^^^^^
//│ ◉ (?f) is assumed for
//│ ▲  - l.1  let wrap x = x :: []
//│ │                  ^
//│ │  - l.4   then wrap z
//│ │                    ^
//│ │  - l.3  let test z cond = if cond
//│ │                  ^
//│ │ 
//│ ◉ (?e) is assumed for
//│ ▲  - l.3  let test z cond = if cond
//│ │                  ^
//│ │  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │ 
//│ ◉ (?d) is assumed for
//│ ▲  - l.8   test (if cond then false else check (not cond)) cond
//│ │               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//│ │ 
//│ ◉ (bool) comes from
//│    - l.8   test (if cond then false else check (not cond)) cond
//│                               ^^^^^
//│ U max: 39, total: 167
//│ UERR 6 errors
//│ L: 2 [list['a64'] ~ bool, list['a64'] <: α67', [α67' - (α61' -> [α67']) ~ ([bool] -> α74') - α74', L: 0 [(α61' -> [α67']) ~ ([bool] -> α74'), (α61' -> [α67']) <: check60', check60' <: ([bool] -> α74')]], α74' <: α71', α71' <: α62', α62' <: α69', [α69' - ([[α69']], [[list['a68']]],) ~ ('a66', list['a66'],) - 'a66', L: 0 [([[α69']], [[list['a68']]],) ~ ('a66', list['a66'],), ([[α69']], [[list['a68']]],) <: ('a66', list['a66'],)]], ['a66' - list['a66'] ~ list['a64'] - 'a64', L: 0 [list['a66'] ~ list['a64'], list['a66'] <: α67', α67' :> list['a64']]], ['a64' - list['a64'] ~ list['a65'] - 'a65', L: 1 [list['a64'] ~ list['a65'], [list['a64'] - ('a64', list['a64'],) ~ ([[α63']], [[list['a65']]],) - list['a65'], L: 0 [('a64', list['a64'],) ~ ([[α63']], [[list['a65']]],), ('a64', list['a64'],) :> ([[α63']], [[list['a65']]],)]]]], ['a65' - list['a65'] ~ list['a64'] - 'a64', L: 1 [list['a65'] ~ list['a64'], [list['a65'] - ([[α63']], [[list['a65']]],) ~ ('a64', list['a64'],) - list['a64'], L: 0 [([[α63']], [[list['a65']]],) ~ ('a64', list['a64'],), ([[α63']], [[list['a65']]],) <: ('a64', list['a64'],)]]]], ['a64' - ('a64', list['a64'],) ~ ([[α63']], [[list['a65']]],) - α63', L: 0 [('a64', list['a64'],) ~ ([[α63']], [[list['a65']]],), ('a64', list['a64'],) :> ([[α63']], [[list['a65']]],)]], [α63' - ([[α63']], [[list['a65']]],) ~ ('a64', list['a64'],) - 'a64', L: 0 [([[α63']], [[list['a65']]],) ~ ('a64', list['a64'],), ([[α63']], [[list['a65']]],) <: ('a64', list['a64'],)]], ['a64' - list['a64'] ~ list['a65'] - 'a65', L: 1 [list['a64'] ~ list['a65'], [list['a64'] - ('a64', list['a64'],) ~ ([[α63']], [[list['a65']]],) - list['a65'], L: 0 [('a64', list['a64'],) ~ ([[α63']], [[list['a65']]],), ('a64', list['a64'],) :> ([[α63']], [[list['a65']]],)]]]], ['a65' - list['a65'] ~ list['a64'] - 'a64', L: 1 [list['a65'] ~ list['a64'], [list['a65'] - ([[α63']], [[list['a65']]],) ~ ('a64', list['a64'],) - list['a64'], L: 0 [([[α63']], [[list['a65']]],) ~ ('a64', list['a64'],), ([[α63']], [[list['a65']]],) <: ('a64', list['a64'],)]]]], ['a64' - list['a64'] ~ list['a66'] - 'a66', L: 0 [list['a64'] ~ list['a66'], list['a64'] <: α67', α67' :> list['a66']]], ['a66' - ('a66', list['a66'],) ~ ([[α69']], [[list['a68']]],) - α69', L: 0 [('a66', list['a66'],) ~ ([[α69']], [[list['a68']]],), ('a66', list['a66'],) :> ([[α69']], [[list['a68']]],)]], α69' :> α62', α62' :> α71', α71' :> bool]
//│ L: 1 [list['a64'] ~ bool, list['a64'] <: α67', [α67' - (α61' -> [α67']) ~ ([bool] -> α74') - α74', L: 0 [(α61' -> [α67']) ~ ([bool] -> α74'), (α61' -> [α67']) <: check60', check60' <: ([bool] -> α74')]], α74' <: α71', α71' <: α62', α62' <: α69', [α69' - ([[α69']], [[list['a68']]],) ~ ('a66', list['a66'],) - 'a66', L: 0 [([[α69']], [[list['a68']]],) ~ ('a66', list['a66'],), ([[α69']], [[list['a68']]],) <: ('a66', list['a66'],)]], ['a66' - ('a66', list['a66'],) ~ ([[α69']], [[list['a68']]],) - α69', L: 0 [('a66', list['a66'],) ~ ([[α69']], [[list['a68']]],), ('a66', list['a66'],) :> ([[α69']], [[list['a68']]],)]], α69' :> α62', α62' :> α71', α71' :> bool]
//│ L: 2 [list['a66'] ~ bool, list['a66'] <: α67', [α67' - (α61' -> [α67']) ~ ([bool] -> α74') - α74', L: 0 [(α61' -> [α67']) ~ ([bool] -> α74'), (α61' -> [α67']) <: check60', check60' <: ([bool] -> α74')]], α74' <: α71', α71' <: α62', α62' <: α69', [α69' - ([[α69']], [[list['a68']]],) ~ ('a66', list['a66'],) - 'a66', L: 0 [([[α69']], [[list['a68']]],) ~ ('a66', list['a66'],), ([[α69']], [[list['a68']]],) <: ('a66', list['a66'],)]], ['a66' - list['a66'] ~ list['a64'] - 'a64', L: 0 [list['a66'] ~ list['a64'], list['a66'] <: α67', α67' :> list['a64']]], ['a64' - list['a64'] ~ list['a65'] - 'a65', L: 1 [list['a64'] ~ list['a65'], [list['a64'] - ('a64', list['a64'],) ~ ([[α63']], [[list['a65']]],) - list['a65'], L: 0 [('a64', list['a64'],) ~ ([[α63']], [[list['a65']]],), ('a64', list['a64'],) :> ([[α63']], [[list['a65']]],)]]]], ['a65' - list['a65'] ~ list['a64'] - 'a64', L: 1 [list['a65'] ~ list['a64'], [list['a65'] - ([[α63']], [[list['a65']]],) ~ ('a64', list['a64'],) - list['a64'], L: 0 [([[α63']], [[list['a65']]],) ~ ('a64', list['a64'],), ([[α63']], [[list['a65']]],) <: ('a64', list['a64'],)]]]], ['a64' - ('a64', list['a64'],) ~ ([[α63']], [[list['a65']]],) - α63', L: 0 [('a64', list['a64'],) ~ ([[α63']], [[list['a65']]],), ('a64', list['a64'],) :> ([[α63']], [[list['a65']]],)]], [α63' - ([[α63']], [[list['a65']]],) ~ ('a64', list['a64'],) - 'a64', L: 0 [([[α63']], [[list['a65']]],) ~ ('a64', list['a64'],), ([[α63']], [[list['a65']]],) <: ('a64', list['a64'],)]], ['a64' - list['a64'] ~ list['a65'] - 'a65', L: 1 [list['a64'] ~ list['a65'], [list['a64'] - ('a64', list['a64'],) ~ ([[α63']], [[list['a65']]],) - list['a65'], L: 0 [('a64', list['a64'],) ~ ([[α63']], [[list['a65']]],), ('a64', list['a64'],) :> ([[α63']], [[list['a65']]],)]]]], ['a65' - list['a65'] ~ list['a64'] - 'a64', L: 1 [list['a65'] ~ list['a64'], [list['a65'] - ([[α63']], [[list['a65']]],) ~ ('a64', list['a64'],) - list['a64'], L: 0 [([[α63']], [[list['a65']]],) ~ ('a64', list['a64'],), ([[α63']], [[list['a65']]],) <: ('a64', list['a64'],)]]]], ['a64' - list['a64'] ~ list['a66'] - 'a66', L: 0 [list['a64'] ~ list['a66'], list['a64'] <: α67', α67' :> list['a66']]], ['a66' - ('a66', list['a66'],) ~ ([[α69']], [[list['a68']]],) - α69', L: 0 [('a66', list['a66'],) ~ ([[α69']], [[list['a68']]],), ('a66', list['a66'],) :> ([[α69']], [[list['a68']]],)]], α69' :> α62', α62' :> α71', α71' :> bool]
//│ L: 3 [list['a66'] ~ bool, list['a66'] <: α67', [α67' - (α61' -> [α67']) ~ ([bool] -> α74') - α74', L: 0 [(α61' -> [α67']) ~ ([bool] -> α74'), (α61' -> [α67']) <: check60', check60' <: ([bool] -> α74')]], α74' <: α71', α71' <: α62', α62' <: α69', [α69' - ([[α69']], [[list['a68']]],) ~ ('a66', list['a66'],) - 'a66', L: 0 [([[α69']], [[list['a68']]],) ~ ('a66', list['a66'],), ([[α69']], [[list['a68']]],) <: ('a66', list['a66'],)]], ['a66' - list['a66'] ~ list['a64'] - 'a64', L: 1 [list['a66'] ~ list['a64'], list['a66'] <: α67', [α67' - (α61' -> [α67']) ~ ([bool] -> α74') - α74', L: 0 [(α61' -> [α67']) ~ ([bool] -> α74'), (α61' -> [α67']) <: check60', check60' <: ([bool] -> α74')]], [α74' - ([bool] -> α74') ~ (α61' -> [α67']) - α67', L: 0 [([bool] -> α74') ~ (α61' -> [α67']), ([bool] -> α74') :> check60', check60' :> (α61' -> [α67'])]], α67' :> list['a64']]], ['a64' - list['a64'] ~ list['a66'] - 'a66', L: 1 [list['a64'] ~ list['a66'], list['a64'] <: α67', [α67' - (α61' -> [α67']) ~ ([bool] -> α74') - α74', L: 0 [(α61' -> [α67']) ~ ([bool] -> α74'), (α61' -> [α67']) <: check60', check60' <: ([bool] -> α74')]], [α74' - ([bool] -> α74') ~ (α61' -> [α67']) - α67', L: 0 [([bool] -> α74') ~ (α61' -> [α67']), ([bool] -> α74') :> check60', check60' :> (α61' -> [α67'])]], α67' :> list['a66']]], ['a66' - list['a66'] ~ list['a64'] - 'a64', L: 2 [list['a66'] ~ list['a64'], list['a66'] <: α67', [α67' - (α61' -> [α67']) ~ ([bool] -> α74') - α74', L: 0 [(α61' -> [α67']) ~ ([bool] -> α74'), (α61' -> [α67']) <: check60', check60' <: ([bool] -> α74')]], α74' <: α71', α71' <: α62', α62' <: α69', [α69' - ([[α69']], [[list['a68']]],) ~ ('a66', list['a66'],) - 'a66', L: 0 [([[α69']], [[list['a68']]],) ~ ('a66', list['a66'],), ([[α69']], [[list['a68']]],) <: ('a66', list['a66'],)]], ['a66' - list['a66'] ~ list['a64'] - 'a64', L: 1 [list['a66'] ~ list['a64'], list['a66'] <: α67', [α67' - (α61' -> [α67']) ~ ([bool] -> α74') - α74', L: 0 [(α61' -> [α67']) ~ ([bool] -> α74'), (α61' -> [α67']) <: check60', check60' <: ([bool] -> α74')]], [α74' - ([bool] -> α74') ~ (α61' -> [α67']) - α67', L: 0 [([bool] -> α74') ~ (α61' -> [α67']), ([bool] -> α74') :> check60', check60' :> (α61' -> [α67'])]], α67' :> list['a64']]], ['a64' - list['a64'] ~ list['a66'] - 'a66', L: 0 [list['a64'] ~ list['a66'], list['a64'] <: α67', α67' :> list['a66']]], ['a66' - ('a66', list['a66'],) ~ ([[α69']], [[list['a68']]],) - α69', L: 0 [('a66', list['a66'],) ~ ([[α69']], [[list['a68']]],), ('a66', list['a66'],) :> ([[α69']], [[list['a68']]],)]], α69' :> α62', α62' :> α71', α71' :> α74', [α74' - ([bool] -> α74') ~ (α61' -> [α67']) - α67', L: 0 [([bool] -> α74') ~ (α61' -> [α67']), ([bool] -> α74') :> check60', check60' :> (α61' -> [α67'])]], α67' :> list['a64']]], ['a64' - list['a64'] ~ list['a66'] - 'a66', L: 0 [list['a64'] ~ list['a66'], list['a64'] <: α67', α67' :> list['a66']]], ['a66' - ('a66', list['a66'],) ~ ([[α69']], [[list['a68']]],) - α69', L: 0 [('a66', list['a66'],) ~ ([[α69']], [[list['a68']]],), ('a66', list['a66'],) :> ([[α69']], [[list['a68']]],)]], α69' :> α62', α62' :> α71', α71' :> bool]
//│ L: 3 [list['a64'] ~ bool, list['a64'] <: α67', [α67' - (α61' -> [α67']) ~ ([bool] -> α74') - α74', L: 0 [(α61' -> [α67']) ~ ([bool] -> α74'), (α61' -> [α67']) <: check60', check60' <: ([bool] -> α74')]], α74' <: α71', α71' <: α62', α62' <: α69', [α69' - ([[α69']], [[list['a68']]],) ~ ('a66', list['a66'],) - 'a66', L: 0 [([[α69']], [[list['a68']]],) ~ ('a66', list['a66'],), ([[α69']], [[list['a68']]],) <: ('a66', list['a66'],)]], ['a66' - list['a66'] ~ list['a64'] - 'a64', L: 1 [list['a66'] ~ list['a64'], list['a66'] <: α67', [α67' - (α61' -> [α67']) ~ ([bool] -> α74') - α74', L: 0 [(α61' -> [α67']) ~ ([bool] -> α74'), (α61' -> [α67']) <: check60', check60' <: ([bool] -> α74')]], [α74' - ([bool] -> α74') ~ (α61' -> [α67']) - α67', L: 0 [([bool] -> α74') ~ (α61' -> [α67']), ([bool] -> α74') :> check60', check60' :> (α61' -> [α67'])]], α67' :> list['a64']]], ['a64' - list['a64'] ~ list['a66'] - 'a66', L: 1 [list['a64'] ~ list['a66'], list['a64'] <: α67', [α67' - (α61' -> [α67']) ~ ([bool] -> α74') - α74', L: 0 [(α61' -> [α67']) ~ ([bool] -> α74'), (α61' -> [α67']) <: check60', check60' <: ([bool] -> α74')]], [α74' - ([bool] -> α74') ~ (α61' -> [α67']) - α67', L: 0 [([bool] -> α74') ~ (α61' -> [α67']), ([bool] -> α74') :> check60', check60' :> (α61' -> [α67'])]], α67' :> list['a66']]], ['a66' - list['a66'] ~ list['a64'] - 'a64', L: 2 [list['a66'] ~ list['a64'], list['a66'] <: α67', [α67' - (α61' -> [α67']) ~ ([bool] -> α74') - α74', L: 0 [(α61' -> [α67']) ~ ([bool] -> α74'), (α61' -> [α67']) <: check60', check60' <: ([bool] -> α74')]], α74' <: α71', α71' <: α62', α62' <: α69', [α69' - ([[α69']], [[list['a68']]],) ~ ('a66', list['a66'],) - 'a66', L: 0 [([[α69']], [[list['a68']]],) ~ ('a66', list['a66'],), ([[α69']], [[list['a68']]],) <: ('a66', list['a66'],)]], ['a66' - list['a66'] ~ list['a64'] - 'a64', L: 1 [list['a66'] ~ list['a64'], list['a66'] <: α67', [α67' - (α61' -> [α67']) ~ ([bool] -> α74') - α74', L: 0 [(α61' -> [α67']) ~ ([bool] -> α74'), (α61' -> [α67']) <: check60', check60' <: ([bool] -> α74')]], [α74' - ([bool] -> α74') ~ (α61' -> [α67']) - α67', L: 0 [([bool] -> α74') ~ (α61' -> [α67']), ([bool] -> α74') :> check60', check60' :> (α61' -> [α67'])]], α67' :> list['a64']]], ['a64' - list['a64'] ~ list['a66'] - 'a66', L: 0 [list['a64'] ~ list['a66'], list['a64'] <: α67', α67' :> list['a66']]], ['a66' - ('a66', list['a66'],) ~ ([[α69']], [[list['a68']]],) - α69', L: 0 [('a66', list['a66'],) ~ ([[α69']], [[list['a68']]],), ('a66', list['a66'],) :> ([[α69']], [[list['a68']]],)]], α69' :> α62', α62' :> α71', α71' :> α74', [α74' - ([bool] -> α74') ~ (α61' -> [α67']) - α67', L: 0 [([bool] -> α74') ~ (α61' -> [α67']), ([bool] -> α74') :> check60', check60' :> (α61' -> [α67'])]], α67' :> list['a64']]], ['a64' - list['a64'] ~ list['a66'] - 'a66', L: 0 [list['a64'] ~ list['a66'], list['a64'] <: α67', α67' :> list['a66']]], ['a66' - ('a66', list['a66'],) ~ ([[α69']], [[list['a68']]],) - α69', L: 0 [('a66', list['a66'],) ~ ([[α69']], [[list['a68']]],), ('a66', list['a66'],) :> ([[α69']], [[list['a68']]],)]], α69' :> α62', α62' :> α71', α71' :> bool]
//│ L: 1 [list['a66'] ~ bool, list['a66'] <: α67', [α67' - (α61' -> [α67']) ~ ([bool] -> α74') - α74', L: 0 [(α61' -> [α67']) ~ ([bool] -> α74'), (α61' -> [α67']) <: check60', check60' <: ([bool] -> α74')]], α74' <: α71', α71' <: α62', α62' <: α69', [α69' - ([[α69']], [[list['a68']]],) ~ ('a66', list['a66'],) - 'a66', L: 0 [([[α69']], [[list['a68']]],) ~ ('a66', list['a66'],), ([[α69']], [[list['a68']]],) <: ('a66', list['a66'],)]], ['a66' - ('a66', list['a66'],) ~ ([[α69']], [[list['a68']]],) - α69', L: 0 [('a66', list['a66'],) ~ ([[α69']], [[list['a68']]],), ('a66', list['a66'],) :> ([[α69']], [[list['a68']]],)]], α69' :> α62', α62' :> α71', α71' :> bool]


