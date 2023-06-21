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
//│ L: 3 [list['a88'] ~ bool, list['a88'] <: α89', [α89' - (α83' -> [α89']) ~ ([bool] -> α96') - α96', L: 0 [(α83' -> [α89']) ~ ([bool] -> α96'), (α83' -> [α89']) <: check82', check82' <: ([bool] -> α96')]], α96' <: α93', α93' <: α84', α84' <: α91', [α91' - ([[α91']], [[list['a90']]],) ~ ('a88', list['a88'],) - 'a88', L: 0 [([[α91']], [[list['a90']]],) ~ ('a88', list['a88'],), ([[α91']], [[list['a90']]],) <: ('a88', list['a88'],)]], ['a88' - list['a88'] ~ list['a86'] - 'a86', L: 1 [list['a88'] ~ list['a86'], list['a88'] <: α89', [α89' - (α83' -> [α89']) ~ ([bool] -> α96') - α96', L: 0 [(α83' -> [α89']) ~ ([bool] -> α96'), (α83' -> [α89']) <: check82', check82' <: ([bool] -> α96')]], [α96' - ([bool] -> α96') ~ (α83' -> [α89']) - α89', L: 0 [([bool] -> α96') ~ (α83' -> [α89']), ([bool] -> α96') :> check82', check82' :> (α83' -> [α89'])]], α89' :> list['a86']]], ['a86' - list['a86'] ~ list['a88'] - 'a88', L: 1 [list['a86'] ~ list['a88'], list['a86'] <: α89', [α89' - (α83' -> [α89']) ~ ([bool] -> α96') - α96', L: 0 [(α83' -> [α89']) ~ ([bool] -> α96'), (α83' -> [α89']) <: check82', check82' <: ([bool] -> α96')]], [α96' - ([bool] -> α96') ~ (α83' -> [α89']) - α89', L: 0 [([bool] -> α96') ~ (α83' -> [α89']), ([bool] -> α96') :> check82', check82' :> (α83' -> [α89'])]], α89' :> list['a88']]], ['a88' - list['a88'] ~ list['a86'] - 'a86', L: 2 [list['a88'] ~ list['a86'], list['a88'] <: α89', [α89' - (α83' -> [α89']) ~ ([bool] -> α96') - α96', L: 0 [(α83' -> [α89']) ~ ([bool] -> α96'), (α83' -> [α89']) <: check82', check82' <: ([bool] -> α96')]], α96' <: α93', α93' <: α84', α84' <: α91', [α91' - ([[α91']], [[list['a90']]],) ~ ('a88', list['a88'],) - 'a88', L: 0 [([[α91']], [[list['a90']]],) ~ ('a88', list['a88'],), ([[α91']], [[list['a90']]],) <: ('a88', list['a88'],)]], ['a88' - list['a88'] ~ list['a86'] - 'a86', L: 1 [list['a88'] ~ list['a86'], list['a88'] <: α89', [α89' - (α83' -> [α89']) ~ ([bool] -> α96') - α96', L: 0 [(α83' -> [α89']) ~ ([bool] -> α96'), (α83' -> [α89']) <: check82', check82' <: ([bool] -> α96')]], [α96' - ([bool] -> α96') ~ (α83' -> [α89']) - α89', L: 0 [([bool] -> α96') ~ (α83' -> [α89']), ([bool] -> α96') :> check82', check82' :> (α83' -> [α89'])]], α89' :> list['a86']]], ['a86' - list['a86'] ~ list['a88'] - 'a88', L: 0 [list['a86'] ~ list['a88'], list['a86'] <: α89', α89' :> list['a88']]], ['a88' - ('a88', list['a88'],) ~ ([[α91']], [[list['a90']]],) - α91', L: 0 [('a88', list['a88'],) ~ ([[α91']], [[list['a90']]],), ('a88', list['a88'],) :> ([[α91']], [[list['a90']]],)]], α91' :> α84', α84' :> α93', α93' :> α96', [α96' - ([bool] -> α96') ~ (α83' -> [α89']) - α89', L: 0 [([bool] -> α96') ~ (α83' -> [α89']), ([bool] -> α96') :> check82', check82' :> (α83' -> [α89'])]], α89' :> list['a86']]], ['a86' - list['a86'] ~ list['a88'] - 'a88', L: 0 [list['a86'] ~ list['a88'], list['a86'] <: α89', α89' :> list['a88']]], ['a88' - ('a88', list['a88'],) ~ ([[α91']], [[list['a90']]],) - α91', L: 0 [('a88', list['a88'],) ~ ([[α91']], [[list['a90']]],), ('a88', list['a88'],) :> ([[α91']], [[list['a90']]],)]], α91' :> α84', α84' :> α93', α93' :> bool]
//│ L: 1 [list['a88'] ~ bool, list['a88'] <: α89', [α89' - (α83' -> [α89']) ~ ([bool] -> α96') - α96', L: 0 [(α83' -> [α89']) ~ ([bool] -> α96'), (α83' -> [α89']) <: check82', check82' <: ([bool] -> α96')]], α96' <: α93', α93' <: α84', α84' <: α91', [α91' - ([[α91']], [[list['a90']]],) ~ ('a88', list['a88'],) - 'a88', L: 0 [([[α91']], [[list['a90']]],) ~ ('a88', list['a88'],), ([[α91']], [[list['a90']]],) <: ('a88', list['a88'],)]], ['a88' - ('a88', list['a88'],) ~ ([[α91']], [[list['a90']]],) - α91', L: 0 [('a88', list['a88'],) ~ ([[α91']], [[list['a90']]],), ('a88', list['a88'],) :> ([[α91']], [[list['a90']]],)]], α91' :> α84', α84' :> α93', α93' :> bool]
//│ L: 1 [list['a86'] ~ bool, list['a86'] <: α89', [α89' - (α83' -> [α89']) ~ ([bool] -> α96') - α96', L: 0 [(α83' -> [α89']) ~ ([bool] -> α96'), (α83' -> [α89']) <: check82', check82' <: ([bool] -> α96')]], α96' <: α93', α93' <: α84', α84' <: α91', [α91' - ([[α91']], [[list['a90']]],) ~ ('a88', list['a88'],) - 'a88', L: 0 [([[α91']], [[list['a90']]],) ~ ('a88', list['a88'],), ([[α91']], [[list['a90']]],) <: ('a88', list['a88'],)]], ['a88' - ('a88', list['a88'],) ~ ([[α91']], [[list['a90']]],) - α91', L: 0 [('a88', list['a88'],) ~ ([[α91']], [[list['a90']]],), ('a88', list['a88'],) :> ([[α91']], [[list['a90']]],)]], α91' :> α84', α84' :> α93', α93' :> bool]
//│ L: 2 [list['a88'] ~ bool, list['a88'] <: α89', [α89' - (α83' -> [α89']) ~ ([bool] -> α96') - α96', L: 0 [(α83' -> [α89']) ~ ([bool] -> α96'), (α83' -> [α89']) <: check82', check82' <: ([bool] -> α96')]], α96' <: α93', α93' <: α84', α84' <: α91', [α91' - ([[α91']], [[list['a90']]],) ~ ('a88', list['a88'],) - 'a88', L: 0 [([[α91']], [[list['a90']]],) ~ ('a88', list['a88'],), ([[α91']], [[list['a90']]],) <: ('a88', list['a88'],)]], ['a88' - list['a88'] ~ list['a86'] - 'a86', L: 0 [list['a88'] ~ list['a86'], list['a88'] <: α89', α89' :> list['a86']]], ['a86' - list['a86'] ~ list['a87'] - 'a87', L: 1 [list['a86'] ~ list['a87'], [list['a86'] - ('a86', list['a86'],) ~ ([[α85']], [[list['a87']]],) - list['a87'], L: 0 [('a86', list['a86'],) ~ ([[α85']], [[list['a87']]],), ('a86', list['a86'],) :> ([[α85']], [[list['a87']]],)]]]], ['a87' - list['a87'] ~ list['a86'] - 'a86', L: 1 [list['a87'] ~ list['a86'], [list['a87'] - ([[α85']], [[list['a87']]],) ~ ('a86', list['a86'],) - list['a86'], L: 0 [([[α85']], [[list['a87']]],) ~ ('a86', list['a86'],), ([[α85']], [[list['a87']]],) <: ('a86', list['a86'],)]]]], ['a86' - ('a86', list['a86'],) ~ ([[α85']], [[list['a87']]],) - α85', L: 0 [('a86', list['a86'],) ~ ([[α85']], [[list['a87']]],), ('a86', list['a86'],) :> ([[α85']], [[list['a87']]],)]], [α85' - ([[α85']], [[list['a87']]],) ~ ('a86', list['a86'],) - 'a86', L: 0 [([[α85']], [[list['a87']]],) ~ ('a86', list['a86'],), ([[α85']], [[list['a87']]],) <: ('a86', list['a86'],)]], ['a86' - list['a86'] ~ list['a87'] - 'a87', L: 1 [list['a86'] ~ list['a87'], [list['a86'] - ('a86', list['a86'],) ~ ([[α85']], [[list['a87']]],) - list['a87'], L: 0 [('a86', list['a86'],) ~ ([[α85']], [[list['a87']]],), ('a86', list['a86'],) :> ([[α85']], [[list['a87']]],)]]]], ['a87' - list['a87'] ~ list['a86'] - 'a86', L: 1 [list['a87'] ~ list['a86'], [list['a87'] - ([[α85']], [[list['a87']]],) ~ ('a86', list['a86'],) - list['a86'], L: 0 [([[α85']], [[list['a87']]],) ~ ('a86', list['a86'],), ([[α85']], [[list['a87']]],) <: ('a86', list['a86'],)]]]], ['a86' - list['a86'] ~ list['a88'] - 'a88', L: 0 [list['a86'] ~ list['a88'], list['a86'] <: α89', α89' :> list['a88']]], ['a88' - ('a88', list['a88'],) ~ ([[α91']], [[list['a90']]],) - α91', L: 0 [('a88', list['a88'],) ~ ([[α91']], [[list['a90']]],), ('a88', list['a88'],) :> ([[α91']], [[list['a90']]],)]], α91' :> α84', α84' :> α93', α93' :> bool]
//│ L: 3 [list['a86'] ~ bool, list['a86'] <: α89', [α89' - (α83' -> [α89']) ~ ([bool] -> α96') - α96', L: 0 [(α83' -> [α89']) ~ ([bool] -> α96'), (α83' -> [α89']) <: check82', check82' <: ([bool] -> α96')]], α96' <: α93', α93' <: α84', α84' <: α91', [α91' - ([[α91']], [[list['a90']]],) ~ ('a88', list['a88'],) - 'a88', L: 0 [([[α91']], [[list['a90']]],) ~ ('a88', list['a88'],), ([[α91']], [[list['a90']]],) <: ('a88', list['a88'],)]], ['a88' - list['a88'] ~ list['a86'] - 'a86', L: 1 [list['a88'] ~ list['a86'], list['a88'] <: α89', [α89' - (α83' -> [α89']) ~ ([bool] -> α96') - α96', L: 0 [(α83' -> [α89']) ~ ([bool] -> α96'), (α83' -> [α89']) <: check82', check82' <: ([bool] -> α96')]], [α96' - ([bool] -> α96') ~ (α83' -> [α89']) - α89', L: 0 [([bool] -> α96') ~ (α83' -> [α89']), ([bool] -> α96') :> check82', check82' :> (α83' -> [α89'])]], α89' :> list['a86']]], ['a86' - list['a86'] ~ list['a88'] - 'a88', L: 1 [list['a86'] ~ list['a88'], list['a86'] <: α89', [α89' - (α83' -> [α89']) ~ ([bool] -> α96') - α96', L: 0 [(α83' -> [α89']) ~ ([bool] -> α96'), (α83' -> [α89']) <: check82', check82' <: ([bool] -> α96')]], [α96' - ([bool] -> α96') ~ (α83' -> [α89']) - α89', L: 0 [([bool] -> α96') ~ (α83' -> [α89']), ([bool] -> α96') :> check82', check82' :> (α83' -> [α89'])]], α89' :> list['a88']]], ['a88' - list['a88'] ~ list['a86'] - 'a86', L: 2 [list['a88'] ~ list['a86'], list['a88'] <: α89', [α89' - (α83' -> [α89']) ~ ([bool] -> α96') - α96', L: 0 [(α83' -> [α89']) ~ ([bool] -> α96'), (α83' -> [α89']) <: check82', check82' <: ([bool] -> α96')]], α96' <: α93', α93' <: α84', α84' <: α91', [α91' - ([[α91']], [[list['a90']]],) ~ ('a88', list['a88'],) - 'a88', L: 0 [([[α91']], [[list['a90']]],) ~ ('a88', list['a88'],), ([[α91']], [[list['a90']]],) <: ('a88', list['a88'],)]], ['a88' - list['a88'] ~ list['a86'] - 'a86', L: 1 [list['a88'] ~ list['a86'], list['a88'] <: α89', [α89' - (α83' -> [α89']) ~ ([bool] -> α96') - α96', L: 0 [(α83' -> [α89']) ~ ([bool] -> α96'), (α83' -> [α89']) <: check82', check82' <: ([bool] -> α96')]], [α96' - ([bool] -> α96') ~ (α83' -> [α89']) - α89', L: 0 [([bool] -> α96') ~ (α83' -> [α89']), ([bool] -> α96') :> check82', check82' :> (α83' -> [α89'])]], α89' :> list['a86']]], ['a86' - list['a86'] ~ list['a88'] - 'a88', L: 0 [list['a86'] ~ list['a88'], list['a86'] <: α89', α89' :> list['a88']]], ['a88' - ('a88', list['a88'],) ~ ([[α91']], [[list['a90']]],) - α91', L: 0 [('a88', list['a88'],) ~ ([[α91']], [[list['a90']]],), ('a88', list['a88'],) :> ([[α91']], [[list['a90']]],)]], α91' :> α84', α84' :> α93', α93' :> α96', [α96' - ([bool] -> α96') ~ (α83' -> [α89']) - α89', L: 0 [([bool] -> α96') ~ (α83' -> [α89']), ([bool] -> α96') :> check82', check82' :> (α83' -> [α89'])]], α89' :> list['a86']]], ['a86' - list['a86'] ~ list['a88'] - 'a88', L: 0 [list['a86'] ~ list['a88'], list['a86'] <: α89', α89' :> list['a88']]], ['a88' - ('a88', list['a88'],) ~ ([[α91']], [[list['a90']]],) - α91', L: 0 [('a88', list['a88'],) ~ ([[α91']], [[list['a90']]],), ('a88', list['a88'],) :> ([[α91']], [[list['a90']]],)]], α91' :> α84', α84' :> α93', α93' :> bool]
//│ L: 2 [list['a86'] ~ bool, list['a86'] <: α89', [α89' - (α83' -> [α89']) ~ ([bool] -> α96') - α96', L: 0 [(α83' -> [α89']) ~ ([bool] -> α96'), (α83' -> [α89']) <: check82', check82' <: ([bool] -> α96')]], α96' <: α93', α93' <: α84', α84' <: α91', [α91' - ([[α91']], [[list['a90']]],) ~ ('a88', list['a88'],) - 'a88', L: 0 [([[α91']], [[list['a90']]],) ~ ('a88', list['a88'],), ([[α91']], [[list['a90']]],) <: ('a88', list['a88'],)]], ['a88' - list['a88'] ~ list['a86'] - 'a86', L: 0 [list['a88'] ~ list['a86'], list['a88'] <: α89', α89' :> list['a86']]], ['a86' - list['a86'] ~ list['a87'] - 'a87', L: 1 [list['a86'] ~ list['a87'], [list['a86'] - ('a86', list['a86'],) ~ ([[α85']], [[list['a87']]],) - list['a87'], L: 0 [('a86', list['a86'],) ~ ([[α85']], [[list['a87']]],), ('a86', list['a86'],) :> ([[α85']], [[list['a87']]],)]]]], ['a87' - list['a87'] ~ list['a86'] - 'a86', L: 1 [list['a87'] ~ list['a86'], [list['a87'] - ([[α85']], [[list['a87']]],) ~ ('a86', list['a86'],) - list['a86'], L: 0 [([[α85']], [[list['a87']]],) ~ ('a86', list['a86'],), ([[α85']], [[list['a87']]],) <: ('a86', list['a86'],)]]]], ['a86' - ('a86', list['a86'],) ~ ([[α85']], [[list['a87']]],) - α85', L: 0 [('a86', list['a86'],) ~ ([[α85']], [[list['a87']]],), ('a86', list['a86'],) :> ([[α85']], [[list['a87']]],)]], [α85' - ([[α85']], [[list['a87']]],) ~ ('a86', list['a86'],) - 'a86', L: 0 [([[α85']], [[list['a87']]],) ~ ('a86', list['a86'],), ([[α85']], [[list['a87']]],) <: ('a86', list['a86'],)]], ['a86' - list['a86'] ~ list['a87'] - 'a87', L: 1 [list['a86'] ~ list['a87'], [list['a86'] - ('a86', list['a86'],) ~ ([[α85']], [[list['a87']]],) - list['a87'], L: 0 [('a86', list['a86'],) ~ ([[α85']], [[list['a87']]],), ('a86', list['a86'],) :> ([[α85']], [[list['a87']]],)]]]], ['a87' - list['a87'] ~ list['a86'] - 'a86', L: 1 [list['a87'] ~ list['a86'], [list['a87'] - ([[α85']], [[list['a87']]],) ~ ('a86', list['a86'],) - list['a86'], L: 0 [([[α85']], [[list['a87']]],) ~ ('a86', list['a86'],), ([[α85']], [[list['a87']]],) <: ('a86', list['a86'],)]]]], ['a86' - list['a86'] ~ list['a88'] - 'a88', L: 0 [list['a86'] ~ list['a88'], list['a86'] <: α89', α89' :> list['a88']]], ['a88' - ('a88', list['a88'],) ~ ([[α91']], [[list['a90']]],) - α91', L: 0 [('a88', list['a88'],) ~ ([[α91']], [[list['a90']]],), ('a88', list['a88'],) :> ([[α91']], [[list['a90']]],)]], α91' :> α84', α84' :> α93', α93' :> bool]


