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
//│ L: 1 [list['a63'] ~ bool, list['a63'] <: α64', [α64' - (α58' -> [α64']) ~ ([bool] -> α69') - α69', L: 0 [(α58' -> [α64']) ~ ([bool] -> α69'), (α58' -> [α64']) <: check57', check57' <: ([bool] -> α69')]], α69' <: α68', α68' <: α59', α59' <: α66', [α66' - ([[α66']], [[list['a65']]],) ~ ('a63', list['a63'],) - 'a63', L: 0 [([[α66']], [[list['a65']]],) ~ ('a63', list['a63'],), ([[α66']], [[list['a65']]],) <: ('a63', list['a63'],)]], ['a63' - ('a63', list['a63'],) ~ ([[α66']], [[list['a65']]],) - α66', L: 0 [('a63', list['a63'],) ~ ([[α66']], [[list['a65']]],), ('a63', list['a63'],) :> ([[α66']], [[list['a65']]],)]], α66' :> α59', α59' :> α68', α68' :> bool]
//│ L: 2 [list['a63'] ~ bool, list['a63'] <: α64', [α64' - (α58' -> [α64']) ~ ([bool] -> α69') - α69', L: 0 [(α58' -> [α64']) ~ ([bool] -> α69'), (α58' -> [α64']) <: check57', check57' <: ([bool] -> α69')]], α69' <: α68', α68' <: α59', α59' <: α66', [α66' - ([[α66']], [[list['a65']]],) ~ ('a63', list['a63'],) - 'a63', L: 0 [([[α66']], [[list['a65']]],) ~ ('a63', list['a63'],), ([[α66']], [[list['a65']]],) <: ('a63', list['a63'],)]], ['a63' - list['a63'] ~ list['a61'] - 'a61', L: 0 [list['a63'] ~ list['a61'], list['a63'] <: α64', α64' :> list['a61']]], ['a61' - list['a61'] ~ list['a62'] - 'a62', L: 1 [list['a61'] ~ list['a62'], [list['a61'] - ('a61', list['a61'],) ~ ([[α60']], [[list['a62']]],) - list['a62'], L: 0 [('a61', list['a61'],) ~ ([[α60']], [[list['a62']]],), ('a61', list['a61'],) :> ([[α60']], [[list['a62']]],)]]]], ['a62' - list['a62'] ~ list['a61'] - 'a61', L: 1 [list['a62'] ~ list['a61'], [list['a62'] - ([[α60']], [[list['a62']]],) ~ ('a61', list['a61'],) - list['a61'], L: 0 [([[α60']], [[list['a62']]],) ~ ('a61', list['a61'],), ([[α60']], [[list['a62']]],) <: ('a61', list['a61'],)]]]], ['a61' - ('a61', list['a61'],) ~ ([[α60']], [[list['a62']]],) - α60', L: 0 [('a61', list['a61'],) ~ ([[α60']], [[list['a62']]],), ('a61', list['a61'],) :> ([[α60']], [[list['a62']]],)]], [α60' - ([[α60']], [[list['a62']]],) ~ ('a61', list['a61'],) - 'a61', L: 0 [([[α60']], [[list['a62']]],) ~ ('a61', list['a61'],), ([[α60']], [[list['a62']]],) <: ('a61', list['a61'],)]], ['a61' - list['a61'] ~ list['a62'] - 'a62', L: 1 [list['a61'] ~ list['a62'], [list['a61'] - ('a61', list['a61'],) ~ ([[α60']], [[list['a62']]],) - list['a62'], L: 0 [('a61', list['a61'],) ~ ([[α60']], [[list['a62']]],), ('a61', list['a61'],) :> ([[α60']], [[list['a62']]],)]]]], ['a62' - list['a62'] ~ list['a61'] - 'a61', L: 1 [list['a62'] ~ list['a61'], [list['a62'] - ([[α60']], [[list['a62']]],) ~ ('a61', list['a61'],) - list['a61'], L: 0 [([[α60']], [[list['a62']]],) ~ ('a61', list['a61'],), ([[α60']], [[list['a62']]],) <: ('a61', list['a61'],)]]]], ['a61' - list['a61'] ~ list['a63'] - 'a63', L: 0 [list['a61'] ~ list['a63'], list['a61'] <: α64', α64' :> list['a63']]], ['a63' - ('a63', list['a63'],) ~ ([[α66']], [[list['a65']]],) - α66', L: 0 [('a63', list['a63'],) ~ ([[α66']], [[list['a65']]],), ('a63', list['a63'],) :> ([[α66']], [[list['a65']]],)]], α66' :> α59', α59' :> α68', α68' :> bool]
//│ L: 2 [list['a61'] ~ bool, list['a61'] <: α64', [α64' - (α58' -> [α64']) ~ ([bool] -> α69') - α69', L: 0 [(α58' -> [α64']) ~ ([bool] -> α69'), (α58' -> [α64']) <: check57', check57' <: ([bool] -> α69')]], α69' <: α68', α68' <: α59', α59' <: α66', [α66' - ([[α66']], [[list['a65']]],) ~ ('a63', list['a63'],) - 'a63', L: 0 [([[α66']], [[list['a65']]],) ~ ('a63', list['a63'],), ([[α66']], [[list['a65']]],) <: ('a63', list['a63'],)]], ['a63' - list['a63'] ~ list['a61'] - 'a61', L: 0 [list['a63'] ~ list['a61'], list['a63'] <: α64', α64' :> list['a61']]], ['a61' - list['a61'] ~ list['a62'] - 'a62', L: 1 [list['a61'] ~ list['a62'], [list['a61'] - ('a61', list['a61'],) ~ ([[α60']], [[list['a62']]],) - list['a62'], L: 0 [('a61', list['a61'],) ~ ([[α60']], [[list['a62']]],), ('a61', list['a61'],) :> ([[α60']], [[list['a62']]],)]]]], ['a62' - list['a62'] ~ list['a61'] - 'a61', L: 1 [list['a62'] ~ list['a61'], [list['a62'] - ([[α60']], [[list['a62']]],) ~ ('a61', list['a61'],) - list['a61'], L: 0 [([[α60']], [[list['a62']]],) ~ ('a61', list['a61'],), ([[α60']], [[list['a62']]],) <: ('a61', list['a61'],)]]]], ['a61' - ('a61', list['a61'],) ~ ([[α60']], [[list['a62']]],) - α60', L: 0 [('a61', list['a61'],) ~ ([[α60']], [[list['a62']]],), ('a61', list['a61'],) :> ([[α60']], [[list['a62']]],)]], [α60' - ([[α60']], [[list['a62']]],) ~ ('a61', list['a61'],) - 'a61', L: 0 [([[α60']], [[list['a62']]],) ~ ('a61', list['a61'],), ([[α60']], [[list['a62']]],) <: ('a61', list['a61'],)]], ['a61' - list['a61'] ~ list['a62'] - 'a62', L: 1 [list['a61'] ~ list['a62'], [list['a61'] - ('a61', list['a61'],) ~ ([[α60']], [[list['a62']]],) - list['a62'], L: 0 [('a61', list['a61'],) ~ ([[α60']], [[list['a62']]],), ('a61', list['a61'],) :> ([[α60']], [[list['a62']]],)]]]], ['a62' - list['a62'] ~ list['a61'] - 'a61', L: 1 [list['a62'] ~ list['a61'], [list['a62'] - ([[α60']], [[list['a62']]],) ~ ('a61', list['a61'],) - list['a61'], L: 0 [([[α60']], [[list['a62']]],) ~ ('a61', list['a61'],), ([[α60']], [[list['a62']]],) <: ('a61', list['a61'],)]]]], ['a61' - list['a61'] ~ list['a63'] - 'a63', L: 0 [list['a61'] ~ list['a63'], list['a61'] <: α64', α64' :> list['a63']]], ['a63' - ('a63', list['a63'],) ~ ([[α66']], [[list['a65']]],) - α66', L: 0 [('a63', list['a63'],) ~ ([[α66']], [[list['a65']]],), ('a63', list['a63'],) :> ([[α66']], [[list['a65']]],)]], α66' :> α59', α59' :> α68', α68' :> bool]
//│ L: 1 [list['a61'] ~ bool, list['a61'] <: α64', [α64' - (α58' -> [α64']) ~ ([bool] -> α69') - α69', L: 0 [(α58' -> [α64']) ~ ([bool] -> α69'), (α58' -> [α64']) <: check57', check57' <: ([bool] -> α69')]], α69' <: α68', α68' <: α59', α59' <: α66', [α66' - ([[α66']], [[list['a65']]],) ~ ('a63', list['a63'],) - 'a63', L: 0 [([[α66']], [[list['a65']]],) ~ ('a63', list['a63'],), ([[α66']], [[list['a65']]],) <: ('a63', list['a63'],)]], ['a63' - ('a63', list['a63'],) ~ ([[α66']], [[list['a65']]],) - α66', L: 0 [('a63', list['a63'],) ~ ([[α66']], [[list['a65']]],), ('a63', list['a63'],) :> ([[α66']], [[list['a65']]],)]], α66' :> α59', α59' :> α68', α68' :> bool]
//│ L: 3 [list['a63'] ~ bool, list['a63'] <: α64', [α64' - (α58' -> [α64']) ~ ([bool] -> α69') - α69', L: 0 [(α58' -> [α64']) ~ ([bool] -> α69'), (α58' -> [α64']) <: check57', check57' <: ([bool] -> α69')]], α69' <: α68', α68' <: α59', α59' <: α66', [α66' - ([[α66']], [[list['a65']]],) ~ ('a63', list['a63'],) - 'a63', L: 0 [([[α66']], [[list['a65']]],) ~ ('a63', list['a63'],), ([[α66']], [[list['a65']]],) <: ('a63', list['a63'],)]], ['a63' - list['a63'] ~ list['a61'] - 'a61', L: 1 [list['a63'] ~ list['a61'], list['a63'] <: α64', [α64' - (α58' -> [α64']) ~ ([bool] -> α69') - α69', L: 0 [(α58' -> [α64']) ~ ([bool] -> α69'), (α58' -> [α64']) <: check57', check57' <: ([bool] -> α69')]], [α69' - ([bool] -> α69') ~ (α58' -> [α64']) - α64', L: 0 [([bool] -> α69') ~ (α58' -> [α64']), ([bool] -> α69') :> check57', check57' :> (α58' -> [α64'])]], α64' :> list['a61']]], ['a61' - list['a61'] ~ list['a63'] - 'a63', L: 1 [list['a61'] ~ list['a63'], list['a61'] <: α64', [α64' - (α58' -> [α64']) ~ ([bool] -> α69') - α69', L: 0 [(α58' -> [α64']) ~ ([bool] -> α69'), (α58' -> [α64']) <: check57', check57' <: ([bool] -> α69')]], [α69' - ([bool] -> α69') ~ (α58' -> [α64']) - α64', L: 0 [([bool] -> α69') ~ (α58' -> [α64']), ([bool] -> α69') :> check57', check57' :> (α58' -> [α64'])]], α64' :> list['a63']]], ['a63' - list['a63'] ~ list['a61'] - 'a61', L: 2 [list['a63'] ~ list['a61'], list['a63'] <: α64', [α64' - (α58' -> [α64']) ~ ([bool] -> α69') - α69', L: 0 [(α58' -> [α64']) ~ ([bool] -> α69'), (α58' -> [α64']) <: check57', check57' <: ([bool] -> α69')]], α69' <: α68', α68' <: α59', α59' <: α66', [α66' - ([[α66']], [[list['a65']]],) ~ ('a63', list['a63'],) - 'a63', L: 0 [([[α66']], [[list['a65']]],) ~ ('a63', list['a63'],), ([[α66']], [[list['a65']]],) <: ('a63', list['a63'],)]], ['a63' - list['a63'] ~ list['a61'] - 'a61', L: 1 [list['a63'] ~ list['a61'], list['a63'] <: α64', [α64' - (α58' -> [α64']) ~ ([bool] -> α69') - α69', L: 0 [(α58' -> [α64']) ~ ([bool] -> α69'), (α58' -> [α64']) <: check57', check57' <: ([bool] -> α69')]], [α69' - ([bool] -> α69') ~ (α58' -> [α64']) - α64', L: 0 [([bool] -> α69') ~ (α58' -> [α64']), ([bool] -> α69') :> check57', check57' :> (α58' -> [α64'])]], α64' :> list['a61']]], ['a61' - list['a61'] ~ list['a63'] - 'a63', L: 0 [list['a61'] ~ list['a63'], list['a61'] <: α64', α64' :> list['a63']]], ['a63' - ('a63', list['a63'],) ~ ([[α66']], [[list['a65']]],) - α66', L: 0 [('a63', list['a63'],) ~ ([[α66']], [[list['a65']]],), ('a63', list['a63'],) :> ([[α66']], [[list['a65']]],)]], α66' :> α59', α59' :> α68', α68' :> α69', [α69' - ([bool] -> α69') ~ (α58' -> [α64']) - α64', L: 0 [([bool] -> α69') ~ (α58' -> [α64']), ([bool] -> α69') :> check57', check57' :> (α58' -> [α64'])]], α64' :> list['a61']]], ['a61' - list['a61'] ~ list['a63'] - 'a63', L: 0 [list['a61'] ~ list['a63'], list['a61'] <: α64', α64' :> list['a63']]], ['a63' - ('a63', list['a63'],) ~ ([[α66']], [[list['a65']]],) - α66', L: 0 [('a63', list['a63'],) ~ ([[α66']], [[list['a65']]],), ('a63', list['a63'],) :> ([[α66']], [[list['a65']]],)]], α66' :> α59', α59' :> α68', α68' :> bool]
//│ L: 3 [list['a61'] ~ bool, list['a61'] <: α64', [α64' - (α58' -> [α64']) ~ ([bool] -> α69') - α69', L: 0 [(α58' -> [α64']) ~ ([bool] -> α69'), (α58' -> [α64']) <: check57', check57' <: ([bool] -> α69')]], α69' <: α68', α68' <: α59', α59' <: α66', [α66' - ([[α66']], [[list['a65']]],) ~ ('a63', list['a63'],) - 'a63', L: 0 [([[α66']], [[list['a65']]],) ~ ('a63', list['a63'],), ([[α66']], [[list['a65']]],) <: ('a63', list['a63'],)]], ['a63' - list['a63'] ~ list['a61'] - 'a61', L: 1 [list['a63'] ~ list['a61'], list['a63'] <: α64', [α64' - (α58' -> [α64']) ~ ([bool] -> α69') - α69', L: 0 [(α58' -> [α64']) ~ ([bool] -> α69'), (α58' -> [α64']) <: check57', check57' <: ([bool] -> α69')]], [α69' - ([bool] -> α69') ~ (α58' -> [α64']) - α64', L: 0 [([bool] -> α69') ~ (α58' -> [α64']), ([bool] -> α69') :> check57', check57' :> (α58' -> [α64'])]], α64' :> list['a61']]], ['a61' - list['a61'] ~ list['a63'] - 'a63', L: 1 [list['a61'] ~ list['a63'], list['a61'] <: α64', [α64' - (α58' -> [α64']) ~ ([bool] -> α69') - α69', L: 0 [(α58' -> [α64']) ~ ([bool] -> α69'), (α58' -> [α64']) <: check57', check57' <: ([bool] -> α69')]], [α69' - ([bool] -> α69') ~ (α58' -> [α64']) - α64', L: 0 [([bool] -> α69') ~ (α58' -> [α64']), ([bool] -> α69') :> check57', check57' :> (α58' -> [α64'])]], α64' :> list['a63']]], ['a63' - list['a63'] ~ list['a61'] - 'a61', L: 2 [list['a63'] ~ list['a61'], list['a63'] <: α64', [α64' - (α58' -> [α64']) ~ ([bool] -> α69') - α69', L: 0 [(α58' -> [α64']) ~ ([bool] -> α69'), (α58' -> [α64']) <: check57', check57' <: ([bool] -> α69')]], α69' <: α68', α68' <: α59', α59' <: α66', [α66' - ([[α66']], [[list['a65']]],) ~ ('a63', list['a63'],) - 'a63', L: 0 [([[α66']], [[list['a65']]],) ~ ('a63', list['a63'],), ([[α66']], [[list['a65']]],) <: ('a63', list['a63'],)]], ['a63' - list['a63'] ~ list['a61'] - 'a61', L: 1 [list['a63'] ~ list['a61'], list['a63'] <: α64', [α64' - (α58' -> [α64']) ~ ([bool] -> α69') - α69', L: 0 [(α58' -> [α64']) ~ ([bool] -> α69'), (α58' -> [α64']) <: check57', check57' <: ([bool] -> α69')]], [α69' - ([bool] -> α69') ~ (α58' -> [α64']) - α64', L: 0 [([bool] -> α69') ~ (α58' -> [α64']), ([bool] -> α69') :> check57', check57' :> (α58' -> [α64'])]], α64' :> list['a61']]], ['a61' - list['a61'] ~ list['a63'] - 'a63', L: 0 [list['a61'] ~ list['a63'], list['a61'] <: α64', α64' :> list['a63']]], ['a63' - ('a63', list['a63'],) ~ ([[α66']], [[list['a65']]],) - α66', L: 0 [('a63', list['a63'],) ~ ([[α66']], [[list['a65']]],), ('a63', list['a63'],) :> ([[α66']], [[list['a65']]],)]], α66' :> α59', α59' :> α68', α68' :> α69', [α69' - ([bool] -> α69') ~ (α58' -> [α64']) - α64', L: 0 [([bool] -> α69') ~ (α58' -> [α64']), ([bool] -> α69') :> check57', check57' :> (α58' -> [α64'])]], α64' :> list['a61']]], ['a61' - list['a61'] ~ list['a63'] - 'a63', L: 0 [list['a61'] ~ list['a63'], list['a61'] <: α64', α64' :> list['a63']]], ['a63' - ('a63', list['a63'],) ~ ([[α66']], [[list['a65']]],) - α66', L: 0 [('a63', list['a63'],) ~ ([[α66']], [[list['a65']]],), ('a63', list['a63'],) :> ([[α66']], [[list['a65']]],)]], α66' :> α59', α59' :> α68', α68' :> bool]


