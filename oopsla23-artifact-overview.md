# Getting Into The Flow: Better Type Error Messages for Constraint-Based Type Inference

Title of the submitted paper: Getting Into The Flow: Better Type Error Messages for Constraint-Based Type Inference

OOPSLA submission number for the paper: #183

## Overview

### Artifact type and format

Scala source code for the HM<sup>ℓ</sup> (HMloc) with many examples.

### File organization

This artifact consists of an SBT (Scala Build Tool) project with an implementation of
a modified version of SimpleSub type system. We call it  HM<sup>ℓ</sup> (HMloc) and is
introduced in the corresponding paper. In addition, we provide a test suite which
includes examples and a web demo that gives live typing and running results
of the user input source.

- The `shared/src/main/scala/hmloc` directory contains the sources of the hmloc compiler.
- The `shared/src/test/scala/hmloc` directory contains the implementation of the testing infrastructure.
- The `shared/src/test/diff` directory contains hmloc tests.
- For more documentation of the compiler codebase, please refer to `hmloc-codebase-doc.md`.

### Claims to artifact evaluation badges

We claim all three badges: functional, reusable, and available.

#### Functional

The HMloc type system types a functional programming language with algebraic
data types, tuples, functions. It also supports let polymorphism. This language
uses a subset of OCaml syntax.

This artifact demonstrates the error reporting technique we've implemented in
HMloc. Type errors for programs show detailed data flow information. The artifact
includes a test suite and many example programs and their reported errors.

The `shared/src/test/diff/ocaml` directory contains tests. Some notable ones are:
  - `Survey*.mls` are the example programs used in user survey described in the paper
  - `OcamlPresentation.mls` - demonstrates a variety of errors
  - `LetPoly.mls` - shows errors in code that uses let polymorphism
  - `Realistic.mls` - shows examples that might occur in an actual codebase
  - `OcamlExprParser.mls` shows the subset of OCaml syntax supported by the parser

You can edit and existing file or create a new file in the same directory to test
your own examples.

#### Reusable

In file `hmloc-codebase-doc.md`,
we explain how the compiler codebase is organized and introduce the general ideas
about the implementation of the type system, which helps the reuse of the codebase for
future extension of features.

Of particular interest is the discussion around provenances used in typing and how
the types are unified. We hope this could provide inspiration for reusing the
artifact to reuse or extend the type system.

#### Available

We agree to publish our artifact under a Creative Commons license.
Note that this hmloc type system will be open source.
We will include the link in the final version of the manuscript.

## Artifact Requirements

Any system with Docker available can access, compile, and test our artifact.
We have shared the instructions for building and running our artifact in a
docker image.

To test our artifact from scratch, one needs to install
a recent Java Virtual Machine (JVM), the Scala Build Tool (SBT)

## Getting Started

### Using the Docker Image

To build the docker image, one should first install Docker if it is not installed yet. [link](https://docs.docker.com/engine/install/)
Then, one can build and launch the artifact with the following command:

```
docker build --tag 'hmloc-oopsla23' .
docker run -it --rm 'hmloc-oopsla23'
```

The user will be attached to the shell of the container after the image gets pulled and the container is launched.
Please `cd` to `hmloc/` and launch the SBT shell by typing `sbt`.

### Setting up from Scratch

1. Follow the Coursier installation instructions at https://get-coursier.io/docs/cli-installation.

2. Change your working directory to the root of this repository and
   launch the SBT shell by typing `sbt` in the terminal.

## Experimenting with HMloc

We provide two ways of experimenting with HMloc. A test suite that runs example files and a web demo where the user can type their programs and see the results live.

We recommend using the the test suite.

### Using the test suite

To compile the code to JVM and re-run the tests upon file changes,
launch SBT and then use the SBT command `~hmlocJVM/testOnly hmloc.DiffTests`.

The test output is inserted as comments beginning with `//│` in the test file,
immediately after each corresponding code block.

If there are any unstaged changes (as determined by `git`),
only the test files with those changes will be run (those in `shared/src/test/diff`). You may modify test files to run your own examples.

We recommend using an editor that automatically reloads open files on changes so that the test results for each code block can be easily seen.

### Web demo

To run the web demo, compile the project with `hmlocJS/fastOptJS`, 
then open the `local_testing.html` file in a browser.

### Syntax reference

| **Feature** | **Type syntax** | **Term syntax** |
| -- | -- | -- |
| **Basic terms and types** | | |
| literals | true, false, 0, 1, "", etc | same |
| function | int -> int | fun x -> x + 1 |
| tuples | (int, int,) | (1, 2,) |
| application | F[A] | f a |
| variable | 'a | x |
| let binding | | let x = s in t |
| pattern matching | | `match t with C1(a, b, c) -> a \| C2(a, b) -> b` |
| **Top level declarations** | | |
| definition | val foo: T | def foo = t |
| algebraic data type | type 'a list = Cons('a, 'a list) | Nil | Cons(1, Nil) |
| type alias | type Foo[T] = List[T] | |
