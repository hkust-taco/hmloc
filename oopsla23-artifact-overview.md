# Getting Into The Flow: Better Type Error Messages for Constraint-Based Type Inference

Title of the submitted paper: Getting Into The Flow: Better Type Error Messages for Constraint-Based Type Inference

OOPSLA submission number for the paper: #183

## Overview

### Artifact type and format

Scala source code for the HM<sup>ℓ</sup> with many examples.

### File organization

This artifact consists of an SBT (Scala Build Tool) project with an implementation of
the SimpleSub modified to work as an HM<sup>ℓ</sup> type system introduced in the
corresponding paper. In addition, we provide a test suite which
includes examples and a web demo that gives live typing and running results
of the user input source.

- The `shared/src/main/scala/mlscript` directory contains the sources of the HM<sup>ℓ</sup> compiler.
  - For more documentation of the compiler codebase, please refer to `hml-codebase-doc.md`.
- The `shared/src/test/scala/mlscript` directory contains the implementation of
  the testing infrastructure.
- The `shared/src/test/diff` directory contains MLscript tests.

### Claims to artifact evaluation badges

We claim all three badges: functional, reusable, and available.

#### Functional

Examples in the paper work as they are shown, which have
desired typing and running results produced by our implementation.
Important examples can be found in the differential test suite.

The `shared/src/test/diff/ocaml` directory contains tests. Some notable ones are:
  - `Survey*.mls` are the example programs used in user survey described in the paper
  - `OcamlExprParser.mls` shows the subset of OCaml syntax supported by the parser
  - `LetPoly.mls` - shows let polymorphism
  - `Scratch.mls` a blank file you can use to test out your own examples

#### Reusable

In file `mls-codebase-doc.md`,
we explain how the compiler codebase is organized and introduce the general ideas
about the implementation of the type system, which helps the reuse of the codebase for
future extension of features.

Of particular interest is the discussion around provenances used in typing and how
the types are unified. We hope this could provide inspiration for reusing the
artifact to reuse or extend the type system.

#### Available

We agree to publish our artifact under a Creative Commons license.
Note that this HM<sup>ℓ</sup> type system will be open source.
We will provide the link after the double-blind review process.

## Artifact Requirements

Any system with Docker available can access, compile, and test our artifact.
We prepared a Docker image that contains all dependencies
to compile and run our artifact.

To test our artifact from scratch, one needs to install
a recent Java Virtual Machine (JVM), the Scala Build Tool (SBT)

## Getting Started

### Using the Docker Image TODO

We have built a Docker image containing all necessary prerequisites
and pushed it to [Docker Hub](https://hub.docker.com/r/superoop/superoop-docker).

To use this image, one should first install Docker if it is not installed yet.
Then, one can launch a container using this image with the following command:

```
docker build --tag 'hml-oopsla23'
docker run -it --rm 'hml-oopsla23'
```

The user will be attached to the shell of the container after the image gets pulled and the container is launched.
Please `cd` to `mlscript/` and launch the SBT shell by typing `sbt`.

### Setting up from Scratch

1. Follow the Coursier installation instructions at https://get-coursier.io/docs/cli-installation.

2. Change your working directory to the root of this repository and
   launch the SBT shell by typing `sbt` in the terminal.

## Experimenting with MLscript

We provide two ways of experimenting with MLscript.

### Web demo

To run the web demo, compile the project with `mlscriptJS/fastOptJS`, 
then open the `local_testing.html` file in a browser.

### Using the test suite

To compile the code to JVM and re-run the tests upon file changes,
launch SBT and then use the SBT command `~mlscriptJVM/testOnly mlscript.DiffTests`.

The test output is inserted as comments beginning with `//│` in the test file,
immediately after each corresponding code block.
We recommend using an editor that automatically reloads open files on changes
so that the test results for each code block can be easily seen.

If there are any unstaged changes (as determined by `git`),
only the corresponding files will be tested in any test file (those in shared/src/test/diff).
One may make modifications to some test files and rerun the test command,
and it will only run the modified tests.
