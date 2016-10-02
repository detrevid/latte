# Latte

## Overview

The project implements LLVM compiler for a java-like toy language Latte.

## Language grammar

Grammar of the language can be found here: [Latte.cf](src/Latte/Latte.cf).
It is written using [the LBNF notation](https://bnfc.readthedocs.org/en/latest/lbnf.html).

Program in Latte consists of:

- definitions of global functions
- definitions of (globally defined) classes with inheritance and virtual methods

## Requirements

### BNFC

You can find information on [the official site](http://bnfc.digitalgrammars.com/) and
[the github page](https://github.com/BNFC/bnfc).
The version that has been used during development is 2.8.

### LLVM

The version that has been used during development is 3.6.2.

## Building

### 1. Run the bash script

```
./remake_grammar.sh
```

The script creates necessary files for later compilation. The script assumes that BNFC is visible in the environment.

### 2. Further steps

#### 2.1. Using Makefile

```
make
```

#### 2.2. Using Cabal

##### 2.2.1. Compile runtime.ll

```
llvm-as -o lib/runtime.bc lib/runtime.ll
```

##### 2.2.2. Run Cabal

```
cabal install
```

## Usage

```
cabal run <latte_program>
```

Where *<latte_program>* is a file containing a program written in Latte language.
This command will create llvm bytecode file.

Note: if you have used Makefile to build the project then
you can also use executable *latc_llm* created by *make* command instead of cabal.


## Testing

```
./testsuite/runtests.sh
```

All tests can be found under [tests](testsuite/tests/). Tests consists of:

- *.lat* - program written in Latte
- *.input* - input data for a program
- *.output* - output that should be returned by a program

Note that projected has not implemented all functionalities that are covered by tests.
To see what tests are currently covered you can see variable *testsDirs* in [runtests.sh](testsuite/runtests.sh) script.

Tests that can be found under [testsuite/tests/mrjp-tests](testsuite/tests/mrjp-tests/) and
[testsuite/tests/testy-latte](testsuite/tests/testy-latte/)
are modified versions of projects: [mrjp-tests](https://github.com/tomwys/mrjp-tests) and
[testy-latte](https://github.com/sygi/testy-latte) respectively.

## Changelog

### 0.1.1.0

- add classes with inheritance and virtual methods
- change string equality semantics - from reference equality to equality of values
- remove usage of alloc in evaluation of boolean expression
- fix constant folding
- code refactoring

## Project structure

```
.
├── clean_grammar.sh             -- Script that removes files created with bnfc
├── latte.cabal                  -- Project-description-file for cabal
├── lib
│   └── runtime.ll               -- File contains declarations and definitions of Latte built-in functions
├── Makefile
├── README.md
├── remake_grammar.sh            -- Script that runs bnfc
├── Setup.hs
├── src
│   ├── Latte
│   │   ├── Backend
│   │   │   └── Compiler.hs
│   │   ├── BNFC
│   │   ├── Frontend
│   │   │   ├── BasicChecker.hs
│   │   │   ├── Optimisations.hs
│   │   │   ├── Precompiler.hs
│   │   │   └── TypeChecker.hs
│   │   ├── Internal
│   │   │   ├── ASTInternal.hs
│   │   │   ├── BuiltIn.hs
│   │   │   └── Type.hs
│   │   ├── Latte.cf             -- Grammar declaration
│   │   ├── MainH.hs
│   │   └── Main.hs
│   └── Makefile
└── testsuite
    ├── runtests.sh
    ├── Test.hs
    └── tests
```
