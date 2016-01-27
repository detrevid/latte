# Latte

## Overview

The project implements LLVM compiler for a java-like toy language Latte.

## Language grammar

Grammar of the language can be found here: [Latte.cf](src/Latte.cf). It is written using [the LBNF notation](https://bnfc.readthedocs.org/en/latest/lbnf.html).

Program in Latte consists of:
* Definitions of global functions
* Definitions of (globally defined) classes with inheritance and virtual methods

## Changelog

### 0.1.1.0

* Add classes with inheritance and virtual methods
* Change string equality semantics - from reference equality to equality of values
* Remove usage of alloc in evaluation of boolean expression
* Fix constant folding
* Code refactoring

## Project structure

```
.
├── clean_grammar.sh             -- Script that removes files created with bnfc
├── latte.cabal                  -- Project-description-file for cabal
├── lib
│   ├── runtime.bc
│   └── runtime.ll
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
