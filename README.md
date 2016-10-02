# Latte

## Overview

The project implements LLVM compiler for a java-like toy language Latte.

## Language grammar

Grammar of the language can be found here: [Latte.cf](src/Latte/Latte.cf). It is written using [the LBNF notation](https://bnfc.readthedocs.org/en/latest/lbnf.html).

Program in Latte consists of:

* Definitions of global functions
* Definitions of (globally defined) classes with inheritance and virtual methods

## Requirements

### BNFC

You can find information on [the official site](http://bnfc.digitalgrammars.com/) and [the github page](https://github.com/BNFC/bnfc). The version that has been used during development is 2.8.

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
cabal run <test>
```

Where ```<test>``` is a file containing a program written in Latte language. This command will create llvm bytecode file.

Note: if you build using Makefile you can also use executable ```latc_llm``` created by make command instead of cabal.


## Testing

```
./testsuite/runtests.sh
```

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
