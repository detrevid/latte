# Latte

## Overview

The project implements compiler from a simple toy language Instant to JVM and LLVM.

## Language grammar

Grammar of the language can be found here: [Instant.cf](src/Latte.cf). It is written using [the LBNF notation](https://bnfc.readthedocs.org/en/latest/lbnf.html).

## Project structure


.
├── clean_grammar.sh
├── dist
├── latc_llvm
├── latte.cabal
├── latte.iml
├── lib
│   ├── runtime.bc
│   └── runtime.ll
├── Makefile
├── README.md
├── remake_grammar.sh
├── Setup.hs
├── src
│   ├── Latte
│   │   ├── Backend
│   │   │   ├── Compiler.hs
│   │   │   └── Type.hs
│   │   ├── BNFC
│   │   │   ├── AbsLatte.hs
│   │   │   ├── DocLatte.txt
│   │   │   ├── ErrM.hs
│   │   │   ├── LexLatte.x
│   │   │   ├── ParLatte.y
│   │   │   ├── PrintLatte.hs
│   │   │   ├── SkelLatte.hs
│   │   │   └── TestLatte.hs
│   │   ├── Frontend
│   │   │   ├── BasicChecker.hs
│   │   │   └── TypeChecker.hs
│   │   ├── Internal
│   │   │   ├── ASTInternal.hs
│   │   │   ├── BuiltIn.hs
│   │   │   ├── ErrM.hs
│   │   │   └── Type.hs
│   │   ├── Latte.cf
│   │   ├── MainH.hs
│   │   └── Main.hs
│   └── Makefile
└── testsuite
    ├── runtests.sh
    ├── Test.hs
    └── tests



99 directories, 614 files
