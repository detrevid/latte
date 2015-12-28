#!/usr/bin/env bash
./clean_grammar.sh
cd src
bnfc -m --haskell -p Latte.BNFC Latte/Latte.cf
make
cd ..
