#!/usr/bin/env bash

make

testsDirs=(
  ./testsuite/tests/lattests/good/
  ./testsuite/tests/mrjp-tests/good/basic/
  ./testsuite/tests/testy-latte/basic/msz/good/
  ./testsuite/tests/testy-latte/basic/sygi/good/
  ./testsuite/tests/mytests/good/
  ./testsuite/tests/lattests/extensions/struct/
  ./testsuite/tests/lattests/extensions/objects1/
  ./testsuite/tests/lattests/extensions/objects2/
  ./testsuite/tests/testy-latte/objects1/sygi/good/
  ./testsuite/tests/testy-latte/objects2/sygi/good/
  )

for testsDir in "${testsDirs[@]}"; do
    for test in ${testsDir}*.lat; do
        fullTestName="${test::-4}"
        testName=$(basename "$fullTestName")
        testOut="$fullTestName.out"
        testInput="$fullTestName.input"
        testEval="$fullTestName.output"
        #LLVM
        testBC="$fullTestName.bc"
        testLL="$fullTestName.ll"
        ./latc_llvm ${test}
        if [ -f ${testEval} ];
        then
            if [ -f ${testInput} ];
            then
                lli ${testBC} < ${testInput} > ${testOut}
            else
                lli ${testBC} > ${testOut}
            fi
            if ! cmp -s ${testOut} ${testEval};
            then
                diff ${testOut} ${testEval}
                exit 1
            fi
        else
            if [ -f ${testInput} ];
            then
                lli ${testBC} < ${testInput}
            else
                lli ${testBC}
            fi
        fi
        rm -rf ${testOut} ${testBC} ${testLL}
        echo "ENDTEST: $fullTestName"
    done
done