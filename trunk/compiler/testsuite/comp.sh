#!/bin/bash
set -e

array=( $@ )
len=${#array[@]}
expected=${array[$len-1]}
args=${array[@]:0:$len-1}
file=${array[$len-2]}

extension="${file##*.}"
basename="${file%.*}"
testsuite=`dirname $0`
build=${GELA_BUILD:-/tmp}

$build/gela-compiler $args > $build/${basename}.ll

clang -S -emit-llvm -DTEST=${basename} \
    -o $build/${basename}_main.ll $testsuite/support/main.c

llvm-link -o $build/${basename}.exe \
    $build/${basename}_main.ll \
    $build/${basename}.ll \
    $build/test_support.ll

lli $build/${basename}.exe > $build/${basename}.out
result=`sum $build/${basename}.out | cut -d\  -f1`

if [ "$result" != "$expected" ]; then
    cat $build/${basename}.out
    echo sum: $result expected: $expected
    exit 1
fi
