#!/bin/sh
set -e
T=gela-asis-0.3.3
svn export svn://forge.ada-ru.org/gela/trunk /tmp/$T

cd /tmp/$T
rm source/gela.gpr
rm -rf tests
rm -rf tools/tests
make generate
rm -rf .build/{asis,ayacc,gela_lib,parser,uaflex,xml2ayacc}/*
rm output.txt dist.sh

cd ..
tar cjvf $T.tar.bz2 $T
