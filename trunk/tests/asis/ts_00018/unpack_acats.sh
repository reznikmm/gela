#!/bin/bash
# Run this script in parent of trunc directory to prepare ACATS

[ -f ACATS30.ZIP ] || wget http://www.ada-auth.org/acats-files/3.0/ACATS30.ZIP

ARCHIVE=ACATS30.ZIP
UNPACK_DIR=acats
TARGET=$UNPACK_DIR/include

[ -d $UNPACK_DIR ] && rm -rf $UNPACK_DIR

mkdir -p $UNPACK_DIR

[ -d $TARGET ] && rm -rf $TARGET
mkdir -p $TARGET

TARGET=`cd $TARGET;pwd`

unzip -a -q $ARCHIVE -d $UNPACK_DIR

pushd $UNPACK_DIR/SUPPORT
find .. -name '*.TST' > TSTTESTS.DAT
gnatchop MACROSUB.ADA
gnatmake macrosub
./macrosub
gnatchop -gnat05 *.ADA *.A *.ADT $TARGET
popd

