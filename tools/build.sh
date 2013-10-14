#!/bin/bash

ARCH=armeabi

ndk-build

cp libs/${ARCH}/hugs hugs/bin
cp libs/${ARCH}/runhugs hugs/bin
cp libs/${ARCH}/ffihugs hugs/bin

echo BASEDIR=/data/data/jackpal.androidterm/app_HOME > inst.sh
echo DLDIR=/sdcard/Download >> inst.sh


for i in `find hugs -type d`; do echo mkdir \$\{BASEDIR\}/$i >> inst.sh; done
for i in `find hugs -type f`; do echo cat \$\{DLDIR\}/$i \> \$\{BASEDIR\}/$i >> inst.sh; done

echo chmod 0755 \$\{BASEDIR\}/hugs/bin/hugs >> inst.sh
echo chmod 0755 \$\{BASEDIR\}/hugs/bin/runhugs >> inst.sh
echo chmod 0755 \$\{BASEDIR\}/hugs/bin/ffihugs >> inst.sh

zip -q -r hugs.zip hugs
