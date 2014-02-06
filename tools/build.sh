#!/bin/bash

ARCH=armeabi

ndk-build

cp libs/${ARCH}/hugs hugs/bin
cp libs/${ARCH}/runhugs hugs/bin
cp libs/${ARCH}/ffihugs hugs/bin

cp libs/${ARCH}/libBase.so hugs/lib/hugs/packages/base/Data/ByteString/Base.so
cp libs/${ARCH}/libError.so hugs/lib/hugs/packages/base/Foreign/C/Error.so
cp libs/${ARCH}/libAlloc.so hugs/lib/hugs/packages/base/Foreign/Marshal/Alloc.so
cp libs/${ARCH}/libUtils.so hugs/lib/hugs/packages/base/Foreign/Marshal/Utils.so
cp libs/${ARCH}/libPtr.so hugs/lib/hugs/packages/base/Foreign/Ptr.so
cp libs/${ARCH}/libInternals.so hugs/lib/hugs/packages/base/System/Posix/Internals.so
cp libs/${ARCH}/libSignals.so hugs/lib/hugs/packages/base/System/Posix/Signals.so
cp libs/${ARCH}/libpInternals.so hugs/lib/hugs/packages/base/System/Process/Internals.so
cp libs/${ARCH}/libStorable.so hugs/lib/hugs/packages/hugsbase/Hugs/Storable.so

echo BASEDIR=/data/data/jackpal.androidterm/app_HOME > inst.sh
echo DLDIR=/sdcard/Download >> inst.sh


for i in `find hugs -type d`; do echo mkdir \$\{BASEDIR\}/$i >> inst.sh; done
for i in `find hugs -type f`; do echo cat \$\{DLDIR\}/$i \> \$\{BASEDIR\}/$i >> inst.sh; done

echo chmod 0755 \$\{BASEDIR\}/hugs/bin/hugs >> inst.sh
echo chmod 0755 \$\{BASEDIR\}/hugs/bin/runhugs >> inst.sh
echo chmod 0755 \$\{BASEDIR\}/hugs/bin/ffihugs >> inst.sh

zip -q -r hugs.zip hugs
