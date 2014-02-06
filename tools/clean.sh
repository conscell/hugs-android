#!/bin/bash

rm -f hugs/bin/hugs hugs/bin/runhugs hugs/bin/ffihugs hugs.zip inst.sh
rm hugs/lib/hugs/packages/base/Data/ByteString/Base.so
rm hugs/lib/hugs/packages/base/Foreign/C/Error.so
rm hugs/lib/hugs/packages/base/Foreign/Marshal/Alloc.so
rm hugs/lib/hugs/packages/base/Foreign/Marshal/Utils.so
rm hugs/lib/hugs/packages/base/Foreign/Ptr.so
rm hugs/lib/hugs/packages/base/System/Posix/Internals.so
rm hugs/lib/hugs/packages/base/System/Posix/Signals.so
rm hugs/lib/hugs/packages/base/System/Process/Internals.so
rm hugs/lib/hugs/packages/hugsbase/Hugs/Storable.so
rm -rf libs obj
