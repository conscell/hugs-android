LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)

LOCAL_MODULE    := readline
LOCAL_SRC_FILES := editline/editline.c \
editline/sysunix.c

LOCAL_CFLAGS := -DSYS_UNIX -DUSE_DIRENT -DHAVE_STDLIB -DHAVE_TERMIO -DANSI_ARROWS

include $(BUILD_STATIC_LIBRARY)

include $(CLEAR_VARS)

LOCAL_MODULE    := hugs
LOCAL_SRC_FILES := hugs.c \
edit.c \
observe.c \
builtin.c \
char.c \
compiler.c \
errors.c \
evaluator.c \
ffi.c \
goal.c \
input.c \
machdep.c \
machine.c \
module.c \
opts.c \
output.c \
plugin.c \
script.c \
static.c \
storage.c \
strutil.c \
subst.c \
type.c \
version.c

LOCAL_STATIC_LIBRARIES := readline

include $(BUILD_EXECUTABLE)

include $(CLEAR_VARS)

LOCAL_MODULE    := runhugs
LOCAL_SRC_FILES := runhugs.c \
server.c \
edit.c \
builtin.c \
char.c \
compiler.c \
errors.c \
evaluator.c \
ffi.c \
goal.c \
input.c \
machdep.c \
machine.c \
module.c \
opts.c \
output.c \
plugin.c \
script.c \
static.c \
storage.c \
strutil.c \
subst.c \
type.c \
version.c

LOCAL_STATIC_LIBRARIES := readline

include $(BUILD_EXECUTABLE)

include $(CLEAR_VARS)

LOCAL_MODULE    := ffihugs
LOCAL_SRC_FILES := runhugs.c \
server.c \
edit.c \
builtin.c \
char.c \
compiler.c \
errors.c \
evaluator.c \
ffi.c \
goal.c \
input.c \
machdep.c \
machine.c \
module.c \
opts.c \
output.c \
plugin.c \
script.c \
static.c \
storage.c \
strutil.c \
subst.c \
type.c \
version.c

LOCAL_STATIC_LIBRARIES := readline

LOCAL_CFLAGS := -DFFI_COMPILER

include $(BUILD_EXECUTABLE)
