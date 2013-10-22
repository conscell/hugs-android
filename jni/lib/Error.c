/* Machine generated file, do not modify */
#include <stdlib.h>
#include "HsFFI.h"
#include "HsBase.h"

static HugsAPI5 *hugs = 0;
#include "string.h"

#ifndef ENABLE_MACRO_INTERFACE
#undef strerror
#endif

static void hugsprim_strerror_2(HugsStackPtr);
static void hugsprim_strerror_2(HugsStackPtr hugs_root)
{
    HsInt32 arg1;
    HsPtr res1;
    arg1 = hugs->getInt32();
    res1 = strerror(arg1);
    hugs->putPtr(res1);
    hugs->returnIO(hugs_root,1);
}
#include "HsBase.h"

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_set_errno
#endif

static void hugsprim___hscore_set_errno_1(HugsStackPtr);
static void hugsprim___hscore_set_errno_1(HugsStackPtr hugs_root)
{
    HsInt32 arg1;
    arg1 = hugs->getInt32();
    __hscore_set_errno(arg1);
    
    hugs->returnIO(hugs_root,0);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_get_errno
#endif

static void hugsprim___hscore_get_errno_0(HugsStackPtr);
static void hugsprim___hscore_get_errno_0(HugsStackPtr hugs_root)
{
    HsInt32 res1;
    res1 = __hscore_get_errno();
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

static struct hugs_primitive hugs_primTable[] = {
    {"strerror", 2, hugsprim_strerror_2},
    {"set_errno", 2, hugsprim___hscore_set_errno_1},
    {"get_errno", 1, hugsprim___hscore_get_errno_0},
};

static void hugs_primControl(int);
static void hugs_primControl(what)
int what; {
}

#ifdef STATIC_LINKAGE
#define initModule initFCError
#endif

static struct hugs_primInfo hugs_prims = { hugs_primControl, hugs_primTable, 0 };

#ifdef __cplusplus
extern "C" {
#endif
#ifndef __cplusplus
DLLEXPORT(int)  HugsAPIVersion(void);
#endif
DLLEXPORT(int)  HugsAPIVersion() {return (5);}
DLLEXPORT(void) initModule(HugsAPI5 *);
DLLEXPORT(void) initModule(HugsAPI5 *hugsAPI) {
    hugs = hugsAPI;
    hugs->registerPrims(&hugs_prims);
}
#ifdef __cplusplus
}
#endif

