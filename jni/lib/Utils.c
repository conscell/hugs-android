/* Machine generated file, do not modify */
#include <stdlib.h>
#include "HsFFI.h"
#include "HsBase.h"

static HugsAPI5 *hugs = 0;
#include "string.h"

#ifndef ENABLE_MACRO_INTERFACE
#undef memmove
#endif

static void hugsprim_memmove_1(HugsStackPtr);
static void hugsprim_memmove_1(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsPtr arg2;
    HsWord64 arg3;
    arg1 = hugs->getPtr();
    arg2 = hugs->getPtr();
    arg3 = hugs->getWord64();
    memmove(arg1, arg2, arg3);
    
    hugs->returnIO(hugs_root,0);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef memcpy
#endif

static void hugsprim_memcpy_0(HugsStackPtr);
static void hugsprim_memcpy_0(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsPtr arg2;
    HsWord64 arg3;
    arg1 = hugs->getPtr();
    arg2 = hugs->getPtr();
    arg3 = hugs->getWord64();
    memcpy(arg1, arg2, arg3);
    
    hugs->returnIO(hugs_root,0);
}

static struct hugs_primitive hugs_primTable[] = {
    {"memmove", 4, hugsprim_memmove_1},
    {"memcpy", 4, hugsprim_memcpy_0},
};

static void hugs_primControl(int);
static void hugs_primControl(what)
int what; {
}

#ifdef STATIC_LINKAGE
#define initModule initFMUtils
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

