/* Machine generated file, do not modify */
#include <stdlib.h>
#include "HsFFI.h"
#include "HsBase.h"

static HugsAPI5 *hugs = 0;
#include "stdlib.h"

#ifndef ENABLE_MACRO_INTERFACE
#undef free
#endif

static void hugsprim_free_3(HugsStackPtr);
static void hugsprim_free_3(HugsStackPtr hugs_root)
{
    hugs->putFunPtr((HsFunPtr)&free);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef free
#endif

static void hugsprim_free_2(HugsStackPtr);
static void hugsprim_free_2(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    arg1 = hugs->getPtr();
    free(arg1);
    
    hugs->returnIO(hugs_root,0);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef realloc
#endif

static void hugsprim_realloc_1(HugsStackPtr);
static void hugsprim_realloc_1(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsWord64 arg2;
    HsPtr res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getWord64();
    res1 = realloc(arg1, arg2);
    hugs->putPtr(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef malloc
#endif

static void hugsprim_malloc_0(HugsStackPtr);
static void hugsprim_malloc_0(HugsStackPtr hugs_root)
{
    HsWord64 arg1;
    HsPtr res1;
    arg1 = hugs->getWord64();
    res1 = malloc(arg1);
    hugs->putPtr(res1);
    hugs->returnIO(hugs_root,1);
}

static struct hugs_primitive hugs_primTable[] = {
    {"finalizerFree", 0, hugsprim_free_3},
    {"_free", 2, hugsprim_free_2},
    {"_realloc", 3, hugsprim_realloc_1},
    {"_malloc", 2, hugsprim_malloc_0},
};

static void hugs_primControl(int);
static void hugs_primControl(what)
int what; {
}

#ifdef STATIC_LINKAGE
#define initModule initFMAlloc
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

