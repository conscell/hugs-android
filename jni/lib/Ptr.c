/* Machine generated file, do not modify */
#include <stdlib.h>
#include "HsFFI.h"
#include "HsBase.h"

static HugsAPI5 *hugs = 0;

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_from_intptr
#endif

static void hugsprim___hscore_from_intptr_3(HugsStackPtr);
static void hugsprim___hscore_from_intptr_3(HugsStackPtr hugs_root)
{
    HsInt64 arg1;
    HsPtr res1;
    arg1 = hugs->getInt64();
    res1 = __hscore_from_intptr(arg1);
    hugs->putPtr(res1);
    hugs->returnId(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_to_intptr
#endif

static void hugsprim___hscore_to_intptr_2(HugsStackPtr);
static void hugsprim___hscore_to_intptr_2(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt64 res1;
    arg1 = hugs->getPtr();
    res1 = __hscore_to_intptr(arg1);
    hugs->putInt64(res1);
    hugs->returnId(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_from_uintptr
#endif

static void hugsprim___hscore_from_uintptr_1(HugsStackPtr);
static void hugsprim___hscore_from_uintptr_1(HugsStackPtr hugs_root)
{
    HsWord64 arg1;
    HsPtr res1;
    arg1 = hugs->getWord64();
    res1 = __hscore_from_uintptr(arg1);
    hugs->putPtr(res1);
    hugs->returnId(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef __hscore_to_uintptr
#endif

static void hugsprim___hscore_to_uintptr_0(HugsStackPtr);
static void hugsprim___hscore_to_uintptr_0(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsWord64 res1;
    arg1 = hugs->getPtr();
    res1 = __hscore_to_uintptr(arg1);
    hugs->putWord64(res1);
    hugs->returnId(hugs_root,1);
}

static struct hugs_primitive hugs_primTable[] = {
    {"intPtrToPtr", 1, hugsprim___hscore_from_intptr_3},
    {"ptrToIntPtr", 1, hugsprim___hscore_to_intptr_2},
    {"wordPtrToPtr", 1, hugsprim___hscore_from_uintptr_1},
    {"ptrToWordPtr", 1, hugsprim___hscore_to_uintptr_0},
};

static void hugs_primControl(int);
static void hugs_primControl(what)
int what; {
}

#ifdef STATIC_LINKAGE
#define initModule initFPtr
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

