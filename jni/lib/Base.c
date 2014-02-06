/* Machine generated file, do not modify */
#include <stdlib.h>
#include "HsFFI.h"
#include "HsBase.h"

static HugsAPI5 *hugs = 0;
#include "fpstring.h"

#ifndef ENABLE_MACRO_INTERFACE
#undef fps_count
#endif

static void hugsprim_fps_count_13(HugsStackPtr);
static void hugsprim_fps_count_13(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsWord64 arg2;
    HsWord8 arg3;
    HsWord64 res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getWord64();
    arg3 = hugs->getWord8();
    res1 = fps_count(arg1, arg2, arg3);
    hugs->putWord64(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef fps_minimum
#endif

static void hugsprim_fps_minimum_12(HugsStackPtr);
static void hugsprim_fps_minimum_12(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsWord64 arg2;
    HsWord8 res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getWord64();
    res1 = fps_minimum(arg1, arg2);
    hugs->putWord8(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef fps_maximum
#endif

static void hugsprim_fps_maximum_11(HugsStackPtr);
static void hugsprim_fps_maximum_11(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsWord64 arg2;
    HsWord8 res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getWord64();
    res1 = fps_maximum(arg1, arg2);
    hugs->putWord8(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef fps_intersperse
#endif

static void hugsprim_fps_intersperse_10(HugsStackPtr);
static void hugsprim_fps_intersperse_10(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsPtr arg2;
    HsWord64 arg3;
    HsWord8 arg4;
    arg1 = hugs->getPtr();
    arg2 = hugs->getPtr();
    arg3 = hugs->getWord64();
    arg4 = hugs->getWord8();
    fps_intersperse(arg1, arg2, arg3, arg4);
    
    hugs->returnIO(hugs_root,0);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef fps_reverse
#endif

static void hugsprim_fps_reverse_9(HugsStackPtr);
static void hugsprim_fps_reverse_9(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsPtr arg2;
    HsWord64 arg3;
    arg1 = hugs->getPtr();
    arg2 = hugs->getPtr();
    arg3 = hugs->getWord64();
    fps_reverse(arg1, arg2, arg3);
    
    hugs->returnIO(hugs_root,0);
}
#include "string.h"

#ifndef ENABLE_MACRO_INTERFACE
#undef memset
#endif

static void hugsprim_memset_8(HugsStackPtr);
static void hugsprim_memset_8(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsWord8 arg2;
    HsWord64 arg3;
    HsPtr res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getWord8();
    arg3 = hugs->getWord64();
    res1 = memset(arg1, arg2, arg3);
    hugs->putPtr(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef memmove
#endif

static void hugsprim_memmove_7(HugsStackPtr);
static void hugsprim_memmove_7(HugsStackPtr hugs_root)
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

static void hugsprim_memcpy_6(HugsStackPtr);
static void hugsprim_memcpy_6(HugsStackPtr hugs_root)
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

#ifndef ENABLE_MACRO_INTERFACE
#undef memcmp
#endif

static void hugsprim_memcmp_5(HugsStackPtr);
static void hugsprim_memcmp_5(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsPtr arg2;
    HsWord64 arg3;
    HsInt32 res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getPtr();
    arg3 = hugs->getWord64();
    res1 = memcmp(arg1, arg2, arg3);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef memchr
#endif

static void hugsprim_memchr_4(HugsStackPtr);
static void hugsprim_memchr_4(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsWord8 arg2;
    HsWord64 arg3;
    HsPtr res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getWord8();
    arg3 = hugs->getWord64();
    res1 = memchr(arg1, arg2, arg3);
    hugs->putPtr(res1);
    hugs->returnIO(hugs_root,1);
}
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
#undef malloc
#endif

static void hugsprim_malloc_1(HugsStackPtr);
static void hugsprim_malloc_1(HugsStackPtr hugs_root)
{
    HsWord64 arg1;
    HsPtr res1;
    arg1 = hugs->getWord64();
    res1 = malloc(arg1);
    hugs->putPtr(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef strlen
#endif

static void hugsprim_strlen_0(HugsStackPtr);
static void hugsprim_strlen_0(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsWord64 res1;
    arg1 = hugs->getPtr();
    res1 = strlen(arg1);
    hugs->putWord64(res1);
    hugs->returnIO(hugs_root,1);
}

static struct hugs_primitive hugs_primTable[] = {
    {"c_count", 4, hugsprim_fps_count_13},
    {"c_minimum", 3, hugsprim_fps_minimum_12},
    {"c_maximum", 3, hugsprim_fps_maximum_11},
    {"c_intersperse", 5, hugsprim_fps_intersperse_10},
    {"c_reverse", 4, hugsprim_fps_reverse_9},
    {"memset", 4, hugsprim_memset_8},
    {"memmove", 4, hugsprim_memmove_7},
    {"memcpy", 4, hugsprim_memcpy_6},
    {"memcmp", 4, hugsprim_memcmp_5},
    {"memchr", 4, hugsprim_memchr_4},
    {"c_free_finalizer", 0, hugsprim_free_3},
    {"c_free", 2, hugsprim_free_2},
    {"c_malloc", 2, hugsprim_malloc_1},
    {"c_strlen", 2, hugsprim_strlen_0},
};

static void hugs_primControl(int);
static void hugs_primControl(what)
int what; {
}

#ifdef STATIC_LINKAGE
#define initModule initDBBase
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

