/* Machine generated file, do not modify */
#include <stdlib.h>
#include "HsFFI.h"
#include "HsBase.h"

static HugsAPI5 *hugs = 0;

#ifndef ENABLE_MACRO_INTERFACE
#undef execvpe
#endif

static void hugsprim_execvpe_1(HugsStackPtr);
static void hugsprim_execvpe_1(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsPtr arg2;
    HsPtr arg3;
    HsInt32 res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getPtr();
    arg3 = hugs->getPtr();
    res1 = execvpe(arg1, arg2, arg3);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef pPrPr_disableITimers
#endif

static void hugsprim_pPrPr_disableITimers_0(HugsStackPtr);
static void hugsprim_pPrPr_disableITimers_0(HugsStackPtr hugs_root)
{
    pPrPr_disableITimers();
    
    hugs->returnIO(hugs_root,0);
}

static struct hugs_primitive hugs_primTable[] = {
    {"c_execvpe", 4, hugsprim_execvpe_1},
    {"pPrPr_disableITimers", 1, hugsprim_pPrPr_disableITimers_0},
};

static void hugs_primControl(int);
static void hugs_primControl(what)
int what; {
}

#ifdef STATIC_LINKAGE
#define initModule initSPInternals
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

