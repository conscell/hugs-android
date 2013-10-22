/* Machine generated file, do not modify */
#include <stdlib.h>
#include "HsFFI.h"
#include "HsBase.h"

extern int killpg(int pgrp, int sig);

static HugsAPI5 *hugs = 0;

#ifndef ENABLE_MACRO_INTERFACE
#undef sigpending
#endif

static void hugsprim_sigpending_8(HugsStackPtr);
static void hugsprim_sigpending_8(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt32 res1;
    arg1 = hugs->getPtr();
    res1 = sigpending(arg1);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef sigismember
#endif

static void hugsprim_sigismember_7(HugsStackPtr);
static void hugsprim_sigismember_7(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt32 arg2;
    HsInt32 res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getInt32();
    res1 = sigismember(arg1, arg2);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef sigfillset
#endif

static void hugsprim_sigfillset_6(HugsStackPtr);
static void hugsprim_sigfillset_6(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt32 res1;
    arg1 = hugs->getPtr();
    res1 = sigfillset(arg1);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef sigdelset
#endif

static void hugsprim_sigdelset_5(HugsStackPtr);
static void hugsprim_sigdelset_5(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt32 arg2;
    HsInt32 res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getInt32();
    res1 = sigdelset(arg1, arg2);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef sigsuspend
#endif

static void hugsprim_sigsuspend_4(HugsStackPtr);
static void hugsprim_sigsuspend_4(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt32 res1;
    arg1 = hugs->getPtr();
    res1 = sigsuspend(arg1);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef alarm
#endif

static void hugsprim_alarm_3(HugsStackPtr);
static void hugsprim_alarm_3(HugsStackPtr hugs_root)
{
    HsWord32 arg1;
    HsWord32 res1;
    arg1 = hugs->getWord32();
    res1 = alarm(arg1);
    hugs->putWord32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef raise
#endif

static void hugsprim_raise_2(HugsStackPtr);
static void hugsprim_raise_2(HugsStackPtr hugs_root)
{
    HsInt32 arg1;
    HsInt32 res1;
    arg1 = hugs->getInt32();
    res1 = raise(arg1);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef killpg
#endif

static void hugsprim_killpg_1(HugsStackPtr);
static void hugsprim_killpg_1(HugsStackPtr hugs_root)
{
    HsInt32 arg1;
    HsInt32 arg2;
    HsInt32 res1;
    arg1 = hugs->getInt32();
    arg2 = hugs->getInt32();
    res1 = killpg(arg1, arg2);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef kill
#endif

static void hugsprim_kill_0(HugsStackPtr);
static void hugsprim_kill_0(HugsStackPtr hugs_root)
{
    HsInt32 arg1;
    HsInt32 arg2;
    HsInt32 res1;
    arg1 = hugs->getInt32();
    arg2 = hugs->getInt32();
    res1 = kill(arg1, arg2);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

static struct hugs_primitive hugs_primTable[] = {
    {"c_sigpending", 2, hugsprim_sigpending_8},
    {"c_sigismember", 3, hugsprim_sigismember_7},
    {"c_sigfillset", 2, hugsprim_sigfillset_6},
    {"c_sigdelset", 3, hugsprim_sigdelset_5},
    {"c_sigsuspend", 2, hugsprim_sigsuspend_4},
    {"c_alarm", 2, hugsprim_alarm_3},
    {"c_raise", 2, hugsprim_raise_2},
    {"c_killpg", 3, hugsprim_killpg_1},
    {"c_kill", 3, hugsprim_kill_0},
};

static void hugs_primControl(int);
static void hugs_primControl(what)
int what; {
}

#ifdef STATIC_LINKAGE
#define initModule initSPSignals
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

