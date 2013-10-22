/* Machine generated file, do not modify */
#include <stdlib.h>
#include "HsFFI.h"

static HugsAPI5 *hugs = 0;
#include "Storable_aux.h"

#ifndef ENABLE_MACRO_INTERFACE
#undef writeWord64OffPtr
#endif

static void hugsprim_writeWord64OffPtr_29(HugsStackPtr);
static void hugsprim_writeWord64OffPtr_29(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt arg2;
    HsWord64 arg3;
    arg1 = hugs->getPtr();
    arg2 = hugs->getInt();
    arg3 = hugs->getWord64();
    writeWord64OffPtr(arg1, arg2, arg3);
    
    hugs->returnIO(hugs_root,0);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef writeWord32OffPtr
#endif

static void hugsprim_writeWord32OffPtr_28(HugsStackPtr);
static void hugsprim_writeWord32OffPtr_28(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt arg2;
    HsWord32 arg3;
    arg1 = hugs->getPtr();
    arg2 = hugs->getInt();
    arg3 = hugs->getWord32();
    writeWord32OffPtr(arg1, arg2, arg3);
    
    hugs->returnIO(hugs_root,0);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef writeWord16OffPtr
#endif

static void hugsprim_writeWord16OffPtr_27(HugsStackPtr);
static void hugsprim_writeWord16OffPtr_27(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt arg2;
    HsWord16 arg3;
    arg1 = hugs->getPtr();
    arg2 = hugs->getInt();
    arg3 = hugs->getWord16();
    writeWord16OffPtr(arg1, arg2, arg3);
    
    hugs->returnIO(hugs_root,0);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef writeWord8OffPtr
#endif

static void hugsprim_writeWord8OffPtr_26(HugsStackPtr);
static void hugsprim_writeWord8OffPtr_26(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt arg2;
    HsWord8 arg3;
    arg1 = hugs->getPtr();
    arg2 = hugs->getInt();
    arg3 = hugs->getWord8();
    writeWord8OffPtr(arg1, arg2, arg3);
    
    hugs->returnIO(hugs_root,0);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef writeInt64OffPtr
#endif

static void hugsprim_writeInt64OffPtr_25(HugsStackPtr);
static void hugsprim_writeInt64OffPtr_25(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt arg2;
    HsInt64 arg3;
    arg1 = hugs->getPtr();
    arg2 = hugs->getInt();
    arg3 = hugs->getInt64();
    writeInt64OffPtr(arg1, arg2, arg3);
    
    hugs->returnIO(hugs_root,0);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef writeInt32OffPtr
#endif

static void hugsprim_writeInt32OffPtr_24(HugsStackPtr);
static void hugsprim_writeInt32OffPtr_24(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt arg2;
    HsInt32 arg3;
    arg1 = hugs->getPtr();
    arg2 = hugs->getInt();
    arg3 = hugs->getInt32();
    writeInt32OffPtr(arg1, arg2, arg3);
    
    hugs->returnIO(hugs_root,0);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef writeInt16OffPtr
#endif

static void hugsprim_writeInt16OffPtr_23(HugsStackPtr);
static void hugsprim_writeInt16OffPtr_23(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt arg2;
    HsInt16 arg3;
    arg1 = hugs->getPtr();
    arg2 = hugs->getInt();
    arg3 = hugs->getInt16();
    writeInt16OffPtr(arg1, arg2, arg3);
    
    hugs->returnIO(hugs_root,0);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef writeInt8OffPtr
#endif

static void hugsprim_writeInt8OffPtr_22(HugsStackPtr);
static void hugsprim_writeInt8OffPtr_22(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt arg2;
    HsInt8 arg3;
    arg1 = hugs->getPtr();
    arg2 = hugs->getInt();
    arg3 = hugs->getInt8();
    writeInt8OffPtr(arg1, arg2, arg3);
    
    hugs->returnIO(hugs_root,0);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef writeStablePtrOffPtr
#endif

static void hugsprim_writeStablePtrOffPtr_21(HugsStackPtr);
static void hugsprim_writeStablePtrOffPtr_21(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt arg2;
    HsStablePtr arg3;
    arg1 = hugs->getPtr();
    arg2 = hugs->getInt();
    arg3 = hugs->getStablePtr4();
    writeStablePtrOffPtr(arg1, arg2, arg3);
    
    hugs->returnIO(hugs_root,0);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef writeDoubleOffPtr
#endif

static void hugsprim_writeDoubleOffPtr_20(HugsStackPtr);
static void hugsprim_writeDoubleOffPtr_20(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt arg2;
    HsDouble arg3;
    arg1 = hugs->getPtr();
    arg2 = hugs->getInt();
    arg3 = hugs->getDouble();
    writeDoubleOffPtr(arg1, arg2, arg3);
    
    hugs->returnIO(hugs_root,0);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef writeFloatOffPtr
#endif

static void hugsprim_writeFloatOffPtr_19(HugsStackPtr);
static void hugsprim_writeFloatOffPtr_19(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt arg2;
    HsFloat arg3;
    arg1 = hugs->getPtr();
    arg2 = hugs->getInt();
    arg3 = hugs->getFloat();
    writeFloatOffPtr(arg1, arg2, arg3);
    
    hugs->returnIO(hugs_root,0);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef writeFunPtrOffPtr
#endif

static void hugsprim_writeFunPtrOffPtr_18(HugsStackPtr);
static void hugsprim_writeFunPtrOffPtr_18(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt arg2;
    HsFunPtr arg3;
    arg1 = hugs->getPtr();
    arg2 = hugs->getInt();
    arg3 = hugs->getFunPtr();
    writeFunPtrOffPtr(arg1, arg2, arg3);
    
    hugs->returnIO(hugs_root,0);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef writePtrOffPtr
#endif

static void hugsprim_writePtrOffPtr_17(HugsStackPtr);
static void hugsprim_writePtrOffPtr_17(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt arg2;
    HsPtr arg3;
    arg1 = hugs->getPtr();
    arg2 = hugs->getInt();
    arg3 = hugs->getPtr();
    writePtrOffPtr(arg1, arg2, arg3);
    
    hugs->returnIO(hugs_root,0);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef writeCharOffPtr
#endif

static void hugsprim_writeCharOffPtr_16(HugsStackPtr);
static void hugsprim_writeCharOffPtr_16(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt arg2;
    HsChar arg3;
    arg1 = hugs->getPtr();
    arg2 = hugs->getInt();
    arg3 = hugs->getChar();
    writeCharOffPtr(arg1, arg2, arg3);
    
    hugs->returnIO(hugs_root,0);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef writeIntOffPtr
#endif

static void hugsprim_writeIntOffPtr_15(HugsStackPtr);
static void hugsprim_writeIntOffPtr_15(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt arg2;
    HsInt arg3;
    arg1 = hugs->getPtr();
    arg2 = hugs->getInt();
    arg3 = hugs->getInt();
    writeIntOffPtr(arg1, arg2, arg3);
    
    hugs->returnIO(hugs_root,0);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef readWord64OffPtr
#endif

static void hugsprim_readWord64OffPtr_14(HugsStackPtr);
static void hugsprim_readWord64OffPtr_14(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt arg2;
    HsWord64 res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getInt();
    res1 = readWord64OffPtr(arg1, arg2);
    hugs->putWord64(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef readWord32OffPtr
#endif

static void hugsprim_readWord32OffPtr_13(HugsStackPtr);
static void hugsprim_readWord32OffPtr_13(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt arg2;
    HsWord32 res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getInt();
    res1 = readWord32OffPtr(arg1, arg2);
    hugs->putWord32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef readWord16OffPtr
#endif

static void hugsprim_readWord16OffPtr_12(HugsStackPtr);
static void hugsprim_readWord16OffPtr_12(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt arg2;
    HsWord16 res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getInt();
    res1 = readWord16OffPtr(arg1, arg2);
    hugs->putWord16(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef readWord8OffPtr
#endif

static void hugsprim_readWord8OffPtr_11(HugsStackPtr);
static void hugsprim_readWord8OffPtr_11(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt arg2;
    HsWord8 res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getInt();
    res1 = readWord8OffPtr(arg1, arg2);
    hugs->putWord8(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef readInt64OffPtr
#endif

static void hugsprim_readInt64OffPtr_10(HugsStackPtr);
static void hugsprim_readInt64OffPtr_10(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt arg2;
    HsInt64 res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getInt();
    res1 = readInt64OffPtr(arg1, arg2);
    hugs->putInt64(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef readInt32OffPtr
#endif

static void hugsprim_readInt32OffPtr_9(HugsStackPtr);
static void hugsprim_readInt32OffPtr_9(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt arg2;
    HsInt32 res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getInt();
    res1 = readInt32OffPtr(arg1, arg2);
    hugs->putInt32(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef readInt16OffPtr
#endif

static void hugsprim_readInt16OffPtr_8(HugsStackPtr);
static void hugsprim_readInt16OffPtr_8(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt arg2;
    HsInt16 res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getInt();
    res1 = readInt16OffPtr(arg1, arg2);
    hugs->putInt16(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef readInt8OffPtr
#endif

static void hugsprim_readInt8OffPtr_7(HugsStackPtr);
static void hugsprim_readInt8OffPtr_7(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt arg2;
    HsInt8 res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getInt();
    res1 = readInt8OffPtr(arg1, arg2);
    hugs->putInt8(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef readStablePtrOffPtr
#endif

static void hugsprim_readStablePtrOffPtr_6(HugsStackPtr);
static void hugsprim_readStablePtrOffPtr_6(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt arg2;
    HsStablePtr res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getInt();
    res1 = readStablePtrOffPtr(arg1, arg2);
    hugs->putStablePtr4(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef readDoubleOffPtr
#endif

static void hugsprim_readDoubleOffPtr_5(HugsStackPtr);
static void hugsprim_readDoubleOffPtr_5(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt arg2;
    HsDouble res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getInt();
    res1 = readDoubleOffPtr(arg1, arg2);
    hugs->putDouble(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef readFloatOffPtr
#endif

static void hugsprim_readFloatOffPtr_4(HugsStackPtr);
static void hugsprim_readFloatOffPtr_4(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt arg2;
    HsFloat res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getInt();
    res1 = readFloatOffPtr(arg1, arg2);
    hugs->putFloat(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef readFunPtrOffPtr
#endif

static void hugsprim_readFunPtrOffPtr_3(HugsStackPtr);
static void hugsprim_readFunPtrOffPtr_3(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt arg2;
    HsFunPtr res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getInt();
    res1 = readFunPtrOffPtr(arg1, arg2);
    hugs->putFunPtr(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef readPtrOffPtr
#endif

static void hugsprim_readPtrOffPtr_2(HugsStackPtr);
static void hugsprim_readPtrOffPtr_2(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt arg2;
    HsPtr res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getInt();
    res1 = readPtrOffPtr(arg1, arg2);
    hugs->putPtr(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef readCharOffPtr
#endif

static void hugsprim_readCharOffPtr_1(HugsStackPtr);
static void hugsprim_readCharOffPtr_1(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt arg2;
    HsChar res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getInt();
    res1 = readCharOffPtr(arg1, arg2);
    hugs->putChar(res1);
    hugs->returnIO(hugs_root,1);
}

#ifndef ENABLE_MACRO_INTERFACE
#undef readIntOffPtr
#endif

static void hugsprim_readIntOffPtr_0(HugsStackPtr);
static void hugsprim_readIntOffPtr_0(HugsStackPtr hugs_root)
{
    HsPtr arg1;
    HsInt arg2;
    HsInt res1;
    arg1 = hugs->getPtr();
    arg2 = hugs->getInt();
    res1 = readIntOffPtr(arg1, arg2);
    hugs->putInt(res1);
    hugs->returnIO(hugs_root,1);
}

static struct hugs_primitive hugs_primTable[] = {
    {"writeWord64OffPtr", 4, hugsprim_writeWord64OffPtr_29},
    {"writeWord32OffPtr", 4, hugsprim_writeWord32OffPtr_28},
    {"writeWord16OffPtr", 4, hugsprim_writeWord16OffPtr_27},
    {"writeWord8OffPtr", 4, hugsprim_writeWord8OffPtr_26},
    {"writeInt64OffPtr", 4, hugsprim_writeInt64OffPtr_25},
    {"writeInt32OffPtr", 4, hugsprim_writeInt32OffPtr_24},
    {"writeInt16OffPtr", 4, hugsprim_writeInt16OffPtr_23},
    {"writeInt8OffPtr", 4, hugsprim_writeInt8OffPtr_22},
    {"writeStablePtrOffPtr", 4, hugsprim_writeStablePtrOffPtr_21},
    {"writeDoubleOffPtr", 4, hugsprim_writeDoubleOffPtr_20},
    {"writeFloatOffPtr", 4, hugsprim_writeFloatOffPtr_19},
    {"writeFunPtrOffPtr", 4, hugsprim_writeFunPtrOffPtr_18},
    {"writePtrOffPtr", 4, hugsprim_writePtrOffPtr_17},
    {"writeCharOffPtr", 4, hugsprim_writeCharOffPtr_16},
    {"writeIntOffPtr", 4, hugsprim_writeIntOffPtr_15},
    {"readWord64OffPtr", 3, hugsprim_readWord64OffPtr_14},
    {"readWord32OffPtr", 3, hugsprim_readWord32OffPtr_13},
    {"readWord16OffPtr", 3, hugsprim_readWord16OffPtr_12},
    {"readWord8OffPtr", 3, hugsprim_readWord8OffPtr_11},
    {"readInt64OffPtr", 3, hugsprim_readInt64OffPtr_10},
    {"readInt32OffPtr", 3, hugsprim_readInt32OffPtr_9},
    {"readInt16OffPtr", 3, hugsprim_readInt16OffPtr_8},
    {"readInt8OffPtr", 3, hugsprim_readInt8OffPtr_7},
    {"readStablePtrOffPtr", 3, hugsprim_readStablePtrOffPtr_6},
    {"readDoubleOffPtr", 3, hugsprim_readDoubleOffPtr_5},
    {"readFloatOffPtr", 3, hugsprim_readFloatOffPtr_4},
    {"readFunPtrOffPtr", 3, hugsprim_readFunPtrOffPtr_3},
    {"readPtrOffPtr", 3, hugsprim_readPtrOffPtr_2},
    {"readCharOffPtr", 3, hugsprim_readCharOffPtr_1},
    {"readIntOffPtr", 3, hugsprim_readIntOffPtr_0},
};

static void hugs_primControl(int);
static void hugs_primControl(what)
int what; {
}

#ifdef STATIC_LINKAGE
#define initModule initHStorable
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

