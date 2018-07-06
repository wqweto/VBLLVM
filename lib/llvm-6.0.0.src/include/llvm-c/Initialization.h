/*===-- llvm-c/Initialization.h - Initialization C Interface ------*- C -*-===*\
|*                                                                            *|
|*                     The LLVM Compiler Infrastructure                       *|
|*                                                                            *|
|* This file is distributed under the University of Illinois Open Source      *|
|* License. See LICENSE.TXT for details.                                      *|
|*                                                                            *|
|*===----------------------------------------------------------------------===*|
|*                                                                            *|
|* This header declares the C interface to LLVM initialization routines,      *|
|* which must be called before you can use the functionality provided by      *|
|* the corresponding LLVM library.                                            *|
|*                                                                            *|
\*===----------------------------------------------------------------------===*/

#ifndef LLVM_C_INITIALIZATION_H
#define LLVM_C_INITIALIZATION_H

#include "llvm-c/Types.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @defgroup LLVMCInitialization Initialization Routines
 * @ingroup LLVMC
 *
 * This module contains routines used to initialize the LLVM system.
 *
 * @{
 */

void LLVM_STDCALL LLVMInitializeCore(LLVMPassRegistryRef R);
void LLVM_STDCALL LLVMInitializeTransformUtils(LLVMPassRegistryRef R);
void LLVM_STDCALL LLVMInitializeScalarOpts(LLVMPassRegistryRef R);
void LLVM_STDCALL LLVMInitializeObjCARCOpts(LLVMPassRegistryRef R);
void LLVM_STDCALL LLVMInitializeVectorization(LLVMPassRegistryRef R);
void LLVM_STDCALL LLVMInitializeInstCombine(LLVMPassRegistryRef R);
void LLVM_STDCALL LLVMInitializeIPO(LLVMPassRegistryRef R);
void LLVM_STDCALL LLVMInitializeInstrumentation(LLVMPassRegistryRef R);
void LLVM_STDCALL LLVMInitializeAnalysis(LLVMPassRegistryRef R);
void LLVM_STDCALL LLVMInitializeIPA(LLVMPassRegistryRef R);
void LLVM_STDCALL LLVMInitializeCodeGen(LLVMPassRegistryRef R);
void LLVM_STDCALL LLVMInitializeTarget(LLVMPassRegistryRef R);

/**
 * @}
 */

#ifdef __cplusplus
}
#endif

#endif
