/*===-- IPO.h - Interprocedural Transformations C Interface -----*- C++ -*-===*\
|*                                                                            *|
|*                     The LLVM Compiler Infrastructure                       *|
|*                                                                            *|
|* This file is distributed under the University of Illinois Open Source      *|
|* License. See LICENSE.TXT for details.                                      *|
|*                                                                            *|
|*===----------------------------------------------------------------------===*|
|*                                                                            *|
|* This header declares the C interface to libLLVMIPO.a, which implements     *|
|* various interprocedural transformations of the LLVM IR.                    *|
|*                                                                            *|
\*===----------------------------------------------------------------------===*/

#ifndef LLVM_C_TRANSFORMS_IPO_H
#define LLVM_C_TRANSFORMS_IPO_H

#include "llvm-c/Types.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @defgroup LLVMCTransformsIPO Interprocedural transformations
 * @ingroup LLVMCTransforms
 *
 * @{
 */

/** See llvm::createArgumentPromotionPass function. */
void LLVM_STDCALL LLVMAddArgumentPromotionPass(LLVMPassManagerRef PM);

/** See llvm::createConstantMergePass function. */
void LLVM_STDCALL LLVMAddConstantMergePass(LLVMPassManagerRef PM);

/** See llvm::createCalledValuePropagationPass function. */
void LLVM_STDCALL LLVMAddCalledValuePropagationPass(LLVMPassManagerRef PM);

/** See llvm::createDeadArgEliminationPass function. */
void LLVM_STDCALL LLVMAddDeadArgEliminationPass(LLVMPassManagerRef PM);

/** See llvm::createFunctionAttrsPass function. */
void LLVM_STDCALL LLVMAddFunctionAttrsPass(LLVMPassManagerRef PM);

/** See llvm::createFunctionInliningPass function. */
void LLVM_STDCALL LLVMAddFunctionInliningPass(LLVMPassManagerRef PM);

/** See llvm::createAlwaysInlinerPass function. */
void LLVM_STDCALL LLVMAddAlwaysInlinerPass(LLVMPassManagerRef PM);

/** See llvm::createGlobalDCEPass function. */
void LLVM_STDCALL LLVMAddGlobalDCEPass(LLVMPassManagerRef PM);

/** See llvm::createGlobalOptimizerPass function. */
void LLVM_STDCALL LLVMAddGlobalOptimizerPass(LLVMPassManagerRef PM);

/** See llvm::createIPConstantPropagationPass function. */
void LLVM_STDCALL LLVMAddIPConstantPropagationPass(LLVMPassManagerRef PM);

/** See llvm::createPruneEHPass function. */
void LLVM_STDCALL LLVMAddPruneEHPass(LLVMPassManagerRef PM);

/** See llvm::createIPSCCPPass function. */
void LLVM_STDCALL LLVMAddIPSCCPPass(LLVMPassManagerRef PM);

/** See llvm::createInternalizePass function. */
void LLVM_STDCALL LLVMAddInternalizePass(LLVMPassManagerRef, unsigned AllButMain);

/** See llvm::createStripDeadPrototypesPass function. */
void LLVM_STDCALL LLVMAddStripDeadPrototypesPass(LLVMPassManagerRef PM);

/** See llvm::createStripSymbolsPass function. */
void LLVM_STDCALL LLVMAddStripSymbolsPass(LLVMPassManagerRef PM);

/**
 * @}
 */

#ifdef __cplusplus
}
#endif /* defined(__cplusplus) */

#endif
