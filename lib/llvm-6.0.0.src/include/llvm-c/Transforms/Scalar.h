/*===-- Scalar.h - Scalar Transformation Library C Interface ----*- C++ -*-===*\
|*                                                                            *|
|*                     The LLVM Compiler Infrastructure                       *|
|*                                                                            *|
|* This file is distributed under the University of Illinois Open Source      *|
|* License. See LICENSE.TXT for details.                                      *|
|*                                                                            *|
|*===----------------------------------------------------------------------===*|
|*                                                                            *|
|* This header declares the C interface to libLLVMScalarOpts.a, which         *|
|* implements various scalar transformations of the LLVM IR.                  *|
|*                                                                            *|
|* Many exotic languages can interoperate with C code but have a harder time  *|
|* with C++ due to name mangling. So in addition to C, this interface enables *|
|* tools written in such languages.                                           *|
|*                                                                            *|
\*===----------------------------------------------------------------------===*/

#ifndef LLVM_C_TRANSFORMS_SCALAR_H
#define LLVM_C_TRANSFORMS_SCALAR_H

#include "llvm-c/Types.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @defgroup LLVMCTransformsScalar Scalar transformations
 * @ingroup LLVMCTransforms
 *
 * @{
 */

/** See llvm::createAggressiveDCEPass function. */
void LLVM_STDCALL LLVMAddAggressiveDCEPass(LLVMPassManagerRef PM);

/** See llvm::createBitTrackingDCEPass function. */
void LLVM_STDCALL LLVMAddBitTrackingDCEPass(LLVMPassManagerRef PM);

/** See llvm::createAlignmentFromAssumptionsPass function. */
void LLVM_STDCALL LLVMAddAlignmentFromAssumptionsPass(LLVMPassManagerRef PM);

/** See llvm::createCFGSimplificationPass function. */
void LLVM_STDCALL LLVMAddCFGSimplificationPass(LLVMPassManagerRef PM);

/** See llvm::createDeadStoreEliminationPass function. */
void LLVM_STDCALL LLVMAddDeadStoreEliminationPass(LLVMPassManagerRef PM);

/** See llvm::createScalarizerPass function. */
void LLVM_STDCALL LLVMAddScalarizerPass(LLVMPassManagerRef PM);

/** See llvm::createMergedLoadStoreMotionPass function. */
void LLVM_STDCALL LLVMAddMergedLoadStoreMotionPass(LLVMPassManagerRef PM);

/** See llvm::createGVNPass function. */
void LLVM_STDCALL LLVMAddGVNPass(LLVMPassManagerRef PM);

/** See llvm::createGVNPass function. */
void LLVM_STDCALL LLVMAddNewGVNPass(LLVMPassManagerRef PM);

/** See llvm::createIndVarSimplifyPass function. */
void LLVM_STDCALL LLVMAddIndVarSimplifyPass(LLVMPassManagerRef PM);

/** See llvm::createInstructionCombiningPass function. */
void LLVM_STDCALL LLVMAddInstructionCombiningPass(LLVMPassManagerRef PM);

/** See llvm::createJumpThreadingPass function. */
void LLVM_STDCALL LLVMAddJumpThreadingPass(LLVMPassManagerRef PM);

/** See llvm::createLICMPass function. */
void LLVM_STDCALL LLVMAddLICMPass(LLVMPassManagerRef PM);

/** See llvm::createLoopDeletionPass function. */
void LLVM_STDCALL LLVMAddLoopDeletionPass(LLVMPassManagerRef PM);

/** See llvm::createLoopIdiomPass function */
void LLVM_STDCALL LLVMAddLoopIdiomPass(LLVMPassManagerRef PM);

/** See llvm::createLoopRotatePass function. */
void LLVM_STDCALL LLVMAddLoopRotatePass(LLVMPassManagerRef PM);

/** See llvm::createLoopRerollPass function. */
void LLVM_STDCALL LLVMAddLoopRerollPass(LLVMPassManagerRef PM);

/** See llvm::createLoopUnrollPass function. */
void LLVM_STDCALL LLVMAddLoopUnrollPass(LLVMPassManagerRef PM);

/** See llvm::createLoopUnswitchPass function. */
void LLVM_STDCALL LLVMAddLoopUnswitchPass(LLVMPassManagerRef PM);

/** See llvm::createMemCpyOptPass function. */
void LLVM_STDCALL LLVMAddMemCpyOptPass(LLVMPassManagerRef PM);

/** See llvm::createPartiallyInlineLibCallsPass function. */
void LLVM_STDCALL LLVMAddPartiallyInlineLibCallsPass(LLVMPassManagerRef PM);

/** See llvm::createLowerSwitchPass function. */
void LLVM_STDCALL LLVMAddLowerSwitchPass(LLVMPassManagerRef PM);

/** See llvm::createPromoteMemoryToRegisterPass function. */
void LLVM_STDCALL LLVMAddPromoteMemoryToRegisterPass(LLVMPassManagerRef PM);

/** See llvm::createReassociatePass function. */
void LLVM_STDCALL LLVMAddReassociatePass(LLVMPassManagerRef PM);

/** See llvm::createSCCPPass function. */
void LLVM_STDCALL LLVMAddSCCPPass(LLVMPassManagerRef PM);

/** See llvm::createSROAPass function. */
void LLVM_STDCALL LLVMAddScalarReplAggregatesPass(LLVMPassManagerRef PM);

/** See llvm::createSROAPass function. */
void LLVM_STDCALL LLVMAddScalarReplAggregatesPassSSA(LLVMPassManagerRef PM);

/** See llvm::createSROAPass function. */
void LLVM_STDCALL LLVMAddScalarReplAggregatesPassWithThreshold(LLVMPassManagerRef PM,
                                                  int Threshold);

/** See llvm::createSimplifyLibCallsPass function. */
void LLVM_STDCALL LLVMAddSimplifyLibCallsPass(LLVMPassManagerRef PM);

/** See llvm::createTailCallEliminationPass function. */
void LLVM_STDCALL LLVMAddTailCallEliminationPass(LLVMPassManagerRef PM);

/** See llvm::createConstantPropagationPass function. */
void LLVM_STDCALL LLVMAddConstantPropagationPass(LLVMPassManagerRef PM);

/** See llvm::demotePromoteMemoryToRegisterPass function. */
void LLVM_STDCALL LLVMAddDemoteMemoryToRegisterPass(LLVMPassManagerRef PM);

/** See llvm::createVerifierPass function. */
void LLVM_STDCALL LLVMAddVerifierPass(LLVMPassManagerRef PM);

/** See llvm::createCorrelatedValuePropagationPass function */
void LLVM_STDCALL LLVMAddCorrelatedValuePropagationPass(LLVMPassManagerRef PM);

/** See llvm::createEarlyCSEPass function */
void LLVM_STDCALL LLVMAddEarlyCSEPass(LLVMPassManagerRef PM);

/** See llvm::createEarlyCSEPass function */
void LLVM_STDCALL LLVMAddEarlyCSEMemSSAPass(LLVMPassManagerRef PM);

/** See llvm::createLowerExpectIntrinsicPass function */
void LLVM_STDCALL LLVMAddLowerExpectIntrinsicPass(LLVMPassManagerRef PM);

/** See llvm::createTypeBasedAliasAnalysisPass function */
void LLVM_STDCALL LLVMAddTypeBasedAliasAnalysisPass(LLVMPassManagerRef PM);

/** See llvm::createScopedNoAliasAAPass function */
void LLVM_STDCALL LLVMAddScopedNoAliasAAPass(LLVMPassManagerRef PM);

/** See llvm::createBasicAliasAnalysisPass function */
void LLVM_STDCALL LLVMAddBasicAliasAnalysisPass(LLVMPassManagerRef PM);

/**
 * @}
 */

#ifdef __cplusplus
}
#endif /* defined(__cplusplus) */

#endif
