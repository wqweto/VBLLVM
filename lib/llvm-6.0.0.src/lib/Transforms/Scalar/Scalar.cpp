//===-- Scalar.cpp --------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements common infrastructure for libLLVMScalarOpts.a, which
// implements several scalar transformations over the LLVM intermediate
// representation, including the C bindings for that library.
//
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/Scalar.h"
#include "llvm-c/Initialization.h"
#include "llvm-c/Transforms/Scalar.h"
#include "llvm/Analysis/BasicAliasAnalysis.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/Analysis/ScopedNoAliasAA.h"
#include "llvm/Analysis/TypeBasedAliasAnalysis.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Verifier.h"
#include "llvm/InitializePasses.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/SimpleLoopUnswitch.h"

using namespace llvm;

/// initializeScalarOptsPasses - Initialize all passes linked into the
/// ScalarOpts library.
void llvm::initializeScalarOpts(PassRegistry &Registry) {
  initializeADCELegacyPassPass(Registry);
  initializeBDCELegacyPassPass(Registry);
  initializeAlignmentFromAssumptionsPass(Registry);
  initializeCallSiteSplittingLegacyPassPass(Registry);
  initializeConstantHoistingLegacyPassPass(Registry);
  initializeConstantPropagationPass(Registry);
  initializeCorrelatedValuePropagationPass(Registry);
  initializeDCELegacyPassPass(Registry);
  initializeDeadInstEliminationPass(Registry);
  initializeDivRemPairsLegacyPassPass(Registry);
  initializeScalarizerPass(Registry);
  initializeDSELegacyPassPass(Registry);
  initializeGuardWideningLegacyPassPass(Registry);
  initializeGVNLegacyPassPass(Registry);
  initializeNewGVNLegacyPassPass(Registry);
  initializeEarlyCSELegacyPassPass(Registry);
  initializeEarlyCSEMemSSALegacyPassPass(Registry);
  initializeGVNHoistLegacyPassPass(Registry);
  initializeGVNSinkLegacyPassPass(Registry);
  initializeFlattenCFGPassPass(Registry);
  initializeInductiveRangeCheckEliminationPass(Registry);
  initializeIndVarSimplifyLegacyPassPass(Registry);
  initializeInferAddressSpacesPass(Registry);
  initializeJumpThreadingPass(Registry);
  initializeLegacyLICMPassPass(Registry);
  initializeLegacyLoopSinkPassPass(Registry);
  initializeLoopDataPrefetchLegacyPassPass(Registry);
  initializeLoopDeletionLegacyPassPass(Registry);
  initializeLoopAccessLegacyAnalysisPass(Registry);
  initializeLoopInstSimplifyLegacyPassPass(Registry);
  initializeLoopInterchangePass(Registry);
  initializeLoopPredicationLegacyPassPass(Registry);
  initializeLoopRotateLegacyPassPass(Registry);
  initializeLoopStrengthReducePass(Registry);
  initializeLoopRerollPass(Registry);
  initializeLoopUnrollPass(Registry);
  initializeLoopUnswitchPass(Registry);
  initializeLoopVersioningLICMPass(Registry);
  initializeLoopIdiomRecognizeLegacyPassPass(Registry);
  initializeLowerAtomicLegacyPassPass(Registry);
  initializeLowerExpectIntrinsicPass(Registry);
  initializeLowerGuardIntrinsicLegacyPassPass(Registry);
  initializeMemCpyOptLegacyPassPass(Registry);
  initializeMergeICmpsPass(Registry);
  initializeMergedLoadStoreMotionLegacyPassPass(Registry);
  initializeNaryReassociateLegacyPassPass(Registry);
  initializePartiallyInlineLibCallsLegacyPassPass(Registry);
  initializeReassociateLegacyPassPass(Registry);
  initializeRegToMemPass(Registry);
  initializeRewriteStatepointsForGCLegacyPassPass(Registry);
  initializeSCCPLegacyPassPass(Registry);
  initializeIPSCCPLegacyPassPass(Registry);
  initializeSROALegacyPassPass(Registry);
  initializeCFGSimplifyPassPass(Registry);
  initializeStructurizeCFGPass(Registry);
  initializeSimpleLoopUnswitchLegacyPassPass(Registry);
  initializeSinkingLegacyPassPass(Registry);
  initializeTailCallElimPass(Registry);
  initializeSeparateConstOffsetFromGEPPass(Registry);
  initializeSpeculativeExecutionLegacyPassPass(Registry);
  initializeStraightLineStrengthReducePass(Registry);
  initializePlaceBackedgeSafepointsImplPass(Registry);
  initializePlaceSafepointsPass(Registry);
  initializeFloat2IntLegacyPassPass(Registry);
  initializeLoopDistributeLegacyPass(Registry);
  initializeLoopLoadEliminationPass(Registry);
  initializeLoopSimplifyCFGLegacyPassPass(Registry);
  initializeLoopVersioningPassPass(Registry);
  initializeEntryExitInstrumenterPass(Registry);
  initializePostInlineEntryExitInstrumenterPass(Registry);
}

void LLVM_STDCALL LLVMInitializeScalarOpts(LLVMPassRegistryRef R) {
  initializeScalarOpts(*unwrap(R));
}

void LLVM_STDCALL LLVMAddAggressiveDCEPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createAggressiveDCEPass());
}

void LLVM_STDCALL LLVMAddBitTrackingDCEPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createBitTrackingDCEPass());
}

void LLVM_STDCALL LLVMAddAlignmentFromAssumptionsPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createAlignmentFromAssumptionsPass());
}

void LLVM_STDCALL LLVMAddCFGSimplificationPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createCFGSimplificationPass(1, false, false, true));
}

void LLVM_STDCALL LLVMAddDeadStoreEliminationPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createDeadStoreEliminationPass());
}

void LLVM_STDCALL LLVMAddScalarizerPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createScalarizerPass());
}

void LLVM_STDCALL LLVMAddGVNPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createGVNPass());
}

void LLVM_STDCALL LLVMAddNewGVNPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createNewGVNPass());
}

void LLVM_STDCALL LLVMAddMergedLoadStoreMotionPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createMergedLoadStoreMotionPass());
}

void LLVM_STDCALL LLVMAddIndVarSimplifyPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createIndVarSimplifyPass());
}

void LLVM_STDCALL LLVMAddInstructionCombiningPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createInstructionCombiningPass());
}

void LLVM_STDCALL LLVMAddJumpThreadingPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createJumpThreadingPass());
}

void LLVM_STDCALL LLVMAddLoopSinkPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createLoopSinkPass());
}

void LLVM_STDCALL LLVMAddLICMPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createLICMPass());
}

void LLVM_STDCALL LLVMAddLoopDeletionPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createLoopDeletionPass());
}

void LLVM_STDCALL LLVMAddLoopIdiomPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createLoopIdiomPass());
}

void LLVM_STDCALL LLVMAddLoopRotatePass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createLoopRotatePass());
}

void LLVM_STDCALL LLVMAddLoopRerollPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createLoopRerollPass());
}

void LLVM_STDCALL LLVMAddLoopSimplifyCFGPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createLoopSimplifyCFGPass());
}

void LLVM_STDCALL LLVMAddLoopUnrollPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createLoopUnrollPass());
}

void LLVM_STDCALL LLVMAddLoopUnswitchPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createLoopUnswitchPass());
}

void LLVM_STDCALL LLVMAddMemCpyOptPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createMemCpyOptPass());
}

void LLVM_STDCALL LLVMAddPartiallyInlineLibCallsPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createPartiallyInlineLibCallsPass());
}

void LLVM_STDCALL LLVMAddLowerSwitchPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createLowerSwitchPass());
}

void LLVM_STDCALL LLVMAddPromoteMemoryToRegisterPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createPromoteMemoryToRegisterPass());
}

void LLVM_STDCALL LLVMAddReassociatePass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createReassociatePass());
}

void LLVM_STDCALL LLVMAddSCCPPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createSCCPPass());
}

void LLVM_STDCALL LLVMAddScalarReplAggregatesPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createSROAPass());
}

void LLVM_STDCALL LLVMAddScalarReplAggregatesPassSSA(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createSROAPass());
}

void LLVM_STDCALL LLVMAddScalarReplAggregatesPassWithThreshold(LLVMPassManagerRef PM,
                                                  int Threshold) {
  unwrap(PM)->add(createSROAPass());
}

void LLVM_STDCALL LLVMAddSimplifyLibCallsPass(LLVMPassManagerRef PM) {
  // NOTE: The simplify-libcalls pass has been removed.
}

void LLVM_STDCALL LLVMAddTailCallEliminationPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createTailCallEliminationPass());
}

void LLVM_STDCALL LLVMAddConstantPropagationPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createConstantPropagationPass());
}

void LLVM_STDCALL LLVMAddDemoteMemoryToRegisterPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createDemoteRegisterToMemoryPass());
}

void LLVM_STDCALL LLVMAddVerifierPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createVerifierPass());
}

void LLVM_STDCALL LLVMAddCorrelatedValuePropagationPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createCorrelatedValuePropagationPass());
}

void LLVM_STDCALL LLVMAddEarlyCSEPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createEarlyCSEPass(false/*=UseMemorySSA*/));
}

void LLVM_STDCALL LLVMAddEarlyCSEMemSSAPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createEarlyCSEPass(true/*=UseMemorySSA*/));
}

void LLVM_STDCALL LLVMAddGVNHoistLegacyPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createGVNHoistPass());
}

void LLVM_STDCALL LLVMAddTypeBasedAliasAnalysisPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createTypeBasedAAWrapperPass());
}

void LLVM_STDCALL LLVMAddScopedNoAliasAAPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createScopedNoAliasAAWrapperPass());
}

void LLVM_STDCALL LLVMAddBasicAliasAnalysisPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createBasicAAWrapperPass());
}

void LLVM_STDCALL LLVMAddLowerExpectIntrinsicPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createLowerExpectIntrinsicPass());
}
