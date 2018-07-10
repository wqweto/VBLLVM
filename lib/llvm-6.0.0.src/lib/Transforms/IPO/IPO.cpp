//===-- IPO.cpp -----------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the common infrastructure (including C bindings) for
// libLLVMIPO.a, which implements several transformations over the LLVM
// intermediate representation.
//
//===----------------------------------------------------------------------===//

#include "llvm-c/Transforms/IPO.h"
#include "llvm-c/Initialization.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/InitializePasses.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/IPO/AlwaysInliner.h"
#include "llvm/Transforms/IPO/FunctionAttrs.h"

using namespace llvm;

void llvm::initializeIPO(PassRegistry &Registry) {
  initializeArgPromotionPass(Registry);
  initializeCalledValuePropagationLegacyPassPass(Registry);
  initializeConstantMergeLegacyPassPass(Registry);
  initializeCrossDSOCFIPass(Registry);
  initializeDAEPass(Registry);
  initializeDAHPass(Registry);
  initializeForceFunctionAttrsLegacyPassPass(Registry);
  initializeGlobalDCELegacyPassPass(Registry);
  initializeGlobalOptLegacyPassPass(Registry);
  initializeGlobalSplitPass(Registry);
  initializeIPCPPass(Registry);
  initializeAlwaysInlinerLegacyPassPass(Registry);
  initializeSimpleInlinerPass(Registry);
  initializeInferFunctionAttrsLegacyPassPass(Registry);
  initializeInternalizeLegacyPassPass(Registry);
  initializeLoopExtractorPass(Registry);
  initializeBlockExtractorPassPass(Registry);
  initializeSingleLoopExtractorPass(Registry);
  initializeLowerTypeTestsPass(Registry);
  initializeMergeFunctionsPass(Registry);
  initializePartialInlinerLegacyPassPass(Registry);
  initializePostOrderFunctionAttrsLegacyPassPass(Registry);
  initializeReversePostOrderFunctionAttrsLegacyPassPass(Registry);
  initializePruneEHPass(Registry);
  initializeStripDeadPrototypesLegacyPassPass(Registry);
  initializeStripSymbolsPass(Registry);
  initializeStripDebugDeclarePass(Registry);
  initializeStripDeadDebugInfoPass(Registry);
  initializeStripNonDebugSymbolsPass(Registry);
  initializeBarrierNoopPass(Registry);
  initializeEliminateAvailableExternallyLegacyPassPass(Registry);
  initializeSampleProfileLoaderLegacyPassPass(Registry);
  initializeFunctionImportLegacyPassPass(Registry);
  initializeWholeProgramDevirtPass(Registry);
}

void LLVM_STDCALL LLVMInitializeIPO(LLVMPassRegistryRef R) {
  initializeIPO(*unwrap(R));
}

void LLVM_STDCALL LLVMAddArgumentPromotionPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createArgumentPromotionPass());
}

void LLVM_STDCALL LLVMAddCalledValuePropagationPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createCalledValuePropagationPass());
}

void LLVM_STDCALL LLVMAddConstantMergePass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createConstantMergePass());
}

void LLVM_STDCALL LLVMAddDeadArgEliminationPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createDeadArgEliminationPass());
}

void LLVM_STDCALL LLVMAddFunctionAttrsPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createPostOrderFunctionAttrsLegacyPass());
}

void LLVM_STDCALL LLVMAddFunctionInliningPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createFunctionInliningPass());
}

void LLVM_STDCALL LLVMAddAlwaysInlinerPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(llvm::createAlwaysInlinerLegacyPass());
}

void LLVM_STDCALL LLVMAddGlobalDCEPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createGlobalDCEPass());
}

void LLVM_STDCALL LLVMAddGlobalOptimizerPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createGlobalOptimizerPass());
}

void LLVM_STDCALL LLVMAddIPConstantPropagationPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createIPConstantPropagationPass());
}

void LLVM_STDCALL LLVMAddPruneEHPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createPruneEHPass());
}

void LLVM_STDCALL LLVMAddIPSCCPPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createIPSCCPPass());
}

void LLVM_STDCALL LLVMAddInternalizePass(LLVMPassManagerRef PM, unsigned AllButMain) {
  auto PreserveMain = [=](const GlobalValue &GV) {
    return AllButMain && GV.getName() == "main";
  };
  unwrap(PM)->add(createInternalizePass(PreserveMain));
}

void LLVM_STDCALL LLVMAddStripDeadPrototypesPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createStripDeadPrototypesPass());
}

void LLVM_STDCALL LLVMAddStripSymbolsPass(LLVMPassManagerRef PM) {
  unwrap(PM)->add(createStripSymbolsPass());
}
