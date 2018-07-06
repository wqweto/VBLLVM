//===----------- OrcCBindings.cpp - C bindings for the Orc APIs -----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "OrcCBindingsStack.h"
#include "llvm-c/OrcBindings.h"

using namespace llvm;

LLVMSharedModuleRef LLVM_STDCALL LLVMOrcMakeSharedModule(LLVMModuleRef Mod) {
  return wrap(new std::shared_ptr<Module>(unwrap(Mod)));
}

void LLVM_STDCALL LLVMOrcDisposeSharedModuleRef(LLVMSharedModuleRef SharedMod) {
  delete unwrap(SharedMod);
}

LLVMOrcJITStackRef LLVM_STDCALL LLVMOrcCreateInstance(LLVMTargetMachineRef TM) {
  TargetMachine *TM2(unwrap(TM));

  Triple T(TM2->getTargetTriple());

  auto CompileCallbackMgr = orc::createLocalCompileCallbackManager(T, 0);
  auto IndirectStubsMgrBuilder =
      orc::createLocalIndirectStubsManagerBuilder(T);

  OrcCBindingsStack *JITStack = new OrcCBindingsStack(
      *TM2, std::move(CompileCallbackMgr), IndirectStubsMgrBuilder);

  return wrap(JITStack);
}

const char *LLVM_STDCALL LLVMOrcGetErrorMsg(LLVMOrcJITStackRef JITStack) {
  OrcCBindingsStack &J = *unwrap(JITStack);
  return J.getErrorMessage().c_str();
}

void LLVM_STDCALL LLVMOrcGetMangledSymbol(LLVMOrcJITStackRef JITStack, char **MangledName,
                             const char *SymbolName) {
  OrcCBindingsStack &J = *unwrap(JITStack);
  std::string Mangled = J.mangle(SymbolName);
  *MangledName = new char[Mangled.size() + 1];
  strcpy(*MangledName, Mangled.c_str());
}

void LLVM_STDCALL LLVMOrcDisposeMangledSymbol(char *MangledName) { delete[] MangledName; }

LLVMOrcErrorCode LLVM_STDCALL 
LLVMOrcCreateLazyCompileCallback(LLVMOrcJITStackRef JITStack,
                                 LLVMOrcTargetAddress *RetAddr,
                                 LLVMOrcLazyCompileCallbackFn Callback,
                                 void *CallbackCtx) {
  OrcCBindingsStack &J = *unwrap(JITStack);
  return J.createLazyCompileCallback(*RetAddr, Callback, CallbackCtx);
}

LLVMOrcErrorCode LLVM_STDCALL LLVMOrcCreateIndirectStub(LLVMOrcJITStackRef JITStack,
                                           const char *StubName,
                                           LLVMOrcTargetAddress InitAddr) {
  OrcCBindingsStack &J = *unwrap(JITStack);
  return J.createIndirectStub(StubName, InitAddr);
}

LLVMOrcErrorCode LLVM_STDCALL LLVMOrcSetIndirectStubPointer(LLVMOrcJITStackRef JITStack,
                                               const char *StubName,
                                               LLVMOrcTargetAddress NewAddr) {
  OrcCBindingsStack &J = *unwrap(JITStack);
  return J.setIndirectStubPointer(StubName, NewAddr);
}

LLVMOrcErrorCode LLVM_STDCALL 
LLVMOrcAddEagerlyCompiledIR(LLVMOrcJITStackRef JITStack,
                            LLVMOrcModuleHandle *RetHandle,
                            LLVMSharedModuleRef Mod,
                            LLVMOrcSymbolResolverFn SymbolResolver,
                            void *SymbolResolverCtx) {
  OrcCBindingsStack &J = *unwrap(JITStack);
  std::shared_ptr<Module> *M(unwrap(Mod));
  return J.addIRModuleEager(*RetHandle, *M, SymbolResolver, SymbolResolverCtx);
}

LLVMOrcErrorCode LLVM_STDCALL 
LLVMOrcAddLazilyCompiledIR(LLVMOrcJITStackRef JITStack,
                           LLVMOrcModuleHandle *RetHandle,
                           LLVMSharedModuleRef Mod,
                           LLVMOrcSymbolResolverFn SymbolResolver,
                           void *SymbolResolverCtx) {
  OrcCBindingsStack &J = *unwrap(JITStack);
  std::shared_ptr<Module> *M(unwrap(Mod));
  return J.addIRModuleLazy(*RetHandle, *M, SymbolResolver, SymbolResolverCtx);
}

LLVMOrcErrorCode LLVM_STDCALL 
LLVMOrcAddObjectFile(LLVMOrcJITStackRef JITStack,
                     LLVMOrcModuleHandle *RetHandle,
                     LLVMMemoryBufferRef Obj,
                     LLVMOrcSymbolResolverFn SymbolResolver,
                     void *SymbolResolverCtx) {
  OrcCBindingsStack &J = *unwrap(JITStack);
  std::unique_ptr<MemoryBuffer> O(unwrap(Obj));
  return J.addObject(*RetHandle, std::move(O), SymbolResolver,
                     SymbolResolverCtx);
}

LLVMOrcErrorCode LLVM_STDCALL LLVMOrcRemoveModule(LLVMOrcJITStackRef JITStack,
                                     LLVMOrcModuleHandle H) {
  OrcCBindingsStack &J = *unwrap(JITStack);
  return J.removeModule(H);
}

LLVMOrcErrorCode LLVM_STDCALL LLVMOrcGetSymbolAddress(LLVMOrcJITStackRef JITStack,
                                         LLVMOrcTargetAddress *RetAddr,
                                         const char *SymbolName) {
  OrcCBindingsStack &J = *unwrap(JITStack);
  return J.findSymbolAddress(*RetAddr, SymbolName, true);
}

LLVMOrcErrorCode LLVM_STDCALL LLVMOrcDisposeInstance(LLVMOrcJITStackRef JITStack) {
  auto *J = unwrap(JITStack);
  auto Err = J->shutdown();
  delete J;
  return Err;
}
