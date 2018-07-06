/*===-- llvm-c/ExecutionEngine.h - ExecutionEngine Lib C Iface --*- C++ -*-===*\
|*                                                                            *|
|*                     The LLVM Compiler Infrastructure                       *|
|*                                                                            *|
|* This file is distributed under the University of Illinois Open Source      *|
|* License. See LICENSE.TXT for details.                                      *|
|*                                                                            *|
|*===----------------------------------------------------------------------===*|
|*                                                                            *|
|* This header declares the C interface to libLLVMExecutionEngine.o, which    *|
|* implements various analyses of the LLVM IR.                                *|
|*                                                                            *|
|* Many exotic languages can interoperate with C code but have a harder time  *|
|* with C++ due to name mangling. So in addition to C, this interface enables *|
|* tools written in such languages.                                           *|
|*                                                                            *|
\*===----------------------------------------------------------------------===*/

#ifndef LLVM_C_EXECUTIONENGINE_H
#define LLVM_C_EXECUTIONENGINE_H

#include "llvm-c/Target.h"
#include "llvm-c/TargetMachine.h"
#include "llvm-c/Types.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @defgroup LLVMCExecutionEngine Execution Engine
 * @ingroup LLVMC
 *
 * @{
 */

void LLVM_STDCALL LLVMLinkInMCJIT(void);
void LLVM_STDCALL LLVMLinkInInterpreter(void);

typedef struct LLVMOpaqueGenericValue *LLVMGenericValueRef;
typedef struct LLVMOpaqueExecutionEngine *LLVMExecutionEngineRef;
typedef struct LLVMOpaqueMCJITMemoryManager *LLVMMCJITMemoryManagerRef;

struct LLVMMCJITCompilerOptions {
  unsigned OptLevel;
  LLVMCodeModel CodeModel;
  LLVMBool NoFramePointerElim;
  LLVMBool EnableFastISel;
  LLVMMCJITMemoryManagerRef MCJMM;
};

/*===-- Operations on generic values --------------------------------------===*/

LLVMGenericValueRef LLVM_STDCALL LLVMCreateGenericValueOfInt(LLVMTypeRef Ty,
                                                unsigned long long N,
                                                LLVMBool IsSigned);

LLVMGenericValueRef LLVM_STDCALL LLVMCreateGenericValueOfPointer(void *P);

LLVMGenericValueRef LLVM_STDCALL LLVMCreateGenericValueOfFloat(LLVMTypeRef Ty, double N);

unsigned LLVM_STDCALL LLVMGenericValueIntWidth(LLVMGenericValueRef GenValRef);

unsigned long long LLVM_STDCALL LLVMGenericValueToInt(LLVMGenericValueRef GenVal,
                                         LLVMBool IsSigned);

void *LLVM_STDCALL LLVMGenericValueToPointer(LLVMGenericValueRef GenVal);

double LLVM_STDCALL LLVMGenericValueToFloat(LLVMTypeRef TyRef, LLVMGenericValueRef GenVal);

void LLVM_STDCALL LLVMDisposeGenericValue(LLVMGenericValueRef GenVal);

/*===-- Operations on execution engines -----------------------------------===*/

LLVMBool LLVM_STDCALL LLVMCreateExecutionEngineForModule(LLVMExecutionEngineRef *OutEE,
                                            LLVMModuleRef M,
                                            char **OutError);

LLVMBool LLVM_STDCALL LLVMCreateInterpreterForModule(LLVMExecutionEngineRef *OutInterp,
                                        LLVMModuleRef M,
                                        char **OutError);

LLVMBool LLVM_STDCALL LLVMCreateJITCompilerForModule(LLVMExecutionEngineRef *OutJIT,
                                        LLVMModuleRef M,
                                        unsigned OptLevel,
                                        char **OutError);

void LLVM_STDCALL LLVMInitializeMCJITCompilerOptions(
  struct LLVMMCJITCompilerOptions *Options, size_t SizeOfOptions);

/**
 * Create an MCJIT execution engine for a module, with the given options. It is
 * the responsibility of the caller to ensure that all fields in Options up to
 * the given SizeOfOptions are initialized. It is correct to pass a smaller
 * value of SizeOfOptions that omits some fields. The canonical way of using
 * this is:
 *
 * LLVMMCJITCompilerOptions options;
 * LLVMInitializeMCJITCompilerOptions(&options, sizeof(options));
 * ... fill in those options you care about
 * LLVMCreateMCJITCompilerForModule(&jit, mod, &options, sizeof(options),
 *                                  &error);
 *
 * Note that this is also correct, though possibly suboptimal:
 *
 * LLVMCreateMCJITCompilerForModule(&jit, mod, 0, 0, &error);
 */
LLVMBool LLVM_STDCALL LLVMCreateMCJITCompilerForModule(
  LLVMExecutionEngineRef *OutJIT, LLVMModuleRef M,
  struct LLVMMCJITCompilerOptions *Options, size_t SizeOfOptions,
  char **OutError);

void LLVM_STDCALL LLVMDisposeExecutionEngine(LLVMExecutionEngineRef EE);

void LLVM_STDCALL LLVMRunStaticConstructors(LLVMExecutionEngineRef EE);

void LLVM_STDCALL LLVMRunStaticDestructors(LLVMExecutionEngineRef EE);

int LLVM_STDCALL LLVMRunFunctionAsMain(LLVMExecutionEngineRef EE, LLVMValueRef F,
                          unsigned ArgC, const char * const *ArgV,
                          const char * const *EnvP);

LLVMGenericValueRef LLVM_STDCALL LLVMRunFunction(LLVMExecutionEngineRef EE, LLVMValueRef F,
                                    unsigned NumArgs,
                                    LLVMGenericValueRef *Args);

void LLVM_STDCALL LLVMFreeMachineCodeForFunction(LLVMExecutionEngineRef EE, LLVMValueRef F);

void LLVM_STDCALL LLVMAddModule(LLVMExecutionEngineRef EE, LLVMModuleRef M);

LLVMBool LLVM_STDCALL LLVMRemoveModule(LLVMExecutionEngineRef EE, LLVMModuleRef M,
                          LLVMModuleRef *OutMod, char **OutError);

LLVMBool LLVM_STDCALL LLVMFindFunction(LLVMExecutionEngineRef EE, const char *Name,
                          LLVMValueRef *OutFn);

void *LLVM_STDCALL LLVMRecompileAndRelinkFunction(LLVMExecutionEngineRef EE,
                                     LLVMValueRef Fn);

LLVMTargetDataRef LLVM_STDCALL LLVMGetExecutionEngineTargetData(LLVMExecutionEngineRef EE);
LLVMTargetMachineRef LLVM_STDCALL 
LLVMGetExecutionEngineTargetMachine(LLVMExecutionEngineRef EE);

void LLVM_STDCALL LLVMAddGlobalMapping(LLVMExecutionEngineRef EE, LLVMValueRef Global,
                          void* Addr);

void *LLVM_STDCALL LLVMGetPointerToGlobal(LLVMExecutionEngineRef EE, LLVMValueRef Global);

uint64_t LLVM_STDCALL LLVMGetGlobalValueAddress(LLVMExecutionEngineRef EE, const char *Name);

uint64_t LLVM_STDCALL LLVMGetFunctionAddress(LLVMExecutionEngineRef EE, const char *Name);

/*===-- Operations on memory managers -------------------------------------===*/

typedef uint8_t *(LLVM_STDCALL *LLVMMemoryManagerAllocateCodeSectionCallback)(
  void *Opaque, uintptr_t Size, unsigned Alignment, unsigned SectionID,
  const char *SectionName);
typedef uint8_t *(LLVM_STDCALL *LLVMMemoryManagerAllocateDataSectionCallback)(
  void *Opaque, uintptr_t Size, unsigned Alignment, unsigned SectionID,
  const char *SectionName, LLVMBool IsReadOnly);
typedef LLVMBool (LLVM_STDCALL *LLVMMemoryManagerFinalizeMemoryCallback)(
  void *Opaque, char **ErrMsg);
typedef void (LLVM_STDCALL *LLVMMemoryManagerDestroyCallback)(void *Opaque);

/**
 * Create a simple custom MCJIT memory manager. This memory manager can
 * intercept allocations in a module-oblivious way. This will return NULL
 * if any of the passed functions are NULL.
 *
 * @param Opaque An opaque client object to pass back to the callbacks.
 * @param AllocateCodeSection Allocate a block of memory for executable code.
 * @param AllocateDataSection Allocate a block of memory for data.
 * @param FinalizeMemory Set page permissions and flush cache. Return 0 on
 *   success, 1 on error.
 */
LLVMMCJITMemoryManagerRef LLVM_STDCALL LLVMCreateSimpleMCJITMemoryManager(
  void *Opaque,
  LLVMMemoryManagerAllocateCodeSectionCallback AllocateCodeSection,
  LLVMMemoryManagerAllocateDataSectionCallback AllocateDataSection,
  LLVMMemoryManagerFinalizeMemoryCallback FinalizeMemory,
  LLVMMemoryManagerDestroyCallback Destroy);

void LLVM_STDCALL LLVMDisposeMCJITMemoryManager(LLVMMCJITMemoryManagerRef MM);

/**
 * @}
 */

#ifdef __cplusplus
}
#endif /* defined(__cplusplus) */

#endif
