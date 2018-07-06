/*===-- llvm-c/Object.h - Object Lib C Iface --------------------*- C++ -*-===*/
/*                                                                            */
/*                     The LLVM Compiler Infrastructure                       */
/*                                                                            */
/* This file is distributed under the University of Illinois Open Source      */
/* License. See LICENSE.TXT for details.                                      */
/*                                                                            */
/*===----------------------------------------------------------------------===*/
/*                                                                            */
/* This header declares the C interface to libLLVMObject.a, which             */
/* implements object file reading and writing.                                */
/*                                                                            */
/* Many exotic languages can interoperate with C code but have a harder time  */
/* with C++ due to name mangling. So in addition to C, this interface enables */
/* tools written in such languages.                                           */
/*                                                                            */
/*===----------------------------------------------------------------------===*/

#ifndef LLVM_C_OBJECT_H
#define LLVM_C_OBJECT_H

#include "llvm-c/Types.h"
#include "llvm/Config/llvm-config.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @defgroup LLVMCObject Object file reading and writing
 * @ingroup LLVMC
 *
 * @{
 */

// Opaque type wrappers
typedef struct LLVMOpaqueObjectFile *LLVMObjectFileRef;
typedef struct LLVMOpaqueSectionIterator *LLVMSectionIteratorRef;
typedef struct LLVMOpaqueSymbolIterator *LLVMSymbolIteratorRef;
typedef struct LLVMOpaqueRelocationIterator *LLVMRelocationIteratorRef;

// ObjectFile creation
LLVMObjectFileRef LLVM_STDCALL LLVMCreateObjectFile(LLVMMemoryBufferRef MemBuf);
void LLVM_STDCALL LLVMDisposeObjectFile(LLVMObjectFileRef ObjectFile);

// ObjectFile Section iterators
LLVMSectionIteratorRef LLVM_STDCALL LLVMGetSections(LLVMObjectFileRef ObjectFile);
void LLVM_STDCALL LLVMDisposeSectionIterator(LLVMSectionIteratorRef SI);
LLVMBool LLVM_STDCALL LLVMIsSectionIteratorAtEnd(LLVMObjectFileRef ObjectFile,
                                LLVMSectionIteratorRef SI);
void LLVM_STDCALL LLVMMoveToNextSection(LLVMSectionIteratorRef SI);
void LLVM_STDCALL LLVMMoveToContainingSection(LLVMSectionIteratorRef Sect,
                                 LLVMSymbolIteratorRef Sym);

// ObjectFile Symbol iterators
LLVMSymbolIteratorRef LLVM_STDCALL LLVMGetSymbols(LLVMObjectFileRef ObjectFile);
void LLVM_STDCALL LLVMDisposeSymbolIterator(LLVMSymbolIteratorRef SI);
LLVMBool LLVM_STDCALL LLVMIsSymbolIteratorAtEnd(LLVMObjectFileRef ObjectFile,
                                LLVMSymbolIteratorRef SI);
void LLVM_STDCALL LLVMMoveToNextSymbol(LLVMSymbolIteratorRef SI);

// SectionRef accessors
const char *LLVM_STDCALL LLVMGetSectionName(LLVMSectionIteratorRef SI);
uint64_t LLVM_STDCALL LLVMGetSectionSize(LLVMSectionIteratorRef SI);
const char *LLVM_STDCALL LLVMGetSectionContents(LLVMSectionIteratorRef SI);
uint64_t LLVM_STDCALL LLVMGetSectionAddress(LLVMSectionIteratorRef SI);
LLVMBool LLVM_STDCALL LLVMGetSectionContainsSymbol(LLVMSectionIteratorRef SI,
                                 LLVMSymbolIteratorRef Sym);

// Section Relocation iterators
LLVMRelocationIteratorRef LLVM_STDCALL LLVMGetRelocations(LLVMSectionIteratorRef Section);
void LLVM_STDCALL LLVMDisposeRelocationIterator(LLVMRelocationIteratorRef RI);
LLVMBool LLVM_STDCALL LLVMIsRelocationIteratorAtEnd(LLVMSectionIteratorRef Section,
                                       LLVMRelocationIteratorRef RI);
void LLVM_STDCALL LLVMMoveToNextRelocation(LLVMRelocationIteratorRef RI);


// SymbolRef accessors
const char *LLVM_STDCALL LLVMGetSymbolName(LLVMSymbolIteratorRef SI);
uint64_t LLVM_STDCALL LLVMGetSymbolAddress(LLVMSymbolIteratorRef SI);
uint64_t LLVM_STDCALL LLVMGetSymbolSize(LLVMSymbolIteratorRef SI);

// RelocationRef accessors
uint64_t LLVM_STDCALL LLVMGetRelocationOffset(LLVMRelocationIteratorRef RI);
LLVMSymbolIteratorRef LLVM_STDCALL LLVMGetRelocationSymbol(LLVMRelocationIteratorRef RI);
uint64_t LLVM_STDCALL LLVMGetRelocationType(LLVMRelocationIteratorRef RI);
// NOTE: Caller takes ownership of returned string of the two
// following functions.
const char *LLVM_STDCALL LLVMGetRelocationTypeName(LLVMRelocationIteratorRef RI);
const char *LLVM_STDCALL LLVMGetRelocationValueString(LLVMRelocationIteratorRef RI);

/**
 * @}
 */

#ifdef __cplusplus
}
#endif /* defined(__cplusplus) */

#endif
