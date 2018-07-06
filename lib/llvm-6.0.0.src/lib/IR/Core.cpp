//===-- Core.cpp ----------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the common infrastructure (including the C bindings)
// for libLLVMCore.a, which implements the LLVM intermediate representation.
//
//===----------------------------------------------------------------------===//

#include "llvm-c/Core.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/DiagnosticPrinter.h"
#include "llvm/IR/GlobalAlias.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Threading.h"
#include "llvm/Support/raw_ostream.h"
#include <cassert>
#include <cstdlib>
#include <cstring>
#include <system_error>

using namespace llvm;

#define DEBUG_TYPE "ir"

void llvm::initializeCore(PassRegistry &Registry) {
  initializeDominatorTreeWrapperPassPass(Registry);
  initializePrintModulePassWrapperPass(Registry);
  initializePrintFunctionPassWrapperPass(Registry);
  initializePrintBasicBlockPassPass(Registry);
  initializeSafepointIRVerifierPass(Registry);
  initializeVerifierLegacyPassPass(Registry);
}

void LLVM_STDCALL LLVMInitializeCore(LLVMPassRegistryRef R) {
  initializeCore(*unwrap(R));
}

void LLVM_STDCALL LLVMShutdown() {
  llvm_shutdown();
}

/*===-- Error handling ----------------------------------------------------===*/

char *LLVM_STDCALL LLVMCreateMessage(const char *Message) {
  return strdup(Message);
}

void LLVM_STDCALL LLVMDisposeMessage(char *Message) {
  free(Message);
}


/*===-- Operations on contexts --------------------------------------------===*/

static ManagedStatic<LLVMContext> GlobalContext;

LLVMContextRef LLVM_STDCALL LLVMContextCreate() {
  return wrap(new LLVMContext());
}

LLVMContextRef LLVM_STDCALL LLVMGetGlobalContext() { return wrap(&*GlobalContext); }

void LLVM_STDCALL LLVMContextSetDiagnosticHandler(LLVMContextRef C,
                                     LLVMDiagnosticHandler Handler,
                                     void *DiagnosticContext) {
  unwrap(C)->setDiagnosticHandlerCallBack(
      LLVM_EXTENSION reinterpret_cast<DiagnosticHandler::DiagnosticHandlerTy>(
          Handler),
      DiagnosticContext);
}

LLVMDiagnosticHandler LLVM_STDCALL LLVMContextGetDiagnosticHandler(LLVMContextRef C) {
  return LLVM_EXTENSION reinterpret_cast<LLVMDiagnosticHandler>(
      unwrap(C)->getDiagnosticHandlerCallBack());
}

void *LLVM_STDCALL LLVMContextGetDiagnosticContext(LLVMContextRef C) {
  return unwrap(C)->getDiagnosticContext();
}

void LLVM_STDCALL LLVMContextSetYieldCallback(LLVMContextRef C, LLVMYieldCallback Callback,
                                 void *OpaqueHandle) {
  auto YieldCallback =
    LLVM_EXTENSION reinterpret_cast<LLVMContext::YieldCallbackTy>(Callback);
  unwrap(C)->setYieldCallback(YieldCallback, OpaqueHandle);
}

void LLVM_STDCALL LLVMContextDispose(LLVMContextRef C) {
  delete unwrap(C);
}

unsigned LLVM_STDCALL LLVMGetMDKindIDInContext(LLVMContextRef C, const char *Name,
                                  unsigned SLen) {
  return unwrap(C)->getMDKindID(StringRef(Name, SLen));
}

unsigned LLVM_STDCALL LLVMGetMDKindID(const char *Name, unsigned SLen) {
  return LLVMGetMDKindIDInContext(LLVMGetGlobalContext(), Name, SLen);
}

#define GET_ATTR_KIND_FROM_NAME
#include "AttributesCompatFunc.inc"

unsigned LLVM_STDCALL LLVMGetEnumAttributeKindForName(const char *Name, size_t SLen) {
  return getAttrKindFromName(StringRef(Name, SLen));
}

unsigned LLVM_STDCALL LLVMGetLastEnumAttributeKind(void) {
  return Attribute::AttrKind::EndAttrKinds;
}

LLVMAttributeRef LLVM_STDCALL LLVMCreateEnumAttribute(LLVMContextRef C, unsigned KindID,
                                         uint64_t Val) {
  return wrap(Attribute::get(*unwrap(C), (Attribute::AttrKind)KindID, Val));
}

unsigned LLVM_STDCALL LLVMGetEnumAttributeKind(LLVMAttributeRef A) {
  return unwrap(A).getKindAsEnum();
}

uint64_t LLVM_STDCALL LLVMGetEnumAttributeValue(LLVMAttributeRef A) {
  auto Attr = unwrap(A);
  if (Attr.isEnumAttribute())
    return 0;
  return Attr.getValueAsInt();
}

LLVMAttributeRef LLVM_STDCALL LLVMCreateStringAttribute(LLVMContextRef C,
                                           const char *K, unsigned KLength,
                                           const char *V, unsigned VLength) {
  return wrap(Attribute::get(*unwrap(C), StringRef(K, KLength),
                             StringRef(V, VLength)));
}

const char *LLVM_STDCALL LLVMGetStringAttributeKind(LLVMAttributeRef A,
                                       unsigned *Length) {
  auto S = unwrap(A).getKindAsString();
  *Length = S.size();
  return S.data();
}

const char *LLVM_STDCALL LLVMGetStringAttributeValue(LLVMAttributeRef A,
                                        unsigned *Length) {
  auto S = unwrap(A).getValueAsString();
  *Length = S.size();
  return S.data();
}

LLVMBool LLVM_STDCALL LLVMIsEnumAttribute(LLVMAttributeRef A) {
  auto Attr = unwrap(A);
  return Attr.isEnumAttribute() || Attr.isIntAttribute();
}

LLVMBool LLVM_STDCALL LLVMIsStringAttribute(LLVMAttributeRef A) {
  return unwrap(A).isStringAttribute();
}

char *LLVM_STDCALL LLVMGetDiagInfoDescription(LLVMDiagnosticInfoRef DI) {
  std::string MsgStorage;
  raw_string_ostream Stream(MsgStorage);
  DiagnosticPrinterRawOStream DP(Stream);

  unwrap(DI)->print(DP);
  Stream.flush();

  return LLVMCreateMessage(MsgStorage.c_str());
}

LLVMDiagnosticSeverity LLVM_STDCALL LLVMGetDiagInfoSeverity(LLVMDiagnosticInfoRef DI) {
    LLVMDiagnosticSeverity severity;

    switch(unwrap(DI)->getSeverity()) {
    default:
      severity = LLVMDSError;
      break;
    case DS_Warning:
      severity = LLVMDSWarning;
      break;
    case DS_Remark:
      severity = LLVMDSRemark;
      break;
    case DS_Note:
      severity = LLVMDSNote;
      break;
    }

    return severity;
}

/*===-- Operations on modules ---------------------------------------------===*/

LLVMModuleRef LLVM_STDCALL LLVMModuleCreateWithName(const char *ModuleID) {
  return wrap(new Module(ModuleID, *GlobalContext));
}

LLVMModuleRef LLVM_STDCALL LLVMModuleCreateWithNameInContext(const char *ModuleID,
                                                LLVMContextRef C) {
  return wrap(new Module(ModuleID, *unwrap(C)));
}

void LLVM_STDCALL LLVMDisposeModule(LLVMModuleRef M) {
  delete unwrap(M);
}

const char *LLVM_STDCALL LLVMGetModuleIdentifier(LLVMModuleRef M, size_t *Len) {
  auto &Str = unwrap(M)->getModuleIdentifier();
  *Len = Str.length();
  return Str.c_str();
}

void LLVM_STDCALL LLVMSetModuleIdentifier(LLVMModuleRef M, const char *Ident, size_t Len) {
  unwrap(M)->setModuleIdentifier(StringRef(Ident, Len));
}


/*--.. Data layout .........................................................--*/
const char *LLVM_STDCALL LLVMGetDataLayoutStr(LLVMModuleRef M) {
  return unwrap(M)->getDataLayoutStr().c_str();
}

const char *LLVM_STDCALL LLVMGetDataLayout(LLVMModuleRef M) {
  return LLVMGetDataLayoutStr(M);
}

void LLVM_STDCALL LLVMSetDataLayout(LLVMModuleRef M, const char *DataLayoutStr) {
  unwrap(M)->setDataLayout(DataLayoutStr);
}

/*--.. Target triple .......................................................--*/
const char * LLVM_STDCALL LLVMGetTarget(LLVMModuleRef M) {
  return unwrap(M)->getTargetTriple().c_str();
}

void LLVM_STDCALL LLVMSetTarget(LLVMModuleRef M, const char *Triple) {
  unwrap(M)->setTargetTriple(Triple);
}

void LLVM_STDCALL LLVMDumpModule(LLVMModuleRef M) {
  unwrap(M)->print(errs(), nullptr,
                   /*ShouldPreserveUseListOrder=*/false, /*IsForDebug=*/true);
}

LLVMBool LLVM_STDCALL LLVMPrintModuleToFile(LLVMModuleRef M, const char *Filename,
                               char **ErrorMessage) {
  std::error_code EC;
  raw_fd_ostream dest(Filename, EC, sys::fs::F_Text);
  if (EC) {
    *ErrorMessage = strdup(EC.message().c_str());
    return true;
  }

  unwrap(M)->print(dest, nullptr);

  dest.close();

  if (dest.has_error()) {
    std::string E = "Error printing to file: " + dest.error().message();
    *ErrorMessage = strdup(E.c_str());
    return true;
  }

  return false;
}

char *LLVM_STDCALL LLVMPrintModuleToString(LLVMModuleRef M) {
  std::string buf;
  raw_string_ostream os(buf);

  unwrap(M)->print(os, nullptr);
  os.flush();

  return strdup(buf.c_str());
}

/*--.. Operations on inline assembler ......................................--*/
void LLVM_STDCALL LLVMSetModuleInlineAsm(LLVMModuleRef M, const char *Asm) {
  unwrap(M)->setModuleInlineAsm(StringRef(Asm));
}


/*--.. Operations on module contexts ......................................--*/
LLVMContextRef LLVM_STDCALL LLVMGetModuleContext(LLVMModuleRef M) {
  return wrap(&unwrap(M)->getContext());
}


/*===-- Operations on types -----------------------------------------------===*/

/*--.. Operations on all types (mostly) ....................................--*/

LLVMTypeKind LLVM_STDCALL LLVMGetTypeKind(LLVMTypeRef Ty) {
  switch (unwrap(Ty)->getTypeID()) {
  case Type::VoidTyID:
    return LLVMVoidTypeKind;
  case Type::HalfTyID:
    return LLVMHalfTypeKind;
  case Type::FloatTyID:
    return LLVMFloatTypeKind;
  case Type::DoubleTyID:
    return LLVMDoubleTypeKind;
  case Type::X86_FP80TyID:
    return LLVMX86_FP80TypeKind;
  case Type::FP128TyID:
    return LLVMFP128TypeKind;
  case Type::PPC_FP128TyID:
    return LLVMPPC_FP128TypeKind;
  case Type::LabelTyID:
    return LLVMLabelTypeKind;
  case Type::MetadataTyID:
    return LLVMMetadataTypeKind;
  case Type::IntegerTyID:
    return LLVMIntegerTypeKind;
  case Type::FunctionTyID:
    return LLVMFunctionTypeKind;
  case Type::StructTyID:
    return LLVMStructTypeKind;
  case Type::ArrayTyID:
    return LLVMArrayTypeKind;
  case Type::PointerTyID:
    return LLVMPointerTypeKind;
  case Type::VectorTyID:
    return LLVMVectorTypeKind;
  case Type::X86_MMXTyID:
    return LLVMX86_MMXTypeKind;
  case Type::TokenTyID:
    return LLVMTokenTypeKind;
  }
  llvm_unreachable("Unhandled TypeID.");
}

LLVMBool LLVM_STDCALL LLVMTypeIsSized(LLVMTypeRef Ty)
{
    return unwrap(Ty)->isSized();
}

LLVMContextRef LLVM_STDCALL LLVMGetTypeContext(LLVMTypeRef Ty) {
  return wrap(&unwrap(Ty)->getContext());
}

#if !defined(NDEBUG) || defined(LLVM_ENABLE_DUMP)
LLVM_DUMP_METHOD void LLVM_STDCALL LLVMDumpType(LLVMTypeRef Ty) {
  return unwrap(Ty)->dump();
}
#endif

char *LLVM_STDCALL LLVMPrintTypeToString(LLVMTypeRef Ty) {
  std::string buf;
  raw_string_ostream os(buf);

  if (unwrap(Ty))
    unwrap(Ty)->print(os);
  else
    os << "Printing <null> Type";

  os.flush();

  return strdup(buf.c_str());
}

/*--.. Operations on integer types .........................................--*/

LLVMTypeRef LLVM_STDCALL LLVMInt1TypeInContext(LLVMContextRef C)  {
  return (LLVMTypeRef) Type::getInt1Ty(*unwrap(C));
}
LLVMTypeRef LLVM_STDCALL LLVMInt8TypeInContext(LLVMContextRef C)  {
  return (LLVMTypeRef) Type::getInt8Ty(*unwrap(C));
}
LLVMTypeRef LLVM_STDCALL LLVMInt16TypeInContext(LLVMContextRef C) {
  return (LLVMTypeRef) Type::getInt16Ty(*unwrap(C));
}
LLVMTypeRef LLVM_STDCALL LLVMInt32TypeInContext(LLVMContextRef C) {
  return (LLVMTypeRef) Type::getInt32Ty(*unwrap(C));
}
LLVMTypeRef LLVM_STDCALL LLVMInt64TypeInContext(LLVMContextRef C) {
  return (LLVMTypeRef) Type::getInt64Ty(*unwrap(C));
}
LLVMTypeRef LLVM_STDCALL LLVMInt128TypeInContext(LLVMContextRef C) {
  return (LLVMTypeRef) Type::getInt128Ty(*unwrap(C));
}
LLVMTypeRef LLVM_STDCALL LLVMIntTypeInContext(LLVMContextRef C, unsigned NumBits) {
  return wrap(IntegerType::get(*unwrap(C), NumBits));
}

LLVMTypeRef LLVM_STDCALL LLVMInt1Type(void)  {
  return LLVMInt1TypeInContext(LLVMGetGlobalContext());
}
LLVMTypeRef LLVM_STDCALL LLVMInt8Type(void)  {
  return LLVMInt8TypeInContext(LLVMGetGlobalContext());
}
LLVMTypeRef LLVM_STDCALL LLVMInt16Type(void) {
  return LLVMInt16TypeInContext(LLVMGetGlobalContext());
}
LLVMTypeRef LLVM_STDCALL LLVMInt32Type(void) {
  return LLVMInt32TypeInContext(LLVMGetGlobalContext());
}
LLVMTypeRef LLVM_STDCALL LLVMInt64Type(void) {
  return LLVMInt64TypeInContext(LLVMGetGlobalContext());
}
LLVMTypeRef LLVM_STDCALL LLVMInt128Type(void) {
  return LLVMInt128TypeInContext(LLVMGetGlobalContext());
}
LLVMTypeRef LLVM_STDCALL LLVMIntType(unsigned NumBits) {
  return LLVMIntTypeInContext(LLVMGetGlobalContext(), NumBits);
}

unsigned LLVM_STDCALL LLVMGetIntTypeWidth(LLVMTypeRef IntegerTy) {
  return unwrap<IntegerType>(IntegerTy)->getBitWidth();
}

/*--.. Operations on real types ............................................--*/

LLVMTypeRef LLVM_STDCALL LLVMHalfTypeInContext(LLVMContextRef C) {
  return (LLVMTypeRef) Type::getHalfTy(*unwrap(C));
}
LLVMTypeRef LLVM_STDCALL LLVMFloatTypeInContext(LLVMContextRef C) {
  return (LLVMTypeRef) Type::getFloatTy(*unwrap(C));
}
LLVMTypeRef LLVM_STDCALL LLVMDoubleTypeInContext(LLVMContextRef C) {
  return (LLVMTypeRef) Type::getDoubleTy(*unwrap(C));
}
LLVMTypeRef LLVM_STDCALL LLVMX86FP80TypeInContext(LLVMContextRef C) {
  return (LLVMTypeRef) Type::getX86_FP80Ty(*unwrap(C));
}
LLVMTypeRef LLVM_STDCALL LLVMFP128TypeInContext(LLVMContextRef C) {
  return (LLVMTypeRef) Type::getFP128Ty(*unwrap(C));
}
LLVMTypeRef LLVM_STDCALL LLVMPPCFP128TypeInContext(LLVMContextRef C) {
  return (LLVMTypeRef) Type::getPPC_FP128Ty(*unwrap(C));
}
LLVMTypeRef LLVM_STDCALL LLVMX86MMXTypeInContext(LLVMContextRef C) {
  return (LLVMTypeRef) Type::getX86_MMXTy(*unwrap(C));
}

LLVMTypeRef LLVM_STDCALL LLVMHalfType(void) {
  return LLVMHalfTypeInContext(LLVMGetGlobalContext());
}
LLVMTypeRef LLVM_STDCALL LLVMFloatType(void) {
  return LLVMFloatTypeInContext(LLVMGetGlobalContext());
}
LLVMTypeRef LLVM_STDCALL LLVMDoubleType(void) {
  return LLVMDoubleTypeInContext(LLVMGetGlobalContext());
}
LLVMTypeRef LLVM_STDCALL LLVMX86FP80Type(void) {
  return LLVMX86FP80TypeInContext(LLVMGetGlobalContext());
}
LLVMTypeRef LLVM_STDCALL LLVMFP128Type(void) {
  return LLVMFP128TypeInContext(LLVMGetGlobalContext());
}
LLVMTypeRef LLVM_STDCALL LLVMPPCFP128Type(void) {
  return LLVMPPCFP128TypeInContext(LLVMGetGlobalContext());
}
LLVMTypeRef LLVM_STDCALL LLVMX86MMXType(void) {
  return LLVMX86MMXTypeInContext(LLVMGetGlobalContext());
}

/*--.. Operations on function types ........................................--*/

LLVMTypeRef LLVM_STDCALL LLVMFunctionType(LLVMTypeRef ReturnType,
                             LLVMTypeRef *ParamTypes, unsigned ParamCount,
                             LLVMBool IsVarArg) {
  ArrayRef<Type*> Tys(unwrap(ParamTypes), ParamCount);
  return wrap(FunctionType::get(unwrap(ReturnType), Tys, IsVarArg != 0));
}

LLVMBool LLVM_STDCALL LLVMIsFunctionVarArg(LLVMTypeRef FunctionTy) {
  return unwrap<FunctionType>(FunctionTy)->isVarArg();
}

LLVMTypeRef LLVM_STDCALL LLVMGetReturnType(LLVMTypeRef FunctionTy) {
  return wrap(unwrap<FunctionType>(FunctionTy)->getReturnType());
}

unsigned LLVM_STDCALL LLVMCountParamTypes(LLVMTypeRef FunctionTy) {
  return unwrap<FunctionType>(FunctionTy)->getNumParams();
}

void LLVM_STDCALL LLVMGetParamTypes(LLVMTypeRef FunctionTy, LLVMTypeRef *Dest) {
  FunctionType *Ty = unwrap<FunctionType>(FunctionTy);
  for (FunctionType::param_iterator I = Ty->param_begin(),
                                    E = Ty->param_end(); I != E; ++I)
    *Dest++ = wrap(*I);
}

/*--.. Operations on struct types ..........................................--*/

LLVMTypeRef LLVM_STDCALL LLVMStructTypeInContext(LLVMContextRef C, LLVMTypeRef *ElementTypes,
                           unsigned ElementCount, LLVMBool Packed) {
  ArrayRef<Type*> Tys(unwrap(ElementTypes), ElementCount);
  return wrap(StructType::get(*unwrap(C), Tys, Packed != 0));
}

LLVMTypeRef LLVM_STDCALL LLVMStructType(LLVMTypeRef *ElementTypes,
                           unsigned ElementCount, LLVMBool Packed) {
  return LLVMStructTypeInContext(LLVMGetGlobalContext(), ElementTypes,
                                 ElementCount, Packed);
}

LLVMTypeRef LLVM_STDCALL LLVMStructCreateNamed(LLVMContextRef C, const char *Name)
{
  return wrap(StructType::create(*unwrap(C), Name));
}

const char *LLVM_STDCALL LLVMGetStructName(LLVMTypeRef Ty)
{
  StructType *Type = unwrap<StructType>(Ty);
  if (!Type->hasName())
    return nullptr;
  return Type->getName().data();
}

void LLVM_STDCALL LLVMStructSetBody(LLVMTypeRef StructTy, LLVMTypeRef *ElementTypes,
                       unsigned ElementCount, LLVMBool Packed) {
  ArrayRef<Type*> Tys(unwrap(ElementTypes), ElementCount);
  unwrap<StructType>(StructTy)->setBody(Tys, Packed != 0);
}

unsigned LLVM_STDCALL LLVMCountStructElementTypes(LLVMTypeRef StructTy) {
  return unwrap<StructType>(StructTy)->getNumElements();
}

void LLVM_STDCALL LLVMGetStructElementTypes(LLVMTypeRef StructTy, LLVMTypeRef *Dest) {
  StructType *Ty = unwrap<StructType>(StructTy);
  for (StructType::element_iterator I = Ty->element_begin(),
                                    E = Ty->element_end(); I != E; ++I)
    *Dest++ = wrap(*I);
}

LLVMTypeRef LLVM_STDCALL LLVMStructGetTypeAtIndex(LLVMTypeRef StructTy, unsigned i) {
  StructType *Ty = unwrap<StructType>(StructTy);
  return wrap(Ty->getTypeAtIndex(i));
}

LLVMBool LLVM_STDCALL LLVMIsPackedStruct(LLVMTypeRef StructTy) {
  return unwrap<StructType>(StructTy)->isPacked();
}

LLVMBool LLVM_STDCALL LLVMIsOpaqueStruct(LLVMTypeRef StructTy) {
  return unwrap<StructType>(StructTy)->isOpaque();
}

LLVMTypeRef LLVM_STDCALL LLVMGetTypeByName(LLVMModuleRef M, const char *Name) {
  return wrap(unwrap(M)->getTypeByName(Name));
}

/*--.. Operations on array, pointer, and vector types (sequence types) .....--*/

void LLVM_STDCALL LLVMGetSubtypes(LLVMTypeRef Tp, LLVMTypeRef *Arr) {
    int i = 0;
    for (auto *T : unwrap(Tp)->subtypes()) {
        Arr[i] = wrap(T);
        i++;
    }
}

LLVMTypeRef LLVM_STDCALL LLVMArrayType(LLVMTypeRef ElementType, unsigned ElementCount) {
  return wrap(ArrayType::get(unwrap(ElementType), ElementCount));
}

LLVMTypeRef LLVM_STDCALL LLVMPointerType(LLVMTypeRef ElementType, unsigned AddressSpace) {
  return wrap(PointerType::get(unwrap(ElementType), AddressSpace));
}

LLVMTypeRef LLVM_STDCALL LLVMVectorType(LLVMTypeRef ElementType, unsigned ElementCount) {
  return wrap(VectorType::get(unwrap(ElementType), ElementCount));
}

LLVMTypeRef LLVM_STDCALL LLVMGetElementType(LLVMTypeRef WrappedTy) {
  auto *Ty = unwrap<Type>(WrappedTy);
  if (auto *PTy = dyn_cast<PointerType>(Ty))
    return wrap(PTy->getElementType());
  return wrap(cast<SequentialType>(Ty)->getElementType());
}

unsigned LLVM_STDCALL LLVMGetNumContainedTypes(LLVMTypeRef Tp) {
    return unwrap(Tp)->getNumContainedTypes();
}

unsigned LLVM_STDCALL LLVMGetArrayLength(LLVMTypeRef ArrayTy) {
  return unwrap<ArrayType>(ArrayTy)->getNumElements();
}

unsigned LLVM_STDCALL LLVMGetPointerAddressSpace(LLVMTypeRef PointerTy) {
  return unwrap<PointerType>(PointerTy)->getAddressSpace();
}

unsigned LLVM_STDCALL LLVMGetVectorSize(LLVMTypeRef VectorTy) {
  return unwrap<VectorType>(VectorTy)->getNumElements();
}

/*--.. Operations on other types ...........................................--*/

LLVMTypeRef LLVM_STDCALL LLVMVoidTypeInContext(LLVMContextRef C)  {
  return wrap(Type::getVoidTy(*unwrap(C)));
}
LLVMTypeRef LLVM_STDCALL LLVMLabelTypeInContext(LLVMContextRef C) {
  return wrap(Type::getLabelTy(*unwrap(C)));
}
LLVMTypeRef LLVM_STDCALL LLVMTokenTypeInContext(LLVMContextRef C) {
  return wrap(Type::getTokenTy(*unwrap(C)));
}
LLVMTypeRef LLVM_STDCALL LLVMMetadataTypeInContext(LLVMContextRef C) {
  return wrap(Type::getMetadataTy(*unwrap(C)));
}

LLVMTypeRef LLVM_STDCALL LLVMVoidType(void)  {
  return LLVMVoidTypeInContext(LLVMGetGlobalContext());
}
LLVMTypeRef LLVM_STDCALL LLVMLabelType(void) {
  return LLVMLabelTypeInContext(LLVMGetGlobalContext());
}

/*===-- Operations on values ----------------------------------------------===*/

/*--.. Operations on all values ............................................--*/

LLVMTypeRef LLVM_STDCALL LLVMTypeOf(LLVMValueRef Val) {
  return wrap(unwrap(Val)->getType());
}

LLVMValueKind LLVM_STDCALL LLVMGetValueKind(LLVMValueRef Val) {
    switch(unwrap(Val)->getValueID()) {
#define HANDLE_VALUE(Name) \
  case Value::Name##Val: \
    return LLVM##Name##ValueKind;
#include "llvm/IR/Value.def"
  default:
    return LLVMInstructionValueKind;
  }
}

const char *LLVM_STDCALL LLVMGetValueName(LLVMValueRef Val) {
  return unwrap(Val)->getName().data();
}

void LLVM_STDCALL LLVMSetValueName(LLVMValueRef Val, const char *Name) {
  unwrap(Val)->setName(Name);
}

LLVM_DUMP_METHOD void LLVM_STDCALL LLVMDumpValue(LLVMValueRef Val) {
  unwrap(Val)->print(errs(), /*IsForDebug=*/true);
}

char* LLVM_STDCALL LLVMPrintValueToString(LLVMValueRef Val) {
  std::string buf;
  raw_string_ostream os(buf);

  if (unwrap(Val))
    unwrap(Val)->print(os);
  else
    os << "Printing <null> Value";

  os.flush();

  return strdup(buf.c_str());
}

void LLVM_STDCALL LLVMReplaceAllUsesWith(LLVMValueRef OldVal, LLVMValueRef NewVal) {
  unwrap(OldVal)->replaceAllUsesWith(unwrap(NewVal));
}

int LLVM_STDCALL LLVMHasMetadata(LLVMValueRef Inst) {
  return unwrap<Instruction>(Inst)->hasMetadata();
}

LLVMValueRef LLVM_STDCALL LLVMGetMetadata(LLVMValueRef Inst, unsigned KindID) {
  auto *I = unwrap<Instruction>(Inst);
  assert(I && "Expected instruction");
  if (auto *MD = I->getMetadata(KindID))
    return wrap(MetadataAsValue::get(I->getContext(), MD));
  return nullptr;
}

// MetadataAsValue uses a canonical format which strips the actual MDNode for
// MDNode with just a single constant value, storing just a ConstantAsMetadata
// This undoes this canonicalization, reconstructing the MDNode.
static MDNode *extractMDNode(MetadataAsValue *MAV) {
  Metadata *MD = MAV->getMetadata();
  assert((isa<MDNode>(MD) || isa<ConstantAsMetadata>(MD)) &&
      "Expected a metadata node or a canonicalized constant");

  if (MDNode *N = dyn_cast<MDNode>(MD))
    return N;

  return MDNode::get(MAV->getContext(), MD);
}

void LLVM_STDCALL LLVMSetMetadata(LLVMValueRef Inst, unsigned KindID, LLVMValueRef Val) {
  MDNode *N = Val ? extractMDNode(unwrap<MetadataAsValue>(Val)) : nullptr;

  unwrap<Instruction>(Inst)->setMetadata(KindID, N);
}

/*--.. Conversion functions ................................................--*/

#define LLVM_DEFINE_VALUE_CAST(name)                                       \
  LLVMValueRef LLVM_STDCALL LLVMIsA##name(LLVMValueRef Val) {                           \
    return wrap(static_cast<Value*>(dyn_cast_or_null<name>(unwrap(Val)))); \
  }

LLVM_FOR_EACH_VALUE_SUBCLASS(LLVM_DEFINE_VALUE_CAST)

LLVMValueRef LLVM_STDCALL LLVMIsAMDNode(LLVMValueRef Val) {
  if (auto *MD = dyn_cast_or_null<MetadataAsValue>(unwrap(Val)))
    if (isa<MDNode>(MD->getMetadata()) ||
        isa<ValueAsMetadata>(MD->getMetadata()))
      return Val;
  return nullptr;
}

LLVMValueRef LLVM_STDCALL LLVMIsAMDString(LLVMValueRef Val) {
  if (auto *MD = dyn_cast_or_null<MetadataAsValue>(unwrap(Val)))
    if (isa<MDString>(MD->getMetadata()))
      return Val;
  return nullptr;
}

/*--.. Operations on Uses ..................................................--*/
LLVMUseRef LLVM_STDCALL LLVMGetFirstUse(LLVMValueRef Val) {
  Value *V = unwrap(Val);
  Value::use_iterator I = V->use_begin();
  if (I == V->use_end())
    return nullptr;
  return wrap(&*I);
}

LLVMUseRef LLVM_STDCALL LLVMGetNextUse(LLVMUseRef U) {
  Use *Next = unwrap(U)->getNext();
  if (Next)
    return wrap(Next);
  return nullptr;
}

LLVMValueRef LLVM_STDCALL LLVMGetUser(LLVMUseRef U) {
  return wrap(unwrap(U)->getUser());
}

LLVMValueRef LLVM_STDCALL LLVMGetUsedValue(LLVMUseRef U) {
  return wrap(unwrap(U)->get());
}

/*--.. Operations on Users .................................................--*/

static LLVMValueRef getMDNodeOperandImpl(LLVMContext &Context, const MDNode *N,
                                         unsigned Index) {
  Metadata *Op = N->getOperand(Index);
  if (!Op)
    return nullptr;
  if (auto *C = dyn_cast<ConstantAsMetadata>(Op))
    return wrap(C->getValue());
  return wrap(MetadataAsValue::get(Context, Op));
}

LLVMValueRef LLVM_STDCALL LLVMGetOperand(LLVMValueRef Val, unsigned Index) {
  Value *V = unwrap(Val);
  if (auto *MD = dyn_cast<MetadataAsValue>(V)) {
    if (auto *L = dyn_cast<ValueAsMetadata>(MD->getMetadata())) {
      assert(Index == 0 && "Function-local metadata can only have one operand");
      return wrap(L->getValue());
    }
    return getMDNodeOperandImpl(V->getContext(),
                                cast<MDNode>(MD->getMetadata()), Index);
  }

  return wrap(cast<User>(V)->getOperand(Index));
}

LLVMUseRef LLVM_STDCALL LLVMGetOperandUse(LLVMValueRef Val, unsigned Index) {
  Value *V = unwrap(Val);
  return wrap(&cast<User>(V)->getOperandUse(Index));
}

void LLVM_STDCALL LLVMSetOperand(LLVMValueRef Val, unsigned Index, LLVMValueRef Op) {
  unwrap<User>(Val)->setOperand(Index, unwrap(Op));
}

int LLVM_STDCALL LLVMGetNumOperands(LLVMValueRef Val) {
  Value *V = unwrap(Val);
  if (isa<MetadataAsValue>(V))
    return LLVMGetMDNodeNumOperands(Val);

  return cast<User>(V)->getNumOperands();
}

/*--.. Operations on constants of any type .................................--*/

LLVMValueRef LLVM_STDCALL LLVMConstNull(LLVMTypeRef Ty) {
  return wrap(Constant::getNullValue(unwrap(Ty)));
}

LLVMValueRef LLVM_STDCALL LLVMConstAllOnes(LLVMTypeRef Ty) {
  return wrap(Constant::getAllOnesValue(unwrap(Ty)));
}

LLVMValueRef LLVM_STDCALL LLVMGetUndef(LLVMTypeRef Ty) {
  return wrap(UndefValue::get(unwrap(Ty)));
}

LLVMBool LLVM_STDCALL LLVMIsConstant(LLVMValueRef Ty) {
  return isa<Constant>(unwrap(Ty));
}

LLVMBool LLVM_STDCALL LLVMIsNull(LLVMValueRef Val) {
  if (Constant *C = dyn_cast<Constant>(unwrap(Val)))
    return C->isNullValue();
  return false;
}

LLVMBool LLVM_STDCALL LLVMIsUndef(LLVMValueRef Val) {
  return isa<UndefValue>(unwrap(Val));
}

LLVMValueRef LLVM_STDCALL LLVMConstPointerNull(LLVMTypeRef Ty) {
  return wrap(ConstantPointerNull::get(unwrap<PointerType>(Ty)));
}

/*--.. Operations on metadata nodes ........................................--*/

LLVMValueRef LLVM_STDCALL LLVMMDStringInContext(LLVMContextRef C, const char *Str,
                                   unsigned SLen) {
  LLVMContext &Context = *unwrap(C);
  return wrap(MetadataAsValue::get(
      Context, MDString::get(Context, StringRef(Str, SLen))));
}

LLVMValueRef LLVM_STDCALL LLVMMDString(const char *Str, unsigned SLen) {
  return LLVMMDStringInContext(LLVMGetGlobalContext(), Str, SLen);
}

LLVMValueRef LLVM_STDCALL LLVMMDNodeInContext(LLVMContextRef C, LLVMValueRef *Vals,
                                 unsigned Count) {
  LLVMContext &Context = *unwrap(C);
  SmallVector<Metadata *, 8> MDs;
  for (auto *OV : makeArrayRef(Vals, Count)) {
    Value *V = unwrap(OV);
    Metadata *MD;
    if (!V)
      MD = nullptr;
    else if (auto *C = dyn_cast<Constant>(V))
      MD = ConstantAsMetadata::get(C);
    else if (auto *MDV = dyn_cast<MetadataAsValue>(V)) {
      MD = MDV->getMetadata();
      assert(!isa<LocalAsMetadata>(MD) && "Unexpected function-local metadata "
                                          "outside of direct argument to call");
    } else {
      // This is function-local metadata.  Pretend to make an MDNode.
      assert(Count == 1 &&
             "Expected only one operand to function-local metadata");
      return wrap(MetadataAsValue::get(Context, LocalAsMetadata::get(V)));
    }

    MDs.push_back(MD);
  }
  return wrap(MetadataAsValue::get(Context, MDNode::get(Context, MDs)));
}

LLVMValueRef LLVM_STDCALL LLVMMDNode(LLVMValueRef *Vals, unsigned Count) {
  return LLVMMDNodeInContext(LLVMGetGlobalContext(), Vals, Count);
}

LLVMValueRef LLVM_STDCALL LLVMMetadataAsValue(LLVMContextRef C, LLVMMetadataRef MD) {
  return wrap(MetadataAsValue::get(*unwrap(C), unwrap(MD)));
}

LLVMMetadataRef LLVM_STDCALL LLVMValueAsMetadata(LLVMValueRef Val) {
  auto *V = unwrap(Val);
  if (auto *C = dyn_cast<Constant>(V))
    return wrap(ConstantAsMetadata::get(C));
  if (auto *MAV = dyn_cast<MetadataAsValue>(V))
    return wrap(MAV->getMetadata());
  return wrap(ValueAsMetadata::get(V));
}

const char *LLVM_STDCALL LLVMGetMDString(LLVMValueRef V, unsigned *Length) {
  if (const auto *MD = dyn_cast<MetadataAsValue>(unwrap(V)))
    if (const MDString *S = dyn_cast<MDString>(MD->getMetadata())) {
      *Length = S->getString().size();
      return S->getString().data();
    }
  *Length = 0;
  return nullptr;
}

unsigned LLVM_STDCALL LLVMGetMDNodeNumOperands(LLVMValueRef V) {
  auto *MD = cast<MetadataAsValue>(unwrap(V));
  if (isa<ValueAsMetadata>(MD->getMetadata()))
    return 1;
  return cast<MDNode>(MD->getMetadata())->getNumOperands();
}

void LLVM_STDCALL LLVMGetMDNodeOperands(LLVMValueRef V, LLVMValueRef *Dest) {
  auto *MD = cast<MetadataAsValue>(unwrap(V));
  if (auto *MDV = dyn_cast<ValueAsMetadata>(MD->getMetadata())) {
    *Dest = wrap(MDV->getValue());
    return;
  }
  const auto *N = cast<MDNode>(MD->getMetadata());
  const unsigned numOperands = N->getNumOperands();
  LLVMContext &Context = unwrap(V)->getContext();
  for (unsigned i = 0; i < numOperands; i++)
    Dest[i] = getMDNodeOperandImpl(Context, N, i);
}

unsigned LLVM_STDCALL LLVMGetNamedMetadataNumOperands(LLVMModuleRef M, const char *Name) {
  if (NamedMDNode *N = unwrap(M)->getNamedMetadata(Name)) {
    return N->getNumOperands();
  }
  return 0;
}

void LLVM_STDCALL LLVMGetNamedMetadataOperands(LLVMModuleRef M, const char *Name,
                                  LLVMValueRef *Dest) {
  NamedMDNode *N = unwrap(M)->getNamedMetadata(Name);
  if (!N)
    return;
  LLVMContext &Context = unwrap(M)->getContext();
  for (unsigned i=0;i<N->getNumOperands();i++)
    Dest[i] = wrap(MetadataAsValue::get(Context, N->getOperand(i)));
}

void LLVM_STDCALL LLVMAddNamedMetadataOperand(LLVMModuleRef M, const char *Name,
                                 LLVMValueRef Val) {
  NamedMDNode *N = unwrap(M)->getOrInsertNamedMetadata(Name);
  if (!N)
    return;
  if (!Val)
    return;
  N->addOperand(extractMDNode(unwrap<MetadataAsValue>(Val)));
}

/*--.. Operations on scalar constants ......................................--*/

LLVMValueRef LLVM_STDCALL LLVMConstInt(LLVMTypeRef IntTy, unsigned long long N,
                          LLVMBool SignExtend) {
  return wrap(ConstantInt::get(unwrap<IntegerType>(IntTy), N, SignExtend != 0));
}

LLVMValueRef LLVM_STDCALL LLVMConstIntOfArbitraryPrecision(LLVMTypeRef IntTy,
                                              unsigned NumWords,
                                              const uint64_t Words[]) {
    IntegerType *Ty = unwrap<IntegerType>(IntTy);
    return wrap(ConstantInt::get(Ty->getContext(),
                                 APInt(Ty->getBitWidth(),
                                       makeArrayRef(Words, NumWords))));
}

LLVMValueRef LLVM_STDCALL LLVMConstIntOfString(LLVMTypeRef IntTy, const char Str[],
                                  uint8_t Radix) {
  return wrap(ConstantInt::get(unwrap<IntegerType>(IntTy), StringRef(Str),
                               Radix));
}

LLVMValueRef LLVM_STDCALL LLVMConstIntOfStringAndSize(LLVMTypeRef IntTy, const char Str[],
                                         unsigned SLen, uint8_t Radix) {
  return wrap(ConstantInt::get(unwrap<IntegerType>(IntTy), StringRef(Str, SLen),
                               Radix));
}

LLVMValueRef LLVM_STDCALL LLVMConstReal(LLVMTypeRef RealTy, double N) {
  return wrap(ConstantFP::get(unwrap(RealTy), N));
}

LLVMValueRef LLVM_STDCALL LLVMConstRealOfString(LLVMTypeRef RealTy, const char *Text) {
  return wrap(ConstantFP::get(unwrap(RealTy), StringRef(Text)));
}

LLVMValueRef LLVM_STDCALL LLVMConstRealOfStringAndSize(LLVMTypeRef RealTy, const char Str[],
                                          unsigned SLen) {
  return wrap(ConstantFP::get(unwrap(RealTy), StringRef(Str, SLen)));
}

unsigned long long LLVM_STDCALL LLVMConstIntGetZExtValue(LLVMValueRef ConstantVal) {
  return unwrap<ConstantInt>(ConstantVal)->getZExtValue();
}

long long LLVM_STDCALL LLVMConstIntGetSExtValue(LLVMValueRef ConstantVal) {
  return unwrap<ConstantInt>(ConstantVal)->getSExtValue();
}

double LLVM_STDCALL LLVMConstRealGetDouble(LLVMValueRef ConstantVal, LLVMBool *LosesInfo) {
  ConstantFP *cFP = unwrap<ConstantFP>(ConstantVal) ;
  Type *Ty = cFP->getType();

  if (Ty->isFloatTy()) {
    *LosesInfo = false;
    return cFP->getValueAPF().convertToFloat();
  }

  if (Ty->isDoubleTy()) {
    *LosesInfo = false;
    return cFP->getValueAPF().convertToDouble();
  }

  bool APFLosesInfo;
  APFloat APF = cFP->getValueAPF();
  APF.convert(APFloat::IEEEdouble(), APFloat::rmNearestTiesToEven, &APFLosesInfo);
  *LosesInfo = APFLosesInfo;
  return APF.convertToDouble();
}

/*--.. Operations on composite constants ...................................--*/

LLVMValueRef LLVM_STDCALL LLVMConstStringInContext(LLVMContextRef C, const char *Str,
                                      unsigned Length,
                                      LLVMBool DontNullTerminate) {
  /* Inverted the sense of AddNull because ', 0)' is a
     better mnemonic for null termination than ', 1)'. */
  return wrap(ConstantDataArray::getString(*unwrap(C), StringRef(Str, Length),
                                           DontNullTerminate == 0));
}

LLVMValueRef LLVM_STDCALL LLVMConstString(const char *Str, unsigned Length,
                             LLVMBool DontNullTerminate) {
  return LLVMConstStringInContext(LLVMGetGlobalContext(), Str, Length,
                                  DontNullTerminate);
}

LLVMValueRef LLVM_STDCALL LLVMGetElementAsConstant(LLVMValueRef C, unsigned idx) {
  return wrap(unwrap<ConstantDataSequential>(C)->getElementAsConstant(idx));
}

LLVMBool LLVM_STDCALL LLVMIsConstantString(LLVMValueRef C) {
  return unwrap<ConstantDataSequential>(C)->isString();
}

const char *LLVM_STDCALL LLVMGetAsString(LLVMValueRef C, size_t *Length) {
  StringRef Str = unwrap<ConstantDataSequential>(C)->getAsString();
  *Length = Str.size();
  return Str.data();
}

LLVMValueRef LLVM_STDCALL LLVMConstArray(LLVMTypeRef ElementTy,
                            LLVMValueRef *ConstantVals, unsigned Length) {
  ArrayRef<Constant*> V(unwrap<Constant>(ConstantVals, Length), Length);
  return wrap(ConstantArray::get(ArrayType::get(unwrap(ElementTy), Length), V));
}

LLVMValueRef LLVM_STDCALL LLVMConstStructInContext(LLVMContextRef C,
                                      LLVMValueRef *ConstantVals,
                                      unsigned Count, LLVMBool Packed) {
  Constant **Elements = unwrap<Constant>(ConstantVals, Count);
  return wrap(ConstantStruct::getAnon(*unwrap(C), makeArrayRef(Elements, Count),
                                      Packed != 0));
}

LLVMValueRef LLVM_STDCALL LLVMConstStruct(LLVMValueRef *ConstantVals, unsigned Count,
                             LLVMBool Packed) {
  return LLVMConstStructInContext(LLVMGetGlobalContext(), ConstantVals, Count,
                                  Packed);
}

LLVMValueRef LLVM_STDCALL LLVMConstNamedStruct(LLVMTypeRef StructTy,
                                  LLVMValueRef *ConstantVals,
                                  unsigned Count) {
  Constant **Elements = unwrap<Constant>(ConstantVals, Count);
  StructType *Ty = cast<StructType>(unwrap(StructTy));

  return wrap(ConstantStruct::get(Ty, makeArrayRef(Elements, Count)));
}

LLVMValueRef LLVM_STDCALL LLVMConstVector(LLVMValueRef *ScalarConstantVals, unsigned Size) {
  return wrap(ConstantVector::get(makeArrayRef(
                            unwrap<Constant>(ScalarConstantVals, Size), Size)));
}

/*-- Opcode mapping */

static LLVMOpcode map_to_llvmopcode(int opcode)
{
    switch (opcode) {
      default: llvm_unreachable("Unhandled Opcode.");
#define HANDLE_INST(num, opc, clas) case num: return LLVM##opc;
#include "llvm/IR/Instruction.def"
#undef HANDLE_INST
    }
}

static int map_from_llvmopcode(LLVMOpcode code)
{
    switch (code) {
#define HANDLE_INST(num, opc, clas) case LLVM##opc: return num;
#include "llvm/IR/Instruction.def"
#undef HANDLE_INST
    }
    llvm_unreachable("Unhandled Opcode.");
}

/*--.. Constant expressions ................................................--*/

LLVMOpcode LLVM_STDCALL LLVMGetConstOpcode(LLVMValueRef ConstantVal) {
  return map_to_llvmopcode(unwrap<ConstantExpr>(ConstantVal)->getOpcode());
}

LLVMValueRef LLVM_STDCALL LLVMAlignOf(LLVMTypeRef Ty) {
  return wrap(ConstantExpr::getAlignOf(unwrap(Ty)));
}

LLVMValueRef LLVM_STDCALL LLVMSizeOf(LLVMTypeRef Ty) {
  return wrap(ConstantExpr::getSizeOf(unwrap(Ty)));
}

LLVMValueRef LLVM_STDCALL LLVMConstNeg(LLVMValueRef ConstantVal) {
  return wrap(ConstantExpr::getNeg(unwrap<Constant>(ConstantVal)));
}

LLVMValueRef LLVM_STDCALL LLVMConstNSWNeg(LLVMValueRef ConstantVal) {
  return wrap(ConstantExpr::getNSWNeg(unwrap<Constant>(ConstantVal)));
}

LLVMValueRef LLVM_STDCALL LLVMConstNUWNeg(LLVMValueRef ConstantVal) {
  return wrap(ConstantExpr::getNUWNeg(unwrap<Constant>(ConstantVal)));
}


LLVMValueRef LLVM_STDCALL LLVMConstFNeg(LLVMValueRef ConstantVal) {
  return wrap(ConstantExpr::getFNeg(unwrap<Constant>(ConstantVal)));
}

LLVMValueRef LLVM_STDCALL LLVMConstNot(LLVMValueRef ConstantVal) {
  return wrap(ConstantExpr::getNot(unwrap<Constant>(ConstantVal)));
}

LLVMValueRef LLVM_STDCALL LLVMConstAdd(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant) {
  return wrap(ConstantExpr::getAdd(unwrap<Constant>(LHSConstant),
                                   unwrap<Constant>(RHSConstant)));
}

LLVMValueRef LLVM_STDCALL LLVMConstNSWAdd(LLVMValueRef LHSConstant,
                             LLVMValueRef RHSConstant) {
  return wrap(ConstantExpr::getNSWAdd(unwrap<Constant>(LHSConstant),
                                      unwrap<Constant>(RHSConstant)));
}

LLVMValueRef LLVM_STDCALL LLVMConstNUWAdd(LLVMValueRef LHSConstant,
                             LLVMValueRef RHSConstant) {
  return wrap(ConstantExpr::getNUWAdd(unwrap<Constant>(LHSConstant),
                                      unwrap<Constant>(RHSConstant)));
}

LLVMValueRef LLVM_STDCALL LLVMConstFAdd(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant) {
  return wrap(ConstantExpr::getFAdd(unwrap<Constant>(LHSConstant),
                                    unwrap<Constant>(RHSConstant)));
}

LLVMValueRef LLVM_STDCALL LLVMConstSub(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant) {
  return wrap(ConstantExpr::getSub(unwrap<Constant>(LHSConstant),
                                   unwrap<Constant>(RHSConstant)));
}

LLVMValueRef LLVM_STDCALL LLVMConstNSWSub(LLVMValueRef LHSConstant,
                             LLVMValueRef RHSConstant) {
  return wrap(ConstantExpr::getNSWSub(unwrap<Constant>(LHSConstant),
                                      unwrap<Constant>(RHSConstant)));
}

LLVMValueRef LLVM_STDCALL LLVMConstNUWSub(LLVMValueRef LHSConstant,
                             LLVMValueRef RHSConstant) {
  return wrap(ConstantExpr::getNUWSub(unwrap<Constant>(LHSConstant),
                                      unwrap<Constant>(RHSConstant)));
}

LLVMValueRef LLVM_STDCALL LLVMConstFSub(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant) {
  return wrap(ConstantExpr::getFSub(unwrap<Constant>(LHSConstant),
                                    unwrap<Constant>(RHSConstant)));
}

LLVMValueRef LLVM_STDCALL LLVMConstMul(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant) {
  return wrap(ConstantExpr::getMul(unwrap<Constant>(LHSConstant),
                                   unwrap<Constant>(RHSConstant)));
}

LLVMValueRef LLVM_STDCALL LLVMConstNSWMul(LLVMValueRef LHSConstant,
                             LLVMValueRef RHSConstant) {
  return wrap(ConstantExpr::getNSWMul(unwrap<Constant>(LHSConstant),
                                      unwrap<Constant>(RHSConstant)));
}

LLVMValueRef LLVM_STDCALL LLVMConstNUWMul(LLVMValueRef LHSConstant,
                             LLVMValueRef RHSConstant) {
  return wrap(ConstantExpr::getNUWMul(unwrap<Constant>(LHSConstant),
                                      unwrap<Constant>(RHSConstant)));
}

LLVMValueRef LLVM_STDCALL LLVMConstFMul(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant) {
  return wrap(ConstantExpr::getFMul(unwrap<Constant>(LHSConstant),
                                    unwrap<Constant>(RHSConstant)));
}

LLVMValueRef LLVM_STDCALL LLVMConstUDiv(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant) {
  return wrap(ConstantExpr::getUDiv(unwrap<Constant>(LHSConstant),
                                    unwrap<Constant>(RHSConstant)));
}

LLVMValueRef LLVM_STDCALL LLVMConstExactUDiv(LLVMValueRef LHSConstant,
                                LLVMValueRef RHSConstant) {
  return wrap(ConstantExpr::getExactUDiv(unwrap<Constant>(LHSConstant),
                                         unwrap<Constant>(RHSConstant)));
}

LLVMValueRef LLVM_STDCALL LLVMConstSDiv(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant) {
  return wrap(ConstantExpr::getSDiv(unwrap<Constant>(LHSConstant),
                                    unwrap<Constant>(RHSConstant)));
}

LLVMValueRef LLVM_STDCALL LLVMConstExactSDiv(LLVMValueRef LHSConstant,
                                LLVMValueRef RHSConstant) {
  return wrap(ConstantExpr::getExactSDiv(unwrap<Constant>(LHSConstant),
                                         unwrap<Constant>(RHSConstant)));
}

LLVMValueRef LLVM_STDCALL LLVMConstFDiv(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant) {
  return wrap(ConstantExpr::getFDiv(unwrap<Constant>(LHSConstant),
                                    unwrap<Constant>(RHSConstant)));
}

LLVMValueRef LLVM_STDCALL LLVMConstURem(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant) {
  return wrap(ConstantExpr::getURem(unwrap<Constant>(LHSConstant),
                                    unwrap<Constant>(RHSConstant)));
}

LLVMValueRef LLVM_STDCALL LLVMConstSRem(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant) {
  return wrap(ConstantExpr::getSRem(unwrap<Constant>(LHSConstant),
                                    unwrap<Constant>(RHSConstant)));
}

LLVMValueRef LLVM_STDCALL LLVMConstFRem(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant) {
  return wrap(ConstantExpr::getFRem(unwrap<Constant>(LHSConstant),
                                    unwrap<Constant>(RHSConstant)));
}

LLVMValueRef LLVM_STDCALL LLVMConstAnd(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant) {
  return wrap(ConstantExpr::getAnd(unwrap<Constant>(LHSConstant),
                                   unwrap<Constant>(RHSConstant)));
}

LLVMValueRef LLVM_STDCALL LLVMConstOr(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant) {
  return wrap(ConstantExpr::getOr(unwrap<Constant>(LHSConstant),
                                  unwrap<Constant>(RHSConstant)));
}

LLVMValueRef LLVM_STDCALL LLVMConstXor(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant) {
  return wrap(ConstantExpr::getXor(unwrap<Constant>(LHSConstant),
                                   unwrap<Constant>(RHSConstant)));
}

LLVMValueRef LLVM_STDCALL LLVMConstICmp(LLVMIntPredicate Predicate,
                           LLVMValueRef LHSConstant, LLVMValueRef RHSConstant) {
  return wrap(ConstantExpr::getICmp(Predicate,
                                    unwrap<Constant>(LHSConstant),
                                    unwrap<Constant>(RHSConstant)));
}

LLVMValueRef LLVM_STDCALL LLVMConstFCmp(LLVMRealPredicate Predicate,
                           LLVMValueRef LHSConstant, LLVMValueRef RHSConstant) {
  return wrap(ConstantExpr::getFCmp(Predicate,
                                    unwrap<Constant>(LHSConstant),
                                    unwrap<Constant>(RHSConstant)));
}

LLVMValueRef LLVM_STDCALL LLVMConstShl(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant) {
  return wrap(ConstantExpr::getShl(unwrap<Constant>(LHSConstant),
                                   unwrap<Constant>(RHSConstant)));
}

LLVMValueRef LLVM_STDCALL LLVMConstLShr(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant) {
  return wrap(ConstantExpr::getLShr(unwrap<Constant>(LHSConstant),
                                    unwrap<Constant>(RHSConstant)));
}

LLVMValueRef LLVM_STDCALL LLVMConstAShr(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant) {
  return wrap(ConstantExpr::getAShr(unwrap<Constant>(LHSConstant),
                                    unwrap<Constant>(RHSConstant)));
}

LLVMValueRef LLVM_STDCALL LLVMConstGEP(LLVMValueRef ConstantVal,
                          LLVMValueRef *ConstantIndices, unsigned NumIndices) {
  ArrayRef<Constant *> IdxList(unwrap<Constant>(ConstantIndices, NumIndices),
                               NumIndices);
  return wrap(ConstantExpr::getGetElementPtr(
      nullptr, unwrap<Constant>(ConstantVal), IdxList));
}

LLVMValueRef LLVM_STDCALL LLVMConstInBoundsGEP(LLVMValueRef ConstantVal,
                                  LLVMValueRef *ConstantIndices,
                                  unsigned NumIndices) {
  Constant* Val = unwrap<Constant>(ConstantVal);
  ArrayRef<Constant *> IdxList(unwrap<Constant>(ConstantIndices, NumIndices),
                               NumIndices);
  return wrap(ConstantExpr::getInBoundsGetElementPtr(nullptr, Val, IdxList));
}

LLVMValueRef LLVM_STDCALL LLVMConstTrunc(LLVMValueRef ConstantVal, LLVMTypeRef ToType) {
  return wrap(ConstantExpr::getTrunc(unwrap<Constant>(ConstantVal),
                                     unwrap(ToType)));
}

LLVMValueRef LLVM_STDCALL LLVMConstSExt(LLVMValueRef ConstantVal, LLVMTypeRef ToType) {
  return wrap(ConstantExpr::getSExt(unwrap<Constant>(ConstantVal),
                                    unwrap(ToType)));
}

LLVMValueRef LLVM_STDCALL LLVMConstZExt(LLVMValueRef ConstantVal, LLVMTypeRef ToType) {
  return wrap(ConstantExpr::getZExt(unwrap<Constant>(ConstantVal),
                                    unwrap(ToType)));
}

LLVMValueRef LLVM_STDCALL LLVMConstFPTrunc(LLVMValueRef ConstantVal, LLVMTypeRef ToType) {
  return wrap(ConstantExpr::getFPTrunc(unwrap<Constant>(ConstantVal),
                                       unwrap(ToType)));
}

LLVMValueRef LLVM_STDCALL LLVMConstFPExt(LLVMValueRef ConstantVal, LLVMTypeRef ToType) {
  return wrap(ConstantExpr::getFPExtend(unwrap<Constant>(ConstantVal),
                                        unwrap(ToType)));
}

LLVMValueRef LLVM_STDCALL LLVMConstUIToFP(LLVMValueRef ConstantVal, LLVMTypeRef ToType) {
  return wrap(ConstantExpr::getUIToFP(unwrap<Constant>(ConstantVal),
                                      unwrap(ToType)));
}

LLVMValueRef LLVM_STDCALL LLVMConstSIToFP(LLVMValueRef ConstantVal, LLVMTypeRef ToType) {
  return wrap(ConstantExpr::getSIToFP(unwrap<Constant>(ConstantVal),
                                      unwrap(ToType)));
}

LLVMValueRef LLVM_STDCALL LLVMConstFPToUI(LLVMValueRef ConstantVal, LLVMTypeRef ToType) {
  return wrap(ConstantExpr::getFPToUI(unwrap<Constant>(ConstantVal),
                                      unwrap(ToType)));
}

LLVMValueRef LLVM_STDCALL LLVMConstFPToSI(LLVMValueRef ConstantVal, LLVMTypeRef ToType) {
  return wrap(ConstantExpr::getFPToSI(unwrap<Constant>(ConstantVal),
                                      unwrap(ToType)));
}

LLVMValueRef LLVM_STDCALL LLVMConstPtrToInt(LLVMValueRef ConstantVal, LLVMTypeRef ToType) {
  return wrap(ConstantExpr::getPtrToInt(unwrap<Constant>(ConstantVal),
                                        unwrap(ToType)));
}

LLVMValueRef LLVM_STDCALL LLVMConstIntToPtr(LLVMValueRef ConstantVal, LLVMTypeRef ToType) {
  return wrap(ConstantExpr::getIntToPtr(unwrap<Constant>(ConstantVal),
                                        unwrap(ToType)));
}

LLVMValueRef LLVM_STDCALL LLVMConstBitCast(LLVMValueRef ConstantVal, LLVMTypeRef ToType) {
  return wrap(ConstantExpr::getBitCast(unwrap<Constant>(ConstantVal),
                                       unwrap(ToType)));
}

LLVMValueRef LLVM_STDCALL LLVMConstAddrSpaceCast(LLVMValueRef ConstantVal,
                                    LLVMTypeRef ToType) {
  return wrap(ConstantExpr::getAddrSpaceCast(unwrap<Constant>(ConstantVal),
                                             unwrap(ToType)));
}

LLVMValueRef LLVM_STDCALL LLVMConstZExtOrBitCast(LLVMValueRef ConstantVal,
                                    LLVMTypeRef ToType) {
  return wrap(ConstantExpr::getZExtOrBitCast(unwrap<Constant>(ConstantVal),
                                             unwrap(ToType)));
}

LLVMValueRef LLVM_STDCALL LLVMConstSExtOrBitCast(LLVMValueRef ConstantVal,
                                    LLVMTypeRef ToType) {
  return wrap(ConstantExpr::getSExtOrBitCast(unwrap<Constant>(ConstantVal),
                                             unwrap(ToType)));
}

LLVMValueRef LLVM_STDCALL LLVMConstTruncOrBitCast(LLVMValueRef ConstantVal,
                                     LLVMTypeRef ToType) {
  return wrap(ConstantExpr::getTruncOrBitCast(unwrap<Constant>(ConstantVal),
                                              unwrap(ToType)));
}

LLVMValueRef LLVM_STDCALL LLVMConstPointerCast(LLVMValueRef ConstantVal,
                                  LLVMTypeRef ToType) {
  return wrap(ConstantExpr::getPointerCast(unwrap<Constant>(ConstantVal),
                                           unwrap(ToType)));
}

LLVMValueRef LLVM_STDCALL LLVMConstIntCast(LLVMValueRef ConstantVal, LLVMTypeRef ToType,
                              LLVMBool isSigned) {
  return wrap(ConstantExpr::getIntegerCast(unwrap<Constant>(ConstantVal),
                                           unwrap(ToType), isSigned));
}

LLVMValueRef LLVM_STDCALL LLVMConstFPCast(LLVMValueRef ConstantVal, LLVMTypeRef ToType) {
  return wrap(ConstantExpr::getFPCast(unwrap<Constant>(ConstantVal),
                                      unwrap(ToType)));
}

LLVMValueRef LLVM_STDCALL LLVMConstSelect(LLVMValueRef ConstantCondition,
                             LLVMValueRef ConstantIfTrue,
                             LLVMValueRef ConstantIfFalse) {
  return wrap(ConstantExpr::getSelect(unwrap<Constant>(ConstantCondition),
                                      unwrap<Constant>(ConstantIfTrue),
                                      unwrap<Constant>(ConstantIfFalse)));
}

LLVMValueRef LLVM_STDCALL LLVMConstExtractElement(LLVMValueRef VectorConstant,
                                     LLVMValueRef IndexConstant) {
  return wrap(ConstantExpr::getExtractElement(unwrap<Constant>(VectorConstant),
                                              unwrap<Constant>(IndexConstant)));
}

LLVMValueRef LLVM_STDCALL LLVMConstInsertElement(LLVMValueRef VectorConstant,
                                    LLVMValueRef ElementValueConstant,
                                    LLVMValueRef IndexConstant) {
  return wrap(ConstantExpr::getInsertElement(unwrap<Constant>(VectorConstant),
                                         unwrap<Constant>(ElementValueConstant),
                                             unwrap<Constant>(IndexConstant)));
}

LLVMValueRef LLVM_STDCALL LLVMConstShuffleVector(LLVMValueRef VectorAConstant,
                                    LLVMValueRef VectorBConstant,
                                    LLVMValueRef MaskConstant) {
  return wrap(ConstantExpr::getShuffleVector(unwrap<Constant>(VectorAConstant),
                                             unwrap<Constant>(VectorBConstant),
                                             unwrap<Constant>(MaskConstant)));
}

LLVMValueRef LLVM_STDCALL LLVMConstExtractValue(LLVMValueRef AggConstant, unsigned *IdxList,
                                   unsigned NumIdx) {
  return wrap(ConstantExpr::getExtractValue(unwrap<Constant>(AggConstant),
                                            makeArrayRef(IdxList, NumIdx)));
}

LLVMValueRef LLVM_STDCALL LLVMConstInsertValue(LLVMValueRef AggConstant,
                                  LLVMValueRef ElementValueConstant,
                                  unsigned *IdxList, unsigned NumIdx) {
  return wrap(ConstantExpr::getInsertValue(unwrap<Constant>(AggConstant),
                                         unwrap<Constant>(ElementValueConstant),
                                           makeArrayRef(IdxList, NumIdx)));
}

LLVMValueRef LLVM_STDCALL LLVMConstInlineAsm(LLVMTypeRef Ty, const char *AsmString,
                                const char *Constraints,
                                LLVMBool HasSideEffects,
                                LLVMBool IsAlignStack) {
  return wrap(InlineAsm::get(dyn_cast<FunctionType>(unwrap(Ty)), AsmString,
                             Constraints, HasSideEffects, IsAlignStack));
}

LLVMValueRef LLVM_STDCALL LLVMBlockAddress(LLVMValueRef F, LLVMBasicBlockRef BB) {
  return wrap(BlockAddress::get(unwrap<Function>(F), unwrap(BB)));
}

/*--.. Operations on global variables, functions, and aliases (globals) ....--*/

LLVMModuleRef LLVM_STDCALL LLVMGetGlobalParent(LLVMValueRef Global) {
  return wrap(unwrap<GlobalValue>(Global)->getParent());
}

LLVMBool LLVM_STDCALL LLVMIsDeclaration(LLVMValueRef Global) {
  return unwrap<GlobalValue>(Global)->isDeclaration();
}

LLVMLinkage LLVM_STDCALL LLVMGetLinkage(LLVMValueRef Global) {
  switch (unwrap<GlobalValue>(Global)->getLinkage()) {
  case GlobalValue::ExternalLinkage:
    return LLVMExternalLinkage;
  case GlobalValue::AvailableExternallyLinkage:
    return LLVMAvailableExternallyLinkage;
  case GlobalValue::LinkOnceAnyLinkage:
    return LLVMLinkOnceAnyLinkage;
  case GlobalValue::LinkOnceODRLinkage:
    return LLVMLinkOnceODRLinkage;
  case GlobalValue::WeakAnyLinkage:
    return LLVMWeakAnyLinkage;
  case GlobalValue::WeakODRLinkage:
    return LLVMWeakODRLinkage;
  case GlobalValue::AppendingLinkage:
    return LLVMAppendingLinkage;
  case GlobalValue::InternalLinkage:
    return LLVMInternalLinkage;
  case GlobalValue::PrivateLinkage:
    return LLVMPrivateLinkage;
  case GlobalValue::ExternalWeakLinkage:
    return LLVMExternalWeakLinkage;
  case GlobalValue::CommonLinkage:
    return LLVMCommonLinkage;
  }

  llvm_unreachable("Invalid GlobalValue linkage!");
}

void LLVM_STDCALL LLVMSetLinkage(LLVMValueRef Global, LLVMLinkage Linkage) {
  GlobalValue *GV = unwrap<GlobalValue>(Global);

  switch (Linkage) {
  case LLVMExternalLinkage:
    GV->setLinkage(GlobalValue::ExternalLinkage);
    break;
  case LLVMAvailableExternallyLinkage:
    GV->setLinkage(GlobalValue::AvailableExternallyLinkage);
    break;
  case LLVMLinkOnceAnyLinkage:
    GV->setLinkage(GlobalValue::LinkOnceAnyLinkage);
    break;
  case LLVMLinkOnceODRLinkage:
    GV->setLinkage(GlobalValue::LinkOnceODRLinkage);
    break;
  case LLVMLinkOnceODRAutoHideLinkage:
    DEBUG(errs() << "LLVMSetLinkage(): LLVMLinkOnceODRAutoHideLinkage is no "
                    "longer supported.");
    break;
  case LLVMWeakAnyLinkage:
    GV->setLinkage(GlobalValue::WeakAnyLinkage);
    break;
  case LLVMWeakODRLinkage:
    GV->setLinkage(GlobalValue::WeakODRLinkage);
    break;
  case LLVMAppendingLinkage:
    GV->setLinkage(GlobalValue::AppendingLinkage);
    break;
  case LLVMInternalLinkage:
    GV->setLinkage(GlobalValue::InternalLinkage);
    break;
  case LLVMPrivateLinkage:
    GV->setLinkage(GlobalValue::PrivateLinkage);
    break;
  case LLVMLinkerPrivateLinkage:
    GV->setLinkage(GlobalValue::PrivateLinkage);
    break;
  case LLVMLinkerPrivateWeakLinkage:
    GV->setLinkage(GlobalValue::PrivateLinkage);
    break;
  case LLVMDLLImportLinkage:
    DEBUG(errs()
          << "LLVMSetLinkage(): LLVMDLLImportLinkage is no longer supported.");
    break;
  case LLVMDLLExportLinkage:
    DEBUG(errs()
          << "LLVMSetLinkage(): LLVMDLLExportLinkage is no longer supported.");
    break;
  case LLVMExternalWeakLinkage:
    GV->setLinkage(GlobalValue::ExternalWeakLinkage);
    break;
  case LLVMGhostLinkage:
    DEBUG(errs()
          << "LLVMSetLinkage(): LLVMGhostLinkage is no longer supported.");
    break;
  case LLVMCommonLinkage:
    GV->setLinkage(GlobalValue::CommonLinkage);
    break;
  }
}

const char *LLVM_STDCALL LLVMGetSection(LLVMValueRef Global) {
  // Using .data() is safe because of how GlobalObject::setSection is
  // implemented.
  return unwrap<GlobalValue>(Global)->getSection().data();
}

void LLVM_STDCALL LLVMSetSection(LLVMValueRef Global, const char *Section) {
  unwrap<GlobalObject>(Global)->setSection(Section);
}

LLVMVisibility LLVM_STDCALL LLVMGetVisibility(LLVMValueRef Global) {
  return static_cast<LLVMVisibility>(
    unwrap<GlobalValue>(Global)->getVisibility());
}

void LLVM_STDCALL LLVMSetVisibility(LLVMValueRef Global, LLVMVisibility Viz) {
  unwrap<GlobalValue>(Global)
    ->setVisibility(static_cast<GlobalValue::VisibilityTypes>(Viz));
}

LLVMDLLStorageClass LLVM_STDCALL LLVMGetDLLStorageClass(LLVMValueRef Global) {
  return static_cast<LLVMDLLStorageClass>(
      unwrap<GlobalValue>(Global)->getDLLStorageClass());
}

void LLVM_STDCALL LLVMSetDLLStorageClass(LLVMValueRef Global, LLVMDLLStorageClass Class) {
  unwrap<GlobalValue>(Global)->setDLLStorageClass(
      static_cast<GlobalValue::DLLStorageClassTypes>(Class));
}

LLVMBool LLVM_STDCALL LLVMHasUnnamedAddr(LLVMValueRef Global) {
  return unwrap<GlobalValue>(Global)->hasGlobalUnnamedAddr();
}

void LLVM_STDCALL LLVMSetUnnamedAddr(LLVMValueRef Global, LLVMBool HasUnnamedAddr) {
  unwrap<GlobalValue>(Global)->setUnnamedAddr(
      HasUnnamedAddr ? GlobalValue::UnnamedAddr::Global
                     : GlobalValue::UnnamedAddr::None);
}

/*--.. Operations on global variables, load and store instructions .........--*/

unsigned LLVM_STDCALL LLVMGetAlignment(LLVMValueRef V) {
  Value *P = unwrap<Value>(V);
  if (GlobalValue *GV = dyn_cast<GlobalValue>(P))
    return GV->getAlignment();
  if (AllocaInst *AI = dyn_cast<AllocaInst>(P))
    return AI->getAlignment();
  if (LoadInst *LI = dyn_cast<LoadInst>(P))
    return LI->getAlignment();
  if (StoreInst *SI = dyn_cast<StoreInst>(P))
    return SI->getAlignment();

  llvm_unreachable(
      "only GlobalValue, AllocaInst, LoadInst and StoreInst have alignment");
}

void LLVM_STDCALL LLVMSetAlignment(LLVMValueRef V, unsigned Bytes) {
  Value *P = unwrap<Value>(V);
  if (GlobalObject *GV = dyn_cast<GlobalObject>(P))
    GV->setAlignment(Bytes);
  else if (AllocaInst *AI = dyn_cast<AllocaInst>(P))
    AI->setAlignment(Bytes);
  else if (LoadInst *LI = dyn_cast<LoadInst>(P))
    LI->setAlignment(Bytes);
  else if (StoreInst *SI = dyn_cast<StoreInst>(P))
    SI->setAlignment(Bytes);
  else
    llvm_unreachable(
        "only GlobalValue, AllocaInst, LoadInst and StoreInst have alignment");
}

/*--.. Operations on global variables ......................................--*/

LLVMValueRef LLVM_STDCALL LLVMAddGlobal(LLVMModuleRef M, LLVMTypeRef Ty, const char *Name) {
  return wrap(new GlobalVariable(*unwrap(M), unwrap(Ty), false,
                                 GlobalValue::ExternalLinkage, nullptr, Name));
}

LLVMValueRef LLVM_STDCALL LLVMAddGlobalInAddressSpace(LLVMModuleRef M, LLVMTypeRef Ty,
                                         const char *Name,
                                         unsigned AddressSpace) {
  return wrap(new GlobalVariable(*unwrap(M), unwrap(Ty), false,
                                 GlobalValue::ExternalLinkage, nullptr, Name,
                                 nullptr, GlobalVariable::NotThreadLocal,
                                 AddressSpace));
}

LLVMValueRef LLVM_STDCALL LLVMGetNamedGlobal(LLVMModuleRef M, const char *Name) {
  return wrap(unwrap(M)->getNamedGlobal(Name));
}

LLVMValueRef LLVM_STDCALL LLVMGetFirstGlobal(LLVMModuleRef M) {
  Module *Mod = unwrap(M);
  Module::global_iterator I = Mod->global_begin();
  if (I == Mod->global_end())
    return nullptr;
  return wrap(&*I);
}

LLVMValueRef LLVM_STDCALL LLVMGetLastGlobal(LLVMModuleRef M) {
  Module *Mod = unwrap(M);
  Module::global_iterator I = Mod->global_end();
  if (I == Mod->global_begin())
    return nullptr;
  return wrap(&*--I);
}

LLVMValueRef LLVM_STDCALL LLVMGetNextGlobal(LLVMValueRef GlobalVar) {
  GlobalVariable *GV = unwrap<GlobalVariable>(GlobalVar);
  Module::global_iterator I(GV);
  if (++I == GV->getParent()->global_end())
    return nullptr;
  return wrap(&*I);
}

LLVMValueRef LLVM_STDCALL LLVMGetPreviousGlobal(LLVMValueRef GlobalVar) {
  GlobalVariable *GV = unwrap<GlobalVariable>(GlobalVar);
  Module::global_iterator I(GV);
  if (I == GV->getParent()->global_begin())
    return nullptr;
  return wrap(&*--I);
}

void LLVM_STDCALL LLVMDeleteGlobal(LLVMValueRef GlobalVar) {
  unwrap<GlobalVariable>(GlobalVar)->eraseFromParent();
}

LLVMValueRef LLVM_STDCALL LLVMGetInitializer(LLVMValueRef GlobalVar) {
  GlobalVariable* GV = unwrap<GlobalVariable>(GlobalVar);
  if ( !GV->hasInitializer() )
    return nullptr;
  return wrap(GV->getInitializer());
}

void LLVM_STDCALL LLVMSetInitializer(LLVMValueRef GlobalVar, LLVMValueRef ConstantVal) {
  unwrap<GlobalVariable>(GlobalVar)
    ->setInitializer(unwrap<Constant>(ConstantVal));
}

LLVMBool LLVM_STDCALL LLVMIsThreadLocal(LLVMValueRef GlobalVar) {
  return unwrap<GlobalVariable>(GlobalVar)->isThreadLocal();
}

void LLVM_STDCALL LLVMSetThreadLocal(LLVMValueRef GlobalVar, LLVMBool IsThreadLocal) {
  unwrap<GlobalVariable>(GlobalVar)->setThreadLocal(IsThreadLocal != 0);
}

LLVMBool LLVM_STDCALL LLVMIsGlobalConstant(LLVMValueRef GlobalVar) {
  return unwrap<GlobalVariable>(GlobalVar)->isConstant();
}

void LLVM_STDCALL LLVMSetGlobalConstant(LLVMValueRef GlobalVar, LLVMBool IsConstant) {
  unwrap<GlobalVariable>(GlobalVar)->setConstant(IsConstant != 0);
}

LLVMThreadLocalMode LLVM_STDCALL LLVMGetThreadLocalMode(LLVMValueRef GlobalVar) {
  switch (unwrap<GlobalVariable>(GlobalVar)->getThreadLocalMode()) {
  case GlobalVariable::NotThreadLocal:
    return LLVMNotThreadLocal;
  case GlobalVariable::GeneralDynamicTLSModel:
    return LLVMGeneralDynamicTLSModel;
  case GlobalVariable::LocalDynamicTLSModel:
    return LLVMLocalDynamicTLSModel;
  case GlobalVariable::InitialExecTLSModel:
    return LLVMInitialExecTLSModel;
  case GlobalVariable::LocalExecTLSModel:
    return LLVMLocalExecTLSModel;
  }

  llvm_unreachable("Invalid GlobalVariable thread local mode");
}

void LLVM_STDCALL LLVMSetThreadLocalMode(LLVMValueRef GlobalVar, LLVMThreadLocalMode Mode) {
  GlobalVariable *GV = unwrap<GlobalVariable>(GlobalVar);

  switch (Mode) {
  case LLVMNotThreadLocal:
    GV->setThreadLocalMode(GlobalVariable::NotThreadLocal);
    break;
  case LLVMGeneralDynamicTLSModel:
    GV->setThreadLocalMode(GlobalVariable::GeneralDynamicTLSModel);
    break;
  case LLVMLocalDynamicTLSModel:
    GV->setThreadLocalMode(GlobalVariable::LocalDynamicTLSModel);
    break;
  case LLVMInitialExecTLSModel:
    GV->setThreadLocalMode(GlobalVariable::InitialExecTLSModel);
    break;
  case LLVMLocalExecTLSModel:
    GV->setThreadLocalMode(GlobalVariable::LocalExecTLSModel);
    break;
  }
}

LLVMBool LLVM_STDCALL LLVMIsExternallyInitialized(LLVMValueRef GlobalVar) {
  return unwrap<GlobalVariable>(GlobalVar)->isExternallyInitialized();
}

void LLVM_STDCALL LLVMSetExternallyInitialized(LLVMValueRef GlobalVar, LLVMBool IsExtInit) {
  unwrap<GlobalVariable>(GlobalVar)->setExternallyInitialized(IsExtInit);
}

/*--.. Operations on aliases ......................................--*/

LLVMValueRef LLVM_STDCALL LLVMAddAlias(LLVMModuleRef M, LLVMTypeRef Ty, LLVMValueRef Aliasee,
                          const char *Name) {
  auto *PTy = cast<PointerType>(unwrap(Ty));
  return wrap(GlobalAlias::create(PTy->getElementType(), PTy->getAddressSpace(),
                                  GlobalValue::ExternalLinkage, Name,
                                  unwrap<Constant>(Aliasee), unwrap(M)));
}

/*--.. Operations on functions .............................................--*/

LLVMValueRef LLVM_STDCALL LLVMAddFunction(LLVMModuleRef M, const char *Name,
                             LLVMTypeRef FunctionTy) {
  return wrap(Function::Create(unwrap<FunctionType>(FunctionTy),
                               GlobalValue::ExternalLinkage, Name, unwrap(M)));
}

LLVMValueRef LLVM_STDCALL LLVMGetNamedFunction(LLVMModuleRef M, const char *Name) {
  return wrap(unwrap(M)->getFunction(Name));
}

LLVMValueRef LLVM_STDCALL LLVMGetFirstFunction(LLVMModuleRef M) {
  Module *Mod = unwrap(M);
  Module::iterator I = Mod->begin();
  if (I == Mod->end())
    return nullptr;
  return wrap(&*I);
}

LLVMValueRef LLVM_STDCALL LLVMGetLastFunction(LLVMModuleRef M) {
  Module *Mod = unwrap(M);
  Module::iterator I = Mod->end();
  if (I == Mod->begin())
    return nullptr;
  return wrap(&*--I);
}

LLVMValueRef LLVM_STDCALL LLVMGetNextFunction(LLVMValueRef Fn) {
  Function *Func = unwrap<Function>(Fn);
  Module::iterator I(Func);
  if (++I == Func->getParent()->end())
    return nullptr;
  return wrap(&*I);
}

LLVMValueRef LLVM_STDCALL LLVMGetPreviousFunction(LLVMValueRef Fn) {
  Function *Func = unwrap<Function>(Fn);
  Module::iterator I(Func);
  if (I == Func->getParent()->begin())
    return nullptr;
  return wrap(&*--I);
}

void LLVM_STDCALL LLVMDeleteFunction(LLVMValueRef Fn) {
  unwrap<Function>(Fn)->eraseFromParent();
}

LLVMBool LLVM_STDCALL LLVMHasPersonalityFn(LLVMValueRef Fn) {
  return unwrap<Function>(Fn)->hasPersonalityFn();
}

LLVMValueRef LLVM_STDCALL LLVMGetPersonalityFn(LLVMValueRef Fn) {
  return wrap(unwrap<Function>(Fn)->getPersonalityFn());
}

void LLVM_STDCALL LLVMSetPersonalityFn(LLVMValueRef Fn, LLVMValueRef PersonalityFn) {
  unwrap<Function>(Fn)->setPersonalityFn(unwrap<Constant>(PersonalityFn));
}

unsigned LLVM_STDCALL LLVMGetIntrinsicID(LLVMValueRef Fn) {
  if (Function *F = dyn_cast<Function>(unwrap(Fn)))
    return F->getIntrinsicID();
  return 0;
}

unsigned LLVM_STDCALL LLVMGetFunctionCallConv(LLVMValueRef Fn) {
  return unwrap<Function>(Fn)->getCallingConv();
}

void LLVM_STDCALL LLVMSetFunctionCallConv(LLVMValueRef Fn, unsigned CC) {
  return unwrap<Function>(Fn)->setCallingConv(
    static_cast<CallingConv::ID>(CC));
}

const char *LLVM_STDCALL LLVMGetGC(LLVMValueRef Fn) {
  Function *F = unwrap<Function>(Fn);
  return F->hasGC()? F->getGC().c_str() : nullptr;
}

void LLVM_STDCALL LLVMSetGC(LLVMValueRef Fn, const char *GC) {
  Function *F = unwrap<Function>(Fn);
  if (GC)
    F->setGC(GC);
  else
    F->clearGC();
}

void LLVM_STDCALL LLVMAddAttributeAtIndex(LLVMValueRef F, LLVMAttributeIndex Idx,
                             LLVMAttributeRef A) {
  unwrap<Function>(F)->addAttribute(Idx, unwrap(A));
}

unsigned LLVM_STDCALL LLVMGetAttributeCountAtIndex(LLVMValueRef F, LLVMAttributeIndex Idx) {
  auto AS = unwrap<Function>(F)->getAttributes().getAttributes(Idx);
  return AS.getNumAttributes();
}

void LLVM_STDCALL LLVMGetAttributesAtIndex(LLVMValueRef F, LLVMAttributeIndex Idx,
                              LLVMAttributeRef *Attrs) {
  auto AS = unwrap<Function>(F)->getAttributes().getAttributes(Idx);
  for (auto A : AS)
    *Attrs++ = wrap(A);
}

LLVMAttributeRef LLVM_STDCALL LLVMGetEnumAttributeAtIndex(LLVMValueRef F,
                                             LLVMAttributeIndex Idx,
                                             unsigned KindID) {
  return wrap(unwrap<Function>(F)->getAttribute(Idx,
                                                (Attribute::AttrKind)KindID));
}

LLVMAttributeRef LLVM_STDCALL LLVMGetStringAttributeAtIndex(LLVMValueRef F,
                                               LLVMAttributeIndex Idx,
                                               const char *K, unsigned KLen) {
  return wrap(unwrap<Function>(F)->getAttribute(Idx, StringRef(K, KLen)));
}

void LLVM_STDCALL LLVMRemoveEnumAttributeAtIndex(LLVMValueRef F, LLVMAttributeIndex Idx,
                                    unsigned KindID) {
  unwrap<Function>(F)->removeAttribute(Idx, (Attribute::AttrKind)KindID);
}

void LLVM_STDCALL LLVMRemoveStringAttributeAtIndex(LLVMValueRef F, LLVMAttributeIndex Idx,
                                      const char *K, unsigned KLen) {
  unwrap<Function>(F)->removeAttribute(Idx, StringRef(K, KLen));
}

void LLVM_STDCALL LLVMAddTargetDependentFunctionAttr(LLVMValueRef Fn, const char *A,
                                        const char *V) {
  Function *Func = unwrap<Function>(Fn);
  Attribute Attr = Attribute::get(Func->getContext(), A, V);
  Func->addAttribute(AttributeList::FunctionIndex, Attr);
}

/*--.. Operations on parameters ............................................--*/

unsigned LLVM_STDCALL LLVMCountParams(LLVMValueRef FnRef) {
  // This function is strictly redundant to
  //   LLVMCountParamTypes(LLVMGetElementType(LLVMTypeOf(FnRef)))
  return unwrap<Function>(FnRef)->arg_size();
}

void LLVM_STDCALL LLVMGetParams(LLVMValueRef FnRef, LLVMValueRef *ParamRefs) {
  Function *Fn = unwrap<Function>(FnRef);
  for (Function::arg_iterator I = Fn->arg_begin(),
                              E = Fn->arg_end(); I != E; I++)
    *ParamRefs++ = wrap(&*I);
}

LLVMValueRef LLVM_STDCALL LLVMGetParam(LLVMValueRef FnRef, unsigned index) {
  Function *Fn = unwrap<Function>(FnRef);
  return wrap(&Fn->arg_begin()[index]);
}

LLVMValueRef LLVM_STDCALL LLVMGetParamParent(LLVMValueRef V) {
  return wrap(unwrap<Argument>(V)->getParent());
}

LLVMValueRef LLVM_STDCALL LLVMGetFirstParam(LLVMValueRef Fn) {
  Function *Func = unwrap<Function>(Fn);
  Function::arg_iterator I = Func->arg_begin();
  if (I == Func->arg_end())
    return nullptr;
  return wrap(&*I);
}

LLVMValueRef LLVM_STDCALL LLVMGetLastParam(LLVMValueRef Fn) {
  Function *Func = unwrap<Function>(Fn);
  Function::arg_iterator I = Func->arg_end();
  if (I == Func->arg_begin())
    return nullptr;
  return wrap(&*--I);
}

LLVMValueRef LLVM_STDCALL LLVMGetNextParam(LLVMValueRef Arg) {
  Argument *A = unwrap<Argument>(Arg);
  Function *Fn = A->getParent();
  if (A->getArgNo() + 1 >= Fn->arg_size())
    return nullptr;
  return wrap(&Fn->arg_begin()[A->getArgNo() + 1]);
}

LLVMValueRef LLVM_STDCALL LLVMGetPreviousParam(LLVMValueRef Arg) {
  Argument *A = unwrap<Argument>(Arg);
  if (A->getArgNo() == 0)
    return nullptr;
  return wrap(&A->getParent()->arg_begin()[A->getArgNo() - 1]);
}

void LLVM_STDCALL LLVMSetParamAlignment(LLVMValueRef Arg, unsigned align) {
  Argument *A = unwrap<Argument>(Arg);
  A->addAttr(Attribute::getWithAlignment(A->getContext(), align));
}

/*--.. Operations on basic blocks ..........................................--*/

LLVMValueRef LLVM_STDCALL LLVMBasicBlockAsValue(LLVMBasicBlockRef BB) {
  return wrap(static_cast<Value*>(unwrap(BB)));
}

LLVMBool LLVM_STDCALL LLVMValueIsBasicBlock(LLVMValueRef Val) {
  return isa<BasicBlock>(unwrap(Val));
}

LLVMBasicBlockRef LLVM_STDCALL LLVMValueAsBasicBlock(LLVMValueRef Val) {
  return wrap(unwrap<BasicBlock>(Val));
}

const char *LLVM_STDCALL LLVMGetBasicBlockName(LLVMBasicBlockRef BB) {
  return unwrap(BB)->getName().data();
}

LLVMValueRef LLVM_STDCALL LLVMGetBasicBlockParent(LLVMBasicBlockRef BB) {
  return wrap(unwrap(BB)->getParent());
}

LLVMValueRef LLVM_STDCALL LLVMGetBasicBlockTerminator(LLVMBasicBlockRef BB) {
  return wrap(unwrap(BB)->getTerminator());
}

unsigned LLVM_STDCALL LLVMCountBasicBlocks(LLVMValueRef FnRef) {
  return unwrap<Function>(FnRef)->size();
}

void LLVM_STDCALL LLVMGetBasicBlocks(LLVMValueRef FnRef, LLVMBasicBlockRef *BasicBlocksRefs){
  Function *Fn = unwrap<Function>(FnRef);
  for (BasicBlock &BB : *Fn)
    *BasicBlocksRefs++ = wrap(&BB);
}

LLVMBasicBlockRef LLVM_STDCALL LLVMGetEntryBasicBlock(LLVMValueRef Fn) {
  return wrap(&unwrap<Function>(Fn)->getEntryBlock());
}

LLVMBasicBlockRef LLVM_STDCALL LLVMGetFirstBasicBlock(LLVMValueRef Fn) {
  Function *Func = unwrap<Function>(Fn);
  Function::iterator I = Func->begin();
  if (I == Func->end())
    return nullptr;
  return wrap(&*I);
}

LLVMBasicBlockRef LLVM_STDCALL LLVMGetLastBasicBlock(LLVMValueRef Fn) {
  Function *Func = unwrap<Function>(Fn);
  Function::iterator I = Func->end();
  if (I == Func->begin())
    return nullptr;
  return wrap(&*--I);
}

LLVMBasicBlockRef LLVM_STDCALL LLVMGetNextBasicBlock(LLVMBasicBlockRef BB) {
  BasicBlock *Block = unwrap(BB);
  Function::iterator I(Block);
  if (++I == Block->getParent()->end())
    return nullptr;
  return wrap(&*I);
}

LLVMBasicBlockRef LLVM_STDCALL LLVMGetPreviousBasicBlock(LLVMBasicBlockRef BB) {
  BasicBlock *Block = unwrap(BB);
  Function::iterator I(Block);
  if (I == Block->getParent()->begin())
    return nullptr;
  return wrap(&*--I);
}

LLVMBasicBlockRef LLVM_STDCALL LLVMAppendBasicBlockInContext(LLVMContextRef C,
                                                LLVMValueRef FnRef,
                                                const char *Name) {
  return wrap(BasicBlock::Create(*unwrap(C), Name, unwrap<Function>(FnRef)));
}

LLVMBasicBlockRef LLVM_STDCALL LLVMAppendBasicBlock(LLVMValueRef FnRef, const char *Name) {
  return LLVMAppendBasicBlockInContext(LLVMGetGlobalContext(), FnRef, Name);
}

LLVMBasicBlockRef LLVM_STDCALL LLVMInsertBasicBlockInContext(LLVMContextRef C,
                                                LLVMBasicBlockRef BBRef,
                                                const char *Name) {
  BasicBlock *BB = unwrap(BBRef);
  return wrap(BasicBlock::Create(*unwrap(C), Name, BB->getParent(), BB));
}

LLVMBasicBlockRef LLVM_STDCALL LLVMInsertBasicBlock(LLVMBasicBlockRef BBRef,
                                       const char *Name) {
  return LLVMInsertBasicBlockInContext(LLVMGetGlobalContext(), BBRef, Name);
}

void LLVM_STDCALL LLVMDeleteBasicBlock(LLVMBasicBlockRef BBRef) {
  unwrap(BBRef)->eraseFromParent();
}

void LLVM_STDCALL LLVMRemoveBasicBlockFromParent(LLVMBasicBlockRef BBRef) {
  unwrap(BBRef)->removeFromParent();
}

void LLVM_STDCALL LLVMMoveBasicBlockBefore(LLVMBasicBlockRef BB, LLVMBasicBlockRef MovePos) {
  unwrap(BB)->moveBefore(unwrap(MovePos));
}

void LLVM_STDCALL LLVMMoveBasicBlockAfter(LLVMBasicBlockRef BB, LLVMBasicBlockRef MovePos) {
  unwrap(BB)->moveAfter(unwrap(MovePos));
}

/*--.. Operations on instructions ..........................................--*/

LLVMBasicBlockRef LLVM_STDCALL LLVMGetInstructionParent(LLVMValueRef Inst) {
  return wrap(unwrap<Instruction>(Inst)->getParent());
}

LLVMValueRef LLVM_STDCALL LLVMGetFirstInstruction(LLVMBasicBlockRef BB) {
  BasicBlock *Block = unwrap(BB);
  BasicBlock::iterator I = Block->begin();
  if (I == Block->end())
    return nullptr;
  return wrap(&*I);
}

LLVMValueRef LLVM_STDCALL LLVMGetLastInstruction(LLVMBasicBlockRef BB) {
  BasicBlock *Block = unwrap(BB);
  BasicBlock::iterator I = Block->end();
  if (I == Block->begin())
    return nullptr;
  return wrap(&*--I);
}

LLVMValueRef LLVM_STDCALL LLVMGetNextInstruction(LLVMValueRef Inst) {
  Instruction *Instr = unwrap<Instruction>(Inst);
  BasicBlock::iterator I(Instr);
  if (++I == Instr->getParent()->end())
    return nullptr;
  return wrap(&*I);
}

LLVMValueRef LLVM_STDCALL LLVMGetPreviousInstruction(LLVMValueRef Inst) {
  Instruction *Instr = unwrap<Instruction>(Inst);
  BasicBlock::iterator I(Instr);
  if (I == Instr->getParent()->begin())
    return nullptr;
  return wrap(&*--I);
}

void LLVM_STDCALL LLVMInstructionRemoveFromParent(LLVMValueRef Inst) {
  unwrap<Instruction>(Inst)->removeFromParent();
}

void LLVM_STDCALL LLVMInstructionEraseFromParent(LLVMValueRef Inst) {
  unwrap<Instruction>(Inst)->eraseFromParent();
}

LLVMIntPredicate LLVM_STDCALL LLVMGetICmpPredicate(LLVMValueRef Inst) {
  if (ICmpInst *I = dyn_cast<ICmpInst>(unwrap(Inst)))
    return (LLVMIntPredicate)I->getPredicate();
  if (ConstantExpr *CE = dyn_cast<ConstantExpr>(unwrap(Inst)))
    if (CE->getOpcode() == Instruction::ICmp)
      return (LLVMIntPredicate)CE->getPredicate();
  return (LLVMIntPredicate)0;
}

LLVMRealPredicate LLVM_STDCALL LLVMGetFCmpPredicate(LLVMValueRef Inst) {
  if (FCmpInst *I = dyn_cast<FCmpInst>(unwrap(Inst)))
    return (LLVMRealPredicate)I->getPredicate();
  if (ConstantExpr *CE = dyn_cast<ConstantExpr>(unwrap(Inst)))
    if (CE->getOpcode() == Instruction::FCmp)
      return (LLVMRealPredicate)CE->getPredicate();
  return (LLVMRealPredicate)0;
}

LLVMOpcode LLVM_STDCALL LLVMGetInstructionOpcode(LLVMValueRef Inst) {
  if (Instruction *C = dyn_cast<Instruction>(unwrap(Inst)))
    return map_to_llvmopcode(C->getOpcode());
  return (LLVMOpcode)0;
}

LLVMValueRef LLVM_STDCALL LLVMInstructionClone(LLVMValueRef Inst) {
  if (Instruction *C = dyn_cast<Instruction>(unwrap(Inst)))
    return wrap(C->clone());
  return nullptr;
}

/*--.. Call and invoke instructions ........................................--*/

unsigned LLVM_STDCALL LLVMGetNumArgOperands(LLVMValueRef Instr) {
  return CallSite(unwrap<Instruction>(Instr)).getNumArgOperands();
}

unsigned LLVM_STDCALL LLVMGetInstructionCallConv(LLVMValueRef Instr) {
  return CallSite(unwrap<Instruction>(Instr)).getCallingConv();
}

void LLVM_STDCALL LLVMSetInstructionCallConv(LLVMValueRef Instr, unsigned CC) {
  return CallSite(unwrap<Instruction>(Instr))
    .setCallingConv(static_cast<CallingConv::ID>(CC));
}

void LLVM_STDCALL LLVMSetInstrParamAlignment(LLVMValueRef Instr, unsigned index,
                                unsigned align) {
  CallSite Call = CallSite(unwrap<Instruction>(Instr));
  Attribute AlignAttr = Attribute::getWithAlignment(Call->getContext(), align);
  Call.addAttribute(index, AlignAttr);
}

void LLVM_STDCALL LLVMAddCallSiteAttribute(LLVMValueRef C, LLVMAttributeIndex Idx,
                              LLVMAttributeRef A) {
  CallSite(unwrap<Instruction>(C)).addAttribute(Idx, unwrap(A));
}

unsigned LLVM_STDCALL LLVMGetCallSiteAttributeCount(LLVMValueRef C,
                                       LLVMAttributeIndex Idx) {
  auto CS = CallSite(unwrap<Instruction>(C));
  auto AS = CS.getAttributes().getAttributes(Idx);
  return AS.getNumAttributes();
}

void LLVM_STDCALL LLVMGetCallSiteAttributes(LLVMValueRef C, LLVMAttributeIndex Idx,
                               LLVMAttributeRef *Attrs) {
  auto CS = CallSite(unwrap<Instruction>(C));
  auto AS = CS.getAttributes().getAttributes(Idx);
  for (auto A : AS)
    *Attrs++ = wrap(A);
}

LLVMAttributeRef LLVM_STDCALL LLVMGetCallSiteEnumAttribute(LLVMValueRef C,
                                              LLVMAttributeIndex Idx,
                                              unsigned KindID) {
  return wrap(CallSite(unwrap<Instruction>(C))
    .getAttribute(Idx, (Attribute::AttrKind)KindID));
}

LLVMAttributeRef LLVM_STDCALL LLVMGetCallSiteStringAttribute(LLVMValueRef C,
                                                LLVMAttributeIndex Idx,
                                                const char *K, unsigned KLen) {
  return wrap(CallSite(unwrap<Instruction>(C))
    .getAttribute(Idx, StringRef(K, KLen)));
}

void LLVM_STDCALL LLVMRemoveCallSiteEnumAttribute(LLVMValueRef C, LLVMAttributeIndex Idx,
                                     unsigned KindID) {
  CallSite(unwrap<Instruction>(C))
    .removeAttribute(Idx, (Attribute::AttrKind)KindID);
}

void LLVM_STDCALL LLVMRemoveCallSiteStringAttribute(LLVMValueRef C, LLVMAttributeIndex Idx,
                                       const char *K, unsigned KLen) {
  CallSite(unwrap<Instruction>(C)).removeAttribute(Idx, StringRef(K, KLen));
}

LLVMValueRef LLVM_STDCALL LLVMGetCalledValue(LLVMValueRef Instr) {
  return wrap(CallSite(unwrap<Instruction>(Instr)).getCalledValue());
}

/*--.. Operations on call instructions (only) ..............................--*/

LLVMBool LLVM_STDCALL LLVMIsTailCall(LLVMValueRef Call) {
  return unwrap<CallInst>(Call)->isTailCall();
}

void LLVM_STDCALL LLVMSetTailCall(LLVMValueRef Call, LLVMBool isTailCall) {
  unwrap<CallInst>(Call)->setTailCall(isTailCall);
}

/*--.. Operations on invoke instructions (only) ............................--*/

LLVMBasicBlockRef LLVM_STDCALL LLVMGetNormalDest(LLVMValueRef Invoke) {
  return wrap(unwrap<InvokeInst>(Invoke)->getNormalDest());
}

LLVMBasicBlockRef LLVM_STDCALL LLVMGetUnwindDest(LLVMValueRef Invoke) {
  return wrap(unwrap<InvokeInst>(Invoke)->getUnwindDest());
}

void LLVM_STDCALL LLVMSetNormalDest(LLVMValueRef Invoke, LLVMBasicBlockRef B) {
  unwrap<InvokeInst>(Invoke)->setNormalDest(unwrap(B));
}

void LLVM_STDCALL LLVMSetUnwindDest(LLVMValueRef Invoke, LLVMBasicBlockRef B) {
  unwrap<InvokeInst>(Invoke)->setUnwindDest(unwrap(B));
}

/*--.. Operations on terminators ...........................................--*/

unsigned LLVM_STDCALL LLVMGetNumSuccessors(LLVMValueRef Term) {
  return unwrap<TerminatorInst>(Term)->getNumSuccessors();
}

LLVMBasicBlockRef LLVM_STDCALL LLVMGetSuccessor(LLVMValueRef Term, unsigned i) {
  return wrap(unwrap<TerminatorInst>(Term)->getSuccessor(i));
}

void LLVM_STDCALL LLVMSetSuccessor(LLVMValueRef Term, unsigned i, LLVMBasicBlockRef block) {
  return unwrap<TerminatorInst>(Term)->setSuccessor(i,unwrap(block));
}

/*--.. Operations on branch instructions (only) ............................--*/

LLVMBool LLVM_STDCALL LLVMIsConditional(LLVMValueRef Branch) {
  return unwrap<BranchInst>(Branch)->isConditional();
}

LLVMValueRef LLVM_STDCALL LLVMGetCondition(LLVMValueRef Branch) {
  return wrap(unwrap<BranchInst>(Branch)->getCondition());
}

void LLVM_STDCALL LLVMSetCondition(LLVMValueRef Branch, LLVMValueRef Cond) {
  return unwrap<BranchInst>(Branch)->setCondition(unwrap(Cond));
}

/*--.. Operations on switch instructions (only) ............................--*/

LLVMBasicBlockRef LLVM_STDCALL LLVMGetSwitchDefaultDest(LLVMValueRef Switch) {
  return wrap(unwrap<SwitchInst>(Switch)->getDefaultDest());
}

/*--.. Operations on alloca instructions (only) ............................--*/

LLVMTypeRef LLVM_STDCALL LLVMGetAllocatedType(LLVMValueRef Alloca) {
  return wrap(unwrap<AllocaInst>(Alloca)->getAllocatedType());
}

/*--.. Operations on gep instructions (only) ...............................--*/

LLVMBool LLVM_STDCALL LLVMIsInBounds(LLVMValueRef GEP) {
  return unwrap<GetElementPtrInst>(GEP)->isInBounds();
}

void LLVM_STDCALL LLVMSetIsInBounds(LLVMValueRef GEP, LLVMBool InBounds) {
  return unwrap<GetElementPtrInst>(GEP)->setIsInBounds(InBounds);
}

/*--.. Operations on phi nodes .............................................--*/

void LLVM_STDCALL LLVMAddIncoming(LLVMValueRef PhiNode, LLVMValueRef *IncomingValues,
                     LLVMBasicBlockRef *IncomingBlocks, unsigned Count) {
  PHINode *PhiVal = unwrap<PHINode>(PhiNode);
  for (unsigned I = 0; I != Count; ++I)
    PhiVal->addIncoming(unwrap(IncomingValues[I]), unwrap(IncomingBlocks[I]));
}

unsigned LLVM_STDCALL LLVMCountIncoming(LLVMValueRef PhiNode) {
  return unwrap<PHINode>(PhiNode)->getNumIncomingValues();
}

LLVMValueRef LLVM_STDCALL LLVMGetIncomingValue(LLVMValueRef PhiNode, unsigned Index) {
  return wrap(unwrap<PHINode>(PhiNode)->getIncomingValue(Index));
}

LLVMBasicBlockRef LLVM_STDCALL LLVMGetIncomingBlock(LLVMValueRef PhiNode, unsigned Index) {
  return wrap(unwrap<PHINode>(PhiNode)->getIncomingBlock(Index));
}

/*--.. Operations on extractvalue and insertvalue nodes ....................--*/

unsigned LLVM_STDCALL LLVMGetNumIndices(LLVMValueRef Inst) {
  auto *I = unwrap(Inst);
  if (auto *GEP = dyn_cast<GetElementPtrInst>(I))
    return GEP->getNumIndices();
  if (auto *EV = dyn_cast<ExtractValueInst>(I))
    return EV->getNumIndices();
  if (auto *IV = dyn_cast<InsertValueInst>(I))
    return IV->getNumIndices();
  llvm_unreachable(
    "LLVMGetNumIndices applies only to extractvalue and insertvalue!");
}

const unsigned *LLVM_STDCALL LLVMGetIndices(LLVMValueRef Inst) {
  auto *I = unwrap(Inst);
  if (auto *EV = dyn_cast<ExtractValueInst>(I))
    return EV->getIndices().data();
  if (auto *IV = dyn_cast<InsertValueInst>(I))
    return IV->getIndices().data();
  llvm_unreachable(
    "LLVMGetIndices applies only to extractvalue and insertvalue!");
}


/*===-- Instruction builders ----------------------------------------------===*/

LLVMBuilderRef LLVM_STDCALL LLVMCreateBuilderInContext(LLVMContextRef C) {
  return wrap(new IRBuilder<>(*unwrap(C)));
}

LLVMBuilderRef LLVM_STDCALL LLVMCreateBuilder(void) {
  return LLVMCreateBuilderInContext(LLVMGetGlobalContext());
}

void LLVM_STDCALL LLVMPositionBuilder(LLVMBuilderRef Builder, LLVMBasicBlockRef Block,
                         LLVMValueRef Instr) {
  BasicBlock *BB = unwrap(Block);
  auto I = Instr ? unwrap<Instruction>(Instr)->getIterator() : BB->end();
  unwrap(Builder)->SetInsertPoint(BB, I);
}

void LLVM_STDCALL LLVMPositionBuilderBefore(LLVMBuilderRef Builder, LLVMValueRef Instr) {
  Instruction *I = unwrap<Instruction>(Instr);
  unwrap(Builder)->SetInsertPoint(I->getParent(), I->getIterator());
}

void LLVM_STDCALL LLVMPositionBuilderAtEnd(LLVMBuilderRef Builder, LLVMBasicBlockRef Block) {
  BasicBlock *BB = unwrap(Block);
  unwrap(Builder)->SetInsertPoint(BB);
}

LLVMBasicBlockRef LLVM_STDCALL LLVMGetInsertBlock(LLVMBuilderRef Builder) {
   return wrap(unwrap(Builder)->GetInsertBlock());
}

void LLVM_STDCALL LLVMClearInsertionPosition(LLVMBuilderRef Builder) {
  unwrap(Builder)->ClearInsertionPoint();
}

void LLVM_STDCALL LLVMInsertIntoBuilder(LLVMBuilderRef Builder, LLVMValueRef Instr) {
  unwrap(Builder)->Insert(unwrap<Instruction>(Instr));
}

void LLVM_STDCALL LLVMInsertIntoBuilderWithName(LLVMBuilderRef Builder, LLVMValueRef Instr,
                                   const char *Name) {
  unwrap(Builder)->Insert(unwrap<Instruction>(Instr), Name);
}

void LLVM_STDCALL LLVMDisposeBuilder(LLVMBuilderRef Builder) {
  delete unwrap(Builder);
}

/*--.. Metadata builders ...................................................--*/

void LLVM_STDCALL LLVMSetCurrentDebugLocation(LLVMBuilderRef Builder, LLVMValueRef L) {
  MDNode *Loc =
      L ? cast<MDNode>(unwrap<MetadataAsValue>(L)->getMetadata()) : nullptr;
  unwrap(Builder)->SetCurrentDebugLocation(DebugLoc(Loc));
}

LLVMValueRef LLVM_STDCALL LLVMGetCurrentDebugLocation(LLVMBuilderRef Builder) {
  LLVMContext &Context = unwrap(Builder)->getContext();
  return wrap(MetadataAsValue::get(
      Context, unwrap(Builder)->getCurrentDebugLocation().getAsMDNode()));
}

void LLVM_STDCALL LLVMSetInstDebugLocation(LLVMBuilderRef Builder, LLVMValueRef Inst) {
  unwrap(Builder)->SetInstDebugLocation(unwrap<Instruction>(Inst));
}


/*--.. Instruction builders ................................................--*/

LLVMValueRef LLVM_STDCALL LLVMBuildRetVoid(LLVMBuilderRef B) {
  return wrap(unwrap(B)->CreateRetVoid());
}

LLVMValueRef LLVM_STDCALL LLVMBuildRet(LLVMBuilderRef B, LLVMValueRef V) {
  return wrap(unwrap(B)->CreateRet(unwrap(V)));
}

LLVMValueRef LLVM_STDCALL LLVMBuildAggregateRet(LLVMBuilderRef B, LLVMValueRef *RetVals,
                                   unsigned N) {
  return wrap(unwrap(B)->CreateAggregateRet(unwrap(RetVals), N));
}

LLVMValueRef LLVM_STDCALL LLVMBuildBr(LLVMBuilderRef B, LLVMBasicBlockRef Dest) {
  return wrap(unwrap(B)->CreateBr(unwrap(Dest)));
}

LLVMValueRef LLVM_STDCALL LLVMBuildCondBr(LLVMBuilderRef B, LLVMValueRef If,
                             LLVMBasicBlockRef Then, LLVMBasicBlockRef Else) {
  return wrap(unwrap(B)->CreateCondBr(unwrap(If), unwrap(Then), unwrap(Else)));
}

LLVMValueRef LLVM_STDCALL LLVMBuildSwitch(LLVMBuilderRef B, LLVMValueRef V,
                             LLVMBasicBlockRef Else, unsigned NumCases) {
  return wrap(unwrap(B)->CreateSwitch(unwrap(V), unwrap(Else), NumCases));
}

LLVMValueRef LLVM_STDCALL LLVMBuildIndirectBr(LLVMBuilderRef B, LLVMValueRef Addr,
                                 unsigned NumDests) {
  return wrap(unwrap(B)->CreateIndirectBr(unwrap(Addr), NumDests));
}

LLVMValueRef LLVM_STDCALL LLVMBuildInvoke(LLVMBuilderRef B, LLVMValueRef Fn,
                             LLVMValueRef *Args, unsigned NumArgs,
                             LLVMBasicBlockRef Then, LLVMBasicBlockRef Catch,
                             const char *Name) {
  return wrap(unwrap(B)->CreateInvoke(unwrap(Fn), unwrap(Then), unwrap(Catch),
                                      makeArrayRef(unwrap(Args), NumArgs),
                                      Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildLandingPad(LLVMBuilderRef B, LLVMTypeRef Ty,
                                 LLVMValueRef PersFn, unsigned NumClauses,
                                 const char *Name) {
  // The personality used to live on the landingpad instruction, but now it
  // lives on the parent function. For compatibility, take the provided
  // personality and put it on the parent function.
  if (PersFn)
    unwrap(B)->GetInsertBlock()->getParent()->setPersonalityFn(
        cast<Function>(unwrap(PersFn)));
  return wrap(unwrap(B)->CreateLandingPad(unwrap(Ty), NumClauses, Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildResume(LLVMBuilderRef B, LLVMValueRef Exn) {
  return wrap(unwrap(B)->CreateResume(unwrap(Exn)));
}

LLVMValueRef LLVM_STDCALL LLVMBuildUnreachable(LLVMBuilderRef B) {
  return wrap(unwrap(B)->CreateUnreachable());
}

void LLVM_STDCALL LLVMAddCase(LLVMValueRef Switch, LLVMValueRef OnVal,
                 LLVMBasicBlockRef Dest) {
  unwrap<SwitchInst>(Switch)->addCase(unwrap<ConstantInt>(OnVal), unwrap(Dest));
}

void LLVM_STDCALL LLVMAddDestination(LLVMValueRef IndirectBr, LLVMBasicBlockRef Dest) {
  unwrap<IndirectBrInst>(IndirectBr)->addDestination(unwrap(Dest));
}

unsigned LLVM_STDCALL LLVMGetNumClauses(LLVMValueRef LandingPad) {
  return unwrap<LandingPadInst>(LandingPad)->getNumClauses();
}

LLVMValueRef LLVM_STDCALL LLVMGetClause(LLVMValueRef LandingPad, unsigned Idx) {
  return wrap(unwrap<LandingPadInst>(LandingPad)->getClause(Idx));
}

void LLVM_STDCALL LLVMAddClause(LLVMValueRef LandingPad, LLVMValueRef ClauseVal) {
  unwrap<LandingPadInst>(LandingPad)->
    addClause(cast<Constant>(unwrap(ClauseVal)));
}

LLVMBool LLVM_STDCALL LLVMIsCleanup(LLVMValueRef LandingPad) {
  return unwrap<LandingPadInst>(LandingPad)->isCleanup();
}

void LLVM_STDCALL LLVMSetCleanup(LLVMValueRef LandingPad, LLVMBool Val) {
  unwrap<LandingPadInst>(LandingPad)->setCleanup(Val);
}

/*--.. Arithmetic ..........................................................--*/

LLVMValueRef LLVM_STDCALL LLVMBuildAdd(LLVMBuilderRef B, LLVMValueRef LHS, LLVMValueRef RHS,
                          const char *Name) {
  return wrap(unwrap(B)->CreateAdd(unwrap(LHS), unwrap(RHS), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildNSWAdd(LLVMBuilderRef B, LLVMValueRef LHS, LLVMValueRef RHS,
                          const char *Name) {
  return wrap(unwrap(B)->CreateNSWAdd(unwrap(LHS), unwrap(RHS), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildNUWAdd(LLVMBuilderRef B, LLVMValueRef LHS, LLVMValueRef RHS,
                          const char *Name) {
  return wrap(unwrap(B)->CreateNUWAdd(unwrap(LHS), unwrap(RHS), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildFAdd(LLVMBuilderRef B, LLVMValueRef LHS, LLVMValueRef RHS,
                          const char *Name) {
  return wrap(unwrap(B)->CreateFAdd(unwrap(LHS), unwrap(RHS), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildSub(LLVMBuilderRef B, LLVMValueRef LHS, LLVMValueRef RHS,
                          const char *Name) {
  return wrap(unwrap(B)->CreateSub(unwrap(LHS), unwrap(RHS), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildNSWSub(LLVMBuilderRef B, LLVMValueRef LHS, LLVMValueRef RHS,
                          const char *Name) {
  return wrap(unwrap(B)->CreateNSWSub(unwrap(LHS), unwrap(RHS), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildNUWSub(LLVMBuilderRef B, LLVMValueRef LHS, LLVMValueRef RHS,
                          const char *Name) {
  return wrap(unwrap(B)->CreateNUWSub(unwrap(LHS), unwrap(RHS), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildFSub(LLVMBuilderRef B, LLVMValueRef LHS, LLVMValueRef RHS,
                          const char *Name) {
  return wrap(unwrap(B)->CreateFSub(unwrap(LHS), unwrap(RHS), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildMul(LLVMBuilderRef B, LLVMValueRef LHS, LLVMValueRef RHS,
                          const char *Name) {
  return wrap(unwrap(B)->CreateMul(unwrap(LHS), unwrap(RHS), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildNSWMul(LLVMBuilderRef B, LLVMValueRef LHS, LLVMValueRef RHS,
                          const char *Name) {
  return wrap(unwrap(B)->CreateNSWMul(unwrap(LHS), unwrap(RHS), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildNUWMul(LLVMBuilderRef B, LLVMValueRef LHS, LLVMValueRef RHS,
                          const char *Name) {
  return wrap(unwrap(B)->CreateNUWMul(unwrap(LHS), unwrap(RHS), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildFMul(LLVMBuilderRef B, LLVMValueRef LHS, LLVMValueRef RHS,
                          const char *Name) {
  return wrap(unwrap(B)->CreateFMul(unwrap(LHS), unwrap(RHS), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildUDiv(LLVMBuilderRef B, LLVMValueRef LHS, LLVMValueRef RHS,
                           const char *Name) {
  return wrap(unwrap(B)->CreateUDiv(unwrap(LHS), unwrap(RHS), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildExactUDiv(LLVMBuilderRef B, LLVMValueRef LHS,
                                LLVMValueRef RHS, const char *Name) {
  return wrap(unwrap(B)->CreateExactUDiv(unwrap(LHS), unwrap(RHS), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildSDiv(LLVMBuilderRef B, LLVMValueRef LHS, LLVMValueRef RHS,
                           const char *Name) {
  return wrap(unwrap(B)->CreateSDiv(unwrap(LHS), unwrap(RHS), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildExactSDiv(LLVMBuilderRef B, LLVMValueRef LHS,
                                LLVMValueRef RHS, const char *Name) {
  return wrap(unwrap(B)->CreateExactSDiv(unwrap(LHS), unwrap(RHS), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildFDiv(LLVMBuilderRef B, LLVMValueRef LHS, LLVMValueRef RHS,
                           const char *Name) {
  return wrap(unwrap(B)->CreateFDiv(unwrap(LHS), unwrap(RHS), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildURem(LLVMBuilderRef B, LLVMValueRef LHS, LLVMValueRef RHS,
                           const char *Name) {
  return wrap(unwrap(B)->CreateURem(unwrap(LHS), unwrap(RHS), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildSRem(LLVMBuilderRef B, LLVMValueRef LHS, LLVMValueRef RHS,
                           const char *Name) {
  return wrap(unwrap(B)->CreateSRem(unwrap(LHS), unwrap(RHS), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildFRem(LLVMBuilderRef B, LLVMValueRef LHS, LLVMValueRef RHS,
                           const char *Name) {
  return wrap(unwrap(B)->CreateFRem(unwrap(LHS), unwrap(RHS), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildShl(LLVMBuilderRef B, LLVMValueRef LHS, LLVMValueRef RHS,
                          const char *Name) {
  return wrap(unwrap(B)->CreateShl(unwrap(LHS), unwrap(RHS), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildLShr(LLVMBuilderRef B, LLVMValueRef LHS, LLVMValueRef RHS,
                           const char *Name) {
  return wrap(unwrap(B)->CreateLShr(unwrap(LHS), unwrap(RHS), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildAShr(LLVMBuilderRef B, LLVMValueRef LHS, LLVMValueRef RHS,
                           const char *Name) {
  return wrap(unwrap(B)->CreateAShr(unwrap(LHS), unwrap(RHS), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildAnd(LLVMBuilderRef B, LLVMValueRef LHS, LLVMValueRef RHS,
                          const char *Name) {
  return wrap(unwrap(B)->CreateAnd(unwrap(LHS), unwrap(RHS), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildOr(LLVMBuilderRef B, LLVMValueRef LHS, LLVMValueRef RHS,
                         const char *Name) {
  return wrap(unwrap(B)->CreateOr(unwrap(LHS), unwrap(RHS), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildXor(LLVMBuilderRef B, LLVMValueRef LHS, LLVMValueRef RHS,
                          const char *Name) {
  return wrap(unwrap(B)->CreateXor(unwrap(LHS), unwrap(RHS), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildBinOp(LLVMBuilderRef B, LLVMOpcode Op,
                            LLVMValueRef LHS, LLVMValueRef RHS,
                            const char *Name) {
  return wrap(unwrap(B)->CreateBinOp(Instruction::BinaryOps(map_from_llvmopcode(Op)), unwrap(LHS),
                                     unwrap(RHS), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildNeg(LLVMBuilderRef B, LLVMValueRef V, const char *Name) {
  return wrap(unwrap(B)->CreateNeg(unwrap(V), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildNSWNeg(LLVMBuilderRef B, LLVMValueRef V,
                             const char *Name) {
  return wrap(unwrap(B)->CreateNSWNeg(unwrap(V), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildNUWNeg(LLVMBuilderRef B, LLVMValueRef V,
                             const char *Name) {
  return wrap(unwrap(B)->CreateNUWNeg(unwrap(V), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildFNeg(LLVMBuilderRef B, LLVMValueRef V, const char *Name) {
  return wrap(unwrap(B)->CreateFNeg(unwrap(V), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildNot(LLVMBuilderRef B, LLVMValueRef V, const char *Name) {
  return wrap(unwrap(B)->CreateNot(unwrap(V), Name));
}

/*--.. Memory ..............................................................--*/

LLVMValueRef LLVM_STDCALL LLVMBuildMalloc(LLVMBuilderRef B, LLVMTypeRef Ty,
                             const char *Name) {
  Type* ITy = Type::getInt32Ty(unwrap(B)->GetInsertBlock()->getContext());
  Constant* AllocSize = ConstantExpr::getSizeOf(unwrap(Ty));
  AllocSize = ConstantExpr::getTruncOrBitCast(AllocSize, ITy);
  Instruction* Malloc = CallInst::CreateMalloc(unwrap(B)->GetInsertBlock(),
                                               ITy, unwrap(Ty), AllocSize,
                                               nullptr, nullptr, "");
  return wrap(unwrap(B)->Insert(Malloc, Twine(Name)));
}

LLVMValueRef LLVM_STDCALL LLVMBuildArrayMalloc(LLVMBuilderRef B, LLVMTypeRef Ty,
                                  LLVMValueRef Val, const char *Name) {
  Type* ITy = Type::getInt32Ty(unwrap(B)->GetInsertBlock()->getContext());
  Constant* AllocSize = ConstantExpr::getSizeOf(unwrap(Ty));
  AllocSize = ConstantExpr::getTruncOrBitCast(AllocSize, ITy);
  Instruction* Malloc = CallInst::CreateMalloc(unwrap(B)->GetInsertBlock(),
                                               ITy, unwrap(Ty), AllocSize,
                                               unwrap(Val), nullptr, "");
  return wrap(unwrap(B)->Insert(Malloc, Twine(Name)));
}

LLVMValueRef LLVM_STDCALL LLVMBuildAlloca(LLVMBuilderRef B, LLVMTypeRef Ty,
                             const char *Name) {
  return wrap(unwrap(B)->CreateAlloca(unwrap(Ty), nullptr, Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildArrayAlloca(LLVMBuilderRef B, LLVMTypeRef Ty,
                                  LLVMValueRef Val, const char *Name) {
  return wrap(unwrap(B)->CreateAlloca(unwrap(Ty), unwrap(Val), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildFree(LLVMBuilderRef B, LLVMValueRef PointerVal) {
  return wrap(unwrap(B)->Insert(
     CallInst::CreateFree(unwrap(PointerVal), unwrap(B)->GetInsertBlock())));
}

LLVMValueRef LLVM_STDCALL LLVMBuildLoad(LLVMBuilderRef B, LLVMValueRef PointerVal,
                           const char *Name) {
  return wrap(unwrap(B)->CreateLoad(unwrap(PointerVal), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildStore(LLVMBuilderRef B, LLVMValueRef Val,
                            LLVMValueRef PointerVal) {
  return wrap(unwrap(B)->CreateStore(unwrap(Val), unwrap(PointerVal)));
}

static AtomicOrdering mapFromLLVMOrdering(LLVMAtomicOrdering Ordering) {
  switch (Ordering) {
    case LLVMAtomicOrderingNotAtomic: return AtomicOrdering::NotAtomic;
    case LLVMAtomicOrderingUnordered: return AtomicOrdering::Unordered;
    case LLVMAtomicOrderingMonotonic: return AtomicOrdering::Monotonic;
    case LLVMAtomicOrderingAcquire: return AtomicOrdering::Acquire;
    case LLVMAtomicOrderingRelease: return AtomicOrdering::Release;
    case LLVMAtomicOrderingAcquireRelease:
      return AtomicOrdering::AcquireRelease;
    case LLVMAtomicOrderingSequentiallyConsistent:
      return AtomicOrdering::SequentiallyConsistent;
  }

  llvm_unreachable("Invalid LLVMAtomicOrdering value!");
}

static LLVMAtomicOrdering mapToLLVMOrdering(AtomicOrdering Ordering) {
  switch (Ordering) {
    case AtomicOrdering::NotAtomic: return LLVMAtomicOrderingNotAtomic;
    case AtomicOrdering::Unordered: return LLVMAtomicOrderingUnordered;
    case AtomicOrdering::Monotonic: return LLVMAtomicOrderingMonotonic;
    case AtomicOrdering::Acquire: return LLVMAtomicOrderingAcquire;
    case AtomicOrdering::Release: return LLVMAtomicOrderingRelease;
    case AtomicOrdering::AcquireRelease:
      return LLVMAtomicOrderingAcquireRelease;
    case AtomicOrdering::SequentiallyConsistent:
      return LLVMAtomicOrderingSequentiallyConsistent;
  }

  llvm_unreachable("Invalid AtomicOrdering value!");
}

// TODO: Should this and other atomic instructions support building with
// "syncscope"?
LLVMValueRef LLVM_STDCALL LLVMBuildFence(LLVMBuilderRef B, LLVMAtomicOrdering Ordering,
                            LLVMBool isSingleThread, const char *Name) {
  return wrap(
    unwrap(B)->CreateFence(mapFromLLVMOrdering(Ordering),
                           isSingleThread ? SyncScope::SingleThread
                                          : SyncScope::System,
                           Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildGEP(LLVMBuilderRef B, LLVMValueRef Pointer,
                          LLVMValueRef *Indices, unsigned NumIndices,
                          const char *Name) {
  ArrayRef<Value *> IdxList(unwrap(Indices), NumIndices);
  return wrap(unwrap(B)->CreateGEP(nullptr, unwrap(Pointer), IdxList, Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildInBoundsGEP(LLVMBuilderRef B, LLVMValueRef Pointer,
                                  LLVMValueRef *Indices, unsigned NumIndices,
                                  const char *Name) {
  ArrayRef<Value *> IdxList(unwrap(Indices), NumIndices);
  return wrap(
      unwrap(B)->CreateInBoundsGEP(nullptr, unwrap(Pointer), IdxList, Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildStructGEP(LLVMBuilderRef B, LLVMValueRef Pointer,
                                unsigned Idx, const char *Name) {
  return wrap(unwrap(B)->CreateStructGEP(nullptr, unwrap(Pointer), Idx, Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildGlobalString(LLVMBuilderRef B, const char *Str,
                                   const char *Name) {
  return wrap(unwrap(B)->CreateGlobalString(Str, Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildGlobalStringPtr(LLVMBuilderRef B, const char *Str,
                                      const char *Name) {
  return wrap(unwrap(B)->CreateGlobalStringPtr(Str, Name));
}

LLVMBool LLVM_STDCALL LLVMGetVolatile(LLVMValueRef MemAccessInst) {
  Value *P = unwrap<Value>(MemAccessInst);
  if (LoadInst *LI = dyn_cast<LoadInst>(P))
    return LI->isVolatile();
  return cast<StoreInst>(P)->isVolatile();
}

void LLVM_STDCALL LLVMSetVolatile(LLVMValueRef MemAccessInst, LLVMBool isVolatile) {
  Value *P = unwrap<Value>(MemAccessInst);
  if (LoadInst *LI = dyn_cast<LoadInst>(P))
    return LI->setVolatile(isVolatile);
  return cast<StoreInst>(P)->setVolatile(isVolatile);
}

LLVMAtomicOrdering LLVM_STDCALL LLVMGetOrdering(LLVMValueRef MemAccessInst) {
  Value *P = unwrap<Value>(MemAccessInst);
  AtomicOrdering O;
  if (LoadInst *LI = dyn_cast<LoadInst>(P))
    O = LI->getOrdering();
  else
    O = cast<StoreInst>(P)->getOrdering();
  return mapToLLVMOrdering(O);
}

void LLVM_STDCALL LLVMSetOrdering(LLVMValueRef MemAccessInst, LLVMAtomicOrdering Ordering) {
  Value *P = unwrap<Value>(MemAccessInst);
  AtomicOrdering O = mapFromLLVMOrdering(Ordering);

  if (LoadInst *LI = dyn_cast<LoadInst>(P))
    return LI->setOrdering(O);
  return cast<StoreInst>(P)->setOrdering(O);
}

/*--.. Casts ...............................................................--*/

LLVMValueRef LLVM_STDCALL LLVMBuildTrunc(LLVMBuilderRef B, LLVMValueRef Val,
                            LLVMTypeRef DestTy, const char *Name) {
  return wrap(unwrap(B)->CreateTrunc(unwrap(Val), unwrap(DestTy), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildZExt(LLVMBuilderRef B, LLVMValueRef Val,
                           LLVMTypeRef DestTy, const char *Name) {
  return wrap(unwrap(B)->CreateZExt(unwrap(Val), unwrap(DestTy), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildSExt(LLVMBuilderRef B, LLVMValueRef Val,
                           LLVMTypeRef DestTy, const char *Name) {
  return wrap(unwrap(B)->CreateSExt(unwrap(Val), unwrap(DestTy), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildFPToUI(LLVMBuilderRef B, LLVMValueRef Val,
                             LLVMTypeRef DestTy, const char *Name) {
  return wrap(unwrap(B)->CreateFPToUI(unwrap(Val), unwrap(DestTy), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildFPToSI(LLVMBuilderRef B, LLVMValueRef Val,
                             LLVMTypeRef DestTy, const char *Name) {
  return wrap(unwrap(B)->CreateFPToSI(unwrap(Val), unwrap(DestTy), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildUIToFP(LLVMBuilderRef B, LLVMValueRef Val,
                             LLVMTypeRef DestTy, const char *Name) {
  return wrap(unwrap(B)->CreateUIToFP(unwrap(Val), unwrap(DestTy), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildSIToFP(LLVMBuilderRef B, LLVMValueRef Val,
                             LLVMTypeRef DestTy, const char *Name) {
  return wrap(unwrap(B)->CreateSIToFP(unwrap(Val), unwrap(DestTy), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildFPTrunc(LLVMBuilderRef B, LLVMValueRef Val,
                              LLVMTypeRef DestTy, const char *Name) {
  return wrap(unwrap(B)->CreateFPTrunc(unwrap(Val), unwrap(DestTy), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildFPExt(LLVMBuilderRef B, LLVMValueRef Val,
                            LLVMTypeRef DestTy, const char *Name) {
  return wrap(unwrap(B)->CreateFPExt(unwrap(Val), unwrap(DestTy), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildPtrToInt(LLVMBuilderRef B, LLVMValueRef Val,
                               LLVMTypeRef DestTy, const char *Name) {
  return wrap(unwrap(B)->CreatePtrToInt(unwrap(Val), unwrap(DestTy), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildIntToPtr(LLVMBuilderRef B, LLVMValueRef Val,
                               LLVMTypeRef DestTy, const char *Name) {
  return wrap(unwrap(B)->CreateIntToPtr(unwrap(Val), unwrap(DestTy), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildBitCast(LLVMBuilderRef B, LLVMValueRef Val,
                              LLVMTypeRef DestTy, const char *Name) {
  return wrap(unwrap(B)->CreateBitCast(unwrap(Val), unwrap(DestTy), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildAddrSpaceCast(LLVMBuilderRef B, LLVMValueRef Val,
                                    LLVMTypeRef DestTy, const char *Name) {
  return wrap(unwrap(B)->CreateAddrSpaceCast(unwrap(Val), unwrap(DestTy), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildZExtOrBitCast(LLVMBuilderRef B, LLVMValueRef Val,
                                    LLVMTypeRef DestTy, const char *Name) {
  return wrap(unwrap(B)->CreateZExtOrBitCast(unwrap(Val), unwrap(DestTy),
                                             Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildSExtOrBitCast(LLVMBuilderRef B, LLVMValueRef Val,
                                    LLVMTypeRef DestTy, const char *Name) {
  return wrap(unwrap(B)->CreateSExtOrBitCast(unwrap(Val), unwrap(DestTy),
                                             Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildTruncOrBitCast(LLVMBuilderRef B, LLVMValueRef Val,
                                     LLVMTypeRef DestTy, const char *Name) {
  return wrap(unwrap(B)->CreateTruncOrBitCast(unwrap(Val), unwrap(DestTy),
                                              Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildCast(LLVMBuilderRef B, LLVMOpcode Op, LLVMValueRef Val,
                           LLVMTypeRef DestTy, const char *Name) {
  return wrap(unwrap(B)->CreateCast(Instruction::CastOps(map_from_llvmopcode(Op)), unwrap(Val),
                                    unwrap(DestTy), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildPointerCast(LLVMBuilderRef B, LLVMValueRef Val,
                                  LLVMTypeRef DestTy, const char *Name) {
  return wrap(unwrap(B)->CreatePointerCast(unwrap(Val), unwrap(DestTy), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildIntCast(LLVMBuilderRef B, LLVMValueRef Val,
                              LLVMTypeRef DestTy, const char *Name) {
  return wrap(unwrap(B)->CreateIntCast(unwrap(Val), unwrap(DestTy),
                                       /*isSigned*/true, Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildFPCast(LLVMBuilderRef B, LLVMValueRef Val,
                             LLVMTypeRef DestTy, const char *Name) {
  return wrap(unwrap(B)->CreateFPCast(unwrap(Val), unwrap(DestTy), Name));
}

/*--.. Comparisons .........................................................--*/

LLVMValueRef LLVM_STDCALL LLVMBuildICmp(LLVMBuilderRef B, LLVMIntPredicate Op,
                           LLVMValueRef LHS, LLVMValueRef RHS,
                           const char *Name) {
  return wrap(unwrap(B)->CreateICmp(static_cast<ICmpInst::Predicate>(Op),
                                    unwrap(LHS), unwrap(RHS), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildFCmp(LLVMBuilderRef B, LLVMRealPredicate Op,
                           LLVMValueRef LHS, LLVMValueRef RHS,
                           const char *Name) {
  return wrap(unwrap(B)->CreateFCmp(static_cast<FCmpInst::Predicate>(Op),
                                    unwrap(LHS), unwrap(RHS), Name));
}

/*--.. Miscellaneous instructions ..........................................--*/

LLVMValueRef LLVM_STDCALL LLVMBuildPhi(LLVMBuilderRef B, LLVMTypeRef Ty, const char *Name) {
  return wrap(unwrap(B)->CreatePHI(unwrap(Ty), 0, Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildCall(LLVMBuilderRef B, LLVMValueRef Fn,
                           LLVMValueRef *Args, unsigned NumArgs,
                           const char *Name) {
  return wrap(unwrap(B)->CreateCall(unwrap(Fn),
                                    makeArrayRef(unwrap(Args), NumArgs),
                                    Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildSelect(LLVMBuilderRef B, LLVMValueRef If,
                             LLVMValueRef Then, LLVMValueRef Else,
                             const char *Name) {
  return wrap(unwrap(B)->CreateSelect(unwrap(If), unwrap(Then), unwrap(Else),
                                      Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildVAArg(LLVMBuilderRef B, LLVMValueRef List,
                            LLVMTypeRef Ty, const char *Name) {
  return wrap(unwrap(B)->CreateVAArg(unwrap(List), unwrap(Ty), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildExtractElement(LLVMBuilderRef B, LLVMValueRef VecVal,
                                      LLVMValueRef Index, const char *Name) {
  return wrap(unwrap(B)->CreateExtractElement(unwrap(VecVal), unwrap(Index),
                                              Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildInsertElement(LLVMBuilderRef B, LLVMValueRef VecVal,
                                    LLVMValueRef EltVal, LLVMValueRef Index,
                                    const char *Name) {
  return wrap(unwrap(B)->CreateInsertElement(unwrap(VecVal), unwrap(EltVal),
                                             unwrap(Index), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildShuffleVector(LLVMBuilderRef B, LLVMValueRef V1,
                                    LLVMValueRef V2, LLVMValueRef Mask,
                                    const char *Name) {
  return wrap(unwrap(B)->CreateShuffleVector(unwrap(V1), unwrap(V2),
                                             unwrap(Mask), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildExtractValue(LLVMBuilderRef B, LLVMValueRef AggVal,
                                   unsigned Index, const char *Name) {
  return wrap(unwrap(B)->CreateExtractValue(unwrap(AggVal), Index, Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildInsertValue(LLVMBuilderRef B, LLVMValueRef AggVal,
                                  LLVMValueRef EltVal, unsigned Index,
                                  const char *Name) {
  return wrap(unwrap(B)->CreateInsertValue(unwrap(AggVal), unwrap(EltVal),
                                           Index, Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildIsNull(LLVMBuilderRef B, LLVMValueRef Val,
                             const char *Name) {
  return wrap(unwrap(B)->CreateIsNull(unwrap(Val), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildIsNotNull(LLVMBuilderRef B, LLVMValueRef Val,
                                const char *Name) {
  return wrap(unwrap(B)->CreateIsNotNull(unwrap(Val), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildPtrDiff(LLVMBuilderRef B, LLVMValueRef LHS,
                              LLVMValueRef RHS, const char *Name) {
  return wrap(unwrap(B)->CreatePtrDiff(unwrap(LHS), unwrap(RHS), Name));
}

LLVMValueRef LLVM_STDCALL LLVMBuildAtomicRMW(LLVMBuilderRef B,LLVMAtomicRMWBinOp op,
                               LLVMValueRef PTR, LLVMValueRef Val,
                               LLVMAtomicOrdering ordering,
                               LLVMBool singleThread) {
  AtomicRMWInst::BinOp intop;
  switch (op) {
    case LLVMAtomicRMWBinOpXchg: intop = AtomicRMWInst::Xchg; break;
    case LLVMAtomicRMWBinOpAdd: intop = AtomicRMWInst::Add; break;
    case LLVMAtomicRMWBinOpSub: intop = AtomicRMWInst::Sub; break;
    case LLVMAtomicRMWBinOpAnd: intop = AtomicRMWInst::And; break;
    case LLVMAtomicRMWBinOpNand: intop = AtomicRMWInst::Nand; break;
    case LLVMAtomicRMWBinOpOr: intop = AtomicRMWInst::Or; break;
    case LLVMAtomicRMWBinOpXor: intop = AtomicRMWInst::Xor; break;
    case LLVMAtomicRMWBinOpMax: intop = AtomicRMWInst::Max; break;
    case LLVMAtomicRMWBinOpMin: intop = AtomicRMWInst::Min; break;
    case LLVMAtomicRMWBinOpUMax: intop = AtomicRMWInst::UMax; break;
    case LLVMAtomicRMWBinOpUMin: intop = AtomicRMWInst::UMin; break;
  }
  return wrap(unwrap(B)->CreateAtomicRMW(intop, unwrap(PTR), unwrap(Val),
    mapFromLLVMOrdering(ordering), singleThread ? SyncScope::SingleThread
                                                : SyncScope::System));
}

LLVMValueRef LLVM_STDCALL LLVMBuildAtomicCmpXchg(LLVMBuilderRef B, LLVMValueRef Ptr,
                                    LLVMValueRef Cmp, LLVMValueRef New,
                                    LLVMAtomicOrdering SuccessOrdering,
                                    LLVMAtomicOrdering FailureOrdering,
                                    LLVMBool singleThread) {

  return wrap(unwrap(B)->CreateAtomicCmpXchg(unwrap(Ptr), unwrap(Cmp),
                unwrap(New), mapFromLLVMOrdering(SuccessOrdering),
                mapFromLLVMOrdering(FailureOrdering),
                singleThread ? SyncScope::SingleThread : SyncScope::System));
}


LLVMBool LLVM_STDCALL LLVMIsAtomicSingleThread(LLVMValueRef AtomicInst) {
  Value *P = unwrap<Value>(AtomicInst);

  if (AtomicRMWInst *I = dyn_cast<AtomicRMWInst>(P))
    return I->getSyncScopeID() == SyncScope::SingleThread;
  return cast<AtomicCmpXchgInst>(P)->getSyncScopeID() ==
             SyncScope::SingleThread;
}

void LLVM_STDCALL LLVMSetAtomicSingleThread(LLVMValueRef AtomicInst, LLVMBool NewValue) {
  Value *P = unwrap<Value>(AtomicInst);
  SyncScope::ID SSID = NewValue ? SyncScope::SingleThread : SyncScope::System;

  if (AtomicRMWInst *I = dyn_cast<AtomicRMWInst>(P))
    return I->setSyncScopeID(SSID);
  return cast<AtomicCmpXchgInst>(P)->setSyncScopeID(SSID);
}

LLVMAtomicOrdering LLVM_STDCALL LLVMGetCmpXchgSuccessOrdering(LLVMValueRef CmpXchgInst)  {
  Value *P = unwrap<Value>(CmpXchgInst);
  return mapToLLVMOrdering(cast<AtomicCmpXchgInst>(P)->getSuccessOrdering());
}

void LLVM_STDCALL LLVMSetCmpXchgSuccessOrdering(LLVMValueRef CmpXchgInst,
                                   LLVMAtomicOrdering Ordering) {
  Value *P = unwrap<Value>(CmpXchgInst);
  AtomicOrdering O = mapFromLLVMOrdering(Ordering);

  return cast<AtomicCmpXchgInst>(P)->setSuccessOrdering(O);
}

LLVMAtomicOrdering LLVM_STDCALL LLVMGetCmpXchgFailureOrdering(LLVMValueRef CmpXchgInst)  {
  Value *P = unwrap<Value>(CmpXchgInst);
  return mapToLLVMOrdering(cast<AtomicCmpXchgInst>(P)->getFailureOrdering());
}

void LLVM_STDCALL LLVMSetCmpXchgFailureOrdering(LLVMValueRef CmpXchgInst,
                                   LLVMAtomicOrdering Ordering) {
  Value *P = unwrap<Value>(CmpXchgInst);
  AtomicOrdering O = mapFromLLVMOrdering(Ordering);

  return cast<AtomicCmpXchgInst>(P)->setFailureOrdering(O);
}

/*===-- Module providers --------------------------------------------------===*/

LLVMModuleProviderRef LLVM_STDCALL LLVMCreateModuleProviderForExistingModule(LLVMModuleRef M) {
  return reinterpret_cast<LLVMModuleProviderRef>(M);
}

void LLVM_STDCALL LLVMDisposeModuleProvider(LLVMModuleProviderRef MP) {
  delete unwrap(MP);
}


/*===-- Memory buffers ----------------------------------------------------===*/

LLVMBool LLVM_STDCALL LLVMCreateMemoryBufferWithContentsOfFile(
    const char *Path,
    LLVMMemoryBufferRef *OutMemBuf,
    char **OutMessage) {

  ErrorOr<std::unique_ptr<MemoryBuffer>> MBOrErr = MemoryBuffer::getFile(Path);
  if (std::error_code EC = MBOrErr.getError()) {
    *OutMessage = strdup(EC.message().c_str());
    return 1;
  }
  *OutMemBuf = wrap(MBOrErr.get().release());
  return 0;
}

LLVMBool LLVM_STDCALL LLVMCreateMemoryBufferWithSTDIN(LLVMMemoryBufferRef *OutMemBuf,
                                         char **OutMessage) {
  ErrorOr<std::unique_ptr<MemoryBuffer>> MBOrErr = MemoryBuffer::getSTDIN();
  if (std::error_code EC = MBOrErr.getError()) {
    *OutMessage = strdup(EC.message().c_str());
    return 1;
  }
  *OutMemBuf = wrap(MBOrErr.get().release());
  return 0;
}

LLVMMemoryBufferRef LLVM_STDCALL LLVMCreateMemoryBufferWithMemoryRange(
    const char *InputData,
    size_t InputDataLength,
    const char *BufferName,
    LLVMBool RequiresNullTerminator) {

  return wrap(MemoryBuffer::getMemBuffer(StringRef(InputData, InputDataLength),
                                         StringRef(BufferName),
                                         RequiresNullTerminator).release());
}

LLVMMemoryBufferRef LLVM_STDCALL LLVMCreateMemoryBufferWithMemoryRangeCopy(
    const char *InputData,
    size_t InputDataLength,
    const char *BufferName) {

  return wrap(
      MemoryBuffer::getMemBufferCopy(StringRef(InputData, InputDataLength),
                                     StringRef(BufferName)).release());
}

const char *LLVM_STDCALL LLVMGetBufferStart(LLVMMemoryBufferRef MemBuf) {
  return unwrap(MemBuf)->getBufferStart();
}

size_t LLVM_STDCALL LLVMGetBufferSize(LLVMMemoryBufferRef MemBuf) {
  return unwrap(MemBuf)->getBufferSize();
}

void LLVM_STDCALL LLVMDisposeMemoryBuffer(LLVMMemoryBufferRef MemBuf) {
  delete unwrap(MemBuf);
}

/*===-- Pass Registry -----------------------------------------------------===*/

LLVMPassRegistryRef LLVM_STDCALL LLVMGetGlobalPassRegistry(void) {
  return wrap(PassRegistry::getPassRegistry());
}

/*===-- Pass Manager ------------------------------------------------------===*/

LLVMPassManagerRef LLVM_STDCALL LLVMCreatePassManager() {
  return wrap(new legacy::PassManager());
}

LLVMPassManagerRef LLVM_STDCALL LLVMCreateFunctionPassManagerForModule(LLVMModuleRef M) {
  return wrap(new legacy::FunctionPassManager(unwrap(M)));
}

LLVMPassManagerRef LLVM_STDCALL LLVMCreateFunctionPassManager(LLVMModuleProviderRef P) {
  return LLVMCreateFunctionPassManagerForModule(
                                            reinterpret_cast<LLVMModuleRef>(P));
}

LLVMBool LLVM_STDCALL LLVMRunPassManager(LLVMPassManagerRef PM, LLVMModuleRef M) {
  return unwrap<legacy::PassManager>(PM)->run(*unwrap(M));
}

LLVMBool LLVM_STDCALL LLVMInitializeFunctionPassManager(LLVMPassManagerRef FPM) {
  return unwrap<legacy::FunctionPassManager>(FPM)->doInitialization();
}

LLVMBool LLVM_STDCALL LLVMRunFunctionPassManager(LLVMPassManagerRef FPM, LLVMValueRef F) {
  return unwrap<legacy::FunctionPassManager>(FPM)->run(*unwrap<Function>(F));
}

LLVMBool LLVM_STDCALL LLVMFinalizeFunctionPassManager(LLVMPassManagerRef FPM) {
  return unwrap<legacy::FunctionPassManager>(FPM)->doFinalization();
}

void LLVM_STDCALL LLVMDisposePassManager(LLVMPassManagerRef PM) {
  delete unwrap(PM);
}

/*===-- Threading ------------------------------------------------------===*/

LLVMBool LLVM_STDCALL LLVMStartMultithreaded() {
  return LLVMIsMultithreaded();
}

void LLVM_STDCALL LLVMStopMultithreaded() {
}

LLVMBool LLVM_STDCALL LLVMIsMultithreaded() {
  return llvm_is_multithreaded();
}
