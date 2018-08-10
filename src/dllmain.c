#include <windows.h>
#include <llvm-c/Target.h>

// LLVM libs
#pragma comment(lib, "LLVMAnalysis.lib")
#pragma comment(lib, "LLVMAsmParser.lib")
#pragma comment(lib, "LLVMAsmPrinter.lib")
#pragma comment(lib, "LLVMBinaryFormat.lib")
#pragma comment(lib, "LLVMBitReader.lib")
#pragma comment(lib, "LLVMBitWriter.lib")
#pragma comment(lib, "LLVMCodeGen.lib")
#pragma comment(lib, "LLVMCore.lib")
#pragma comment(lib, "LLVMCoroutines.lib")
#pragma comment(lib, "LLVMCoverage.lib")
#pragma comment(lib, "LLVMDebugInfoCodeView.lib")
#pragma comment(lib, "LLVMDebugInfoDWARF.lib")
#pragma comment(lib, "LLVMDebugInfoMSF.lib")
#pragma comment(lib, "LLVMDebugInfoPDB.lib")
#pragma comment(lib, "LLVMDemangle.lib")
#pragma comment(lib, "LLVMDlltoolDriver.lib")
#pragma comment(lib, "LLVMExecutionEngine.lib")
#pragma comment(lib, "LLVMFuzzMutate.lib")
#pragma comment(lib, "LLVMGlobalISel.lib")
#pragma comment(lib, "LLVMInstCombine.lib")
#pragma comment(lib, "LLVMInstrumentation.lib")
#pragma comment(lib, "LLVMInterpreter.lib")
#pragma comment(lib, "LLVMipo.lib")
#pragma comment(lib, "LLVMIRReader.lib")
#pragma comment(lib, "LLVMLibDriver.lib")
#pragma comment(lib, "LLVMLineEditor.lib")
#pragma comment(lib, "LLVMLinker.lib")
#pragma comment(lib, "LLVMLTO.lib")
#pragma comment(lib, "LLVMMC.lib")
#pragma comment(lib, "LLVMMCDisassembler.lib")
#pragma comment(lib, "LLVMMCJIT.lib")
#pragma comment(lib, "LLVMMCParser.lib")
#pragma comment(lib, "LLVMMIRParser.lib")
#pragma comment(lib, "LLVMObjCARCOpts.lib")
#pragma comment(lib, "LLVMObject.lib")
#pragma comment(lib, "LLVMObjectYAML.lib")
#pragma comment(lib, "LLVMOption.lib")
#pragma comment(lib, "LLVMOrcJIT.lib")
#pragma comment(lib, "LLVMPasses.lib")
#pragma comment(lib, "LLVMProfileData.lib")
#pragma comment(lib, "LLVMRuntimeDyld.lib")
#pragma comment(lib, "LLVMScalarOpts.lib")
#pragma comment(lib, "LLVMSelectionDAG.lib")
#pragma comment(lib, "LLVMSupport.lib")
#pragma comment(lib, "LLVMSymbolize.lib")
#pragma comment(lib, "LLVMTableGen.lib")
#pragma comment(lib, "LLVMTarget.lib")
#pragma comment(lib, "LLVMTransformUtils.lib")
#pragma comment(lib, "LLVMVectorize.lib")
#pragma comment(lib, "LLVMWindowsManifest.lib")
#pragma comment(lib, "LLVMX86AsmParser.lib")
#pragma comment(lib, "LLVMX86AsmPrinter.lib")
#pragma comment(lib, "LLVMX86CodeGen.lib")
#pragma comment(lib, "LLVMX86Desc.lib")
#pragma comment(lib, "LLVMX86Disassembler.lib")
#pragma comment(lib, "LLVMX86Info.lib")
#pragma comment(lib, "LLVMX86Utils.lib")
#pragma comment(lib, "LLVMXRay.lib")
// lld libs
#pragma comment(lib, "lldCOFF.lib")
#pragma comment(lib, "lldCommon.lib")
#pragma comment(lib, "lldCore.lib")
#pragma comment(lib, "lldDriver.lib")
#pragma comment(lib, "lldELF.lib")
#pragma comment(lib, "lldMachO.lib")
#pragma comment(lib, "lldMinGW.lib")
#pragma comment(lib, "lldReaderWriter.lib")
#pragma comment(lib, "lldWasm.lib")
#pragma comment(lib, "lldYAML.lib")

BOOL APIENTRY DllMain( HMODULE hModule,
                       DWORD  ul_reason_for_call,
                       LPVOID lpReserved )
{
	switch (ul_reason_for_call)
	{
	case DLL_PROCESS_ATTACH:
	case DLL_THREAD_ATTACH:
	case DLL_THREAD_DETACH:
	case DLL_PROCESS_DETACH:
		break;
	}
	return TRUE;
}

void LLVM_STDCALL VBLLVMInitializeAllTargetInfos(void) {
    LLVMInitializeAllTargetInfos();
}

void LLVM_STDCALL VBLLVMInitializeAllTargets(void) {
    LLVMInitializeAllTargets();
}

void LLVM_STDCALL VBLLVMInitializeAllTargetMCs(void) {
    LLVMInitializeAllTargetMCs();
}

void LLVM_STDCALL VBLLVMInitializeAllAsmPrinters(void) {
    LLVMInitializeAllAsmPrinters();
}

void LLVM_STDCALL VBLLVMInitializeAllAsmParsers(void) {
    LLVMInitializeAllAsmParsers();
}

void LLVM_STDCALL VBLLVMInitializeAllDisassemblers(void) {
    LLVMInitializeAllDisassemblers();
}

LLVMBool LLVM_STDCALL VBLLVMInitializeNativeTarget(void) {
    return LLVMInitializeNativeTarget();
}

LLVMBool LLVM_STDCALL VBLLVMInitializeNativeAsmParser(void) {
    return LLVMInitializeNativeAsmParser();
}

LLVMBool LLVM_STDCALL VBLLVMInitializeNativeAsmPrinter(void) {
    return LLVMInitializeNativeAsmPrinter();
}

LLVMBool LLVM_STDCALL VBLLVMInitializeNativeDisassembler(void) {
    return LLVMInitializeNativeDisassembler();
}

/*
  LLVMInitializeAllTargetInfos=_VBLLVMInitializeAllTargetInfos@0
  LLVMInitializeAllTargets=_VBLLVMInitializeAllTargets@0
  LLVMInitializeAllTargetMCs=_VBLLVMInitializeAllTargetMCs@0
  LLVMInitializeAllAsmPrinters=_VBLLVMInitializeAllAsmPrinters@0
  LLVMInitializeAllAsmParsers=_VBLLVMInitializeAllAsmParsers@0
  LLVMInitializeAllDisassemblers=_VBLLVMInitializeAllDisassemblers@0
  LLVMInitializeNativeTarget=_VBLLVMInitializeNativeTarget@0
  LLVMInitializeNativeAsmParser=_VBLLVMInitializeNativeAsmParser@0
  LLVMInitializeNativeAsmPrinter=_VBLLVMInitializeNativeAsmPrinter@0
  LLVMInitializeNativeDisassembler=_VBLLVMInitializeNativeDisassembler@0  
*/
