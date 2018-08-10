@echo off
setlocal
set prj_root=%~dp0..

call %prj_root%\lib\install-release\lib\dump-def.bat > VBLLVM.def
echo   LLVMInitializeAllTargetInfos=_VBLLVMInitializeAllTargetInfos@0 >> VBLLVM.def
echo   LLVMInitializeAllTargets=_VBLLVMInitializeAllTargets@0 >> VBLLVM.def
echo   LLVMInitializeAllTargetMCs=_VBLLVMInitializeAllTargetMCs@0 >> VBLLVM.def
echo   LLVMInitializeAllAsmPrinters=_VBLLVMInitializeAllAsmPrinters@0 >> VBLLVM.def
echo   LLVMInitializeAllAsmParsers=_VBLLVMInitializeAllAsmParsers@0 >> VBLLVM.def
echo   LLVMInitializeAllDisassemblers=_VBLLVMInitializeAllDisassemblers@0 >> VBLLVM.def
echo   LLVMInitializeNativeTarget=_VBLLVMInitializeNativeTarget@0 >> VBLLVM.def
echo   LLVMInitializeNativeAsmParser=_VBLLVMInitializeNativeAsmParser@0 >> VBLLVM.def
echo   LLVMInitializeNativeAsmPrinter=_VBLLVMInitializeNativeAsmPrinter@0 >> VBLLVM.def
echo   LLVMInitializeNativeDisassembler=_VBLLVMInitializeNativeDisassembler@0 >> VBLLVM.def
echo   LLVMLLDLink=_LLVMLLDLink@16 >> VBLLVM.def
