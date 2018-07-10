Attribute VB_Name = "Module1"
Option Explicit

'=========================================================================
' API
'=========================================================================

'--- for VirtualAlloc
Private Const PAGE_EXECUTE_READWRITE        As Long = &H40
'--- console
Private Const STD_OUTPUT_HANDLE             As Long = -11&
Private Const STD_ERROR_HANDLE              As Long = -12&
'--- for LLVMVerifyModule
Private Const LLVMAbortProcessAction        As Long = 0
Private Const LLVMX86StdcallCallConv        As Long = 64

Private Declare Sub CopyMemory Lib "kernel32" Alias "RtlMoveMemory" (dest As Any, src As Any, ByVal l As Long)
Private Declare Function lstrlen Lib "kernel32" Alias "lstrlenA" (ByVal lpString As Long) As Long
Private Declare Function LoadLibrary Lib "kernel32" Alias "LoadLibraryA" (ByVal lpLibFileName As String) As Long
Private Declare Function VirtualProtect Lib "kernel32" (ByVal lpAddress As Long, ByVal dwSize As Long, ByVal flNewProtect As Long, ByRef lpflOldProtect As Long) As Long
'--- console
Private Declare Function GetStdHandle Lib "kernel32" (ByVal nStdHandle As Long) As Long
Private Declare Function WriteFile Lib "kernel32" (ByVal hFile As Long, lpBuffer As Any, ByVal nNumberOfBytesToWrite As Long, lpNumberOfBytesWritten As Long, lpOverlapped As Any) As Long
Private Declare Function CharToOemBuff Lib "user32" Alias "CharToOemBuffA" (ByVal lpszSrc As String, lpszDst As Any, ByVal cchDstLength As Long) As Long
'--- LLVM
Private Declare Function LLVMModuleCreateWithName Lib "vbllvm" (ByVal ModuleID As String) As Long
Private Declare Sub LLVMDisposeModule Lib "vbllvm" (ByVal hM As Long)
Private Declare Function LLVMInt32Type Lib "vbllvm" () As Long
Private Declare Function LLVMFunctionType Lib "vbllvm" (ByVal ReturnType As Long, ParamTypes As Any, ByVal ParamCount As Long, ByVal IsVarArg As Long) As Long
Private Declare Function LLVMAddFunction Lib "vbllvm" (ByVal hM As Long, ByVal Name As String, ByVal FunctionTy As Long) As Long
Private Declare Function LLVMAppendBasicBlock Lib "vbllvm" (ByVal hFn As Long, ByVal Name As String) As Long
Private Declare Function LLVMCreateBuilder Lib "vbllvm" () As Long
Private Declare Function LLVMPositionBuilderAtEnd Lib "vbllvm" (ByVal Builder As Long, ByVal Block As Long) As Long
Private Declare Function LLVMGetParam Lib "vbllvm" (ByVal hFn As Long, ByVal Index As Long) As Long
Private Declare Function LLVMBuildAdd Lib "vbllvm" (ByVal Builder As Long, ByVal LHS As Long, ByVal RHS As Long, ByVal Name As String) As Long
Private Declare Function LLVMBuildRet Lib "vbllvm" (ByVal Builder As Long, ByVal hV As Long) As Long
Private Declare Function LLVMVerifyModule Lib "vbllvm" (ByVal hM As Long, ByVal Action As Long, OutMessage As Long) As Long
Private Declare Function LLVMDisposeMessage Lib "vbllvm" (ByVal Message As Long) As Long
Private Declare Function LLVMInitializeNativeTarget Lib "vbllvm" () As Long
Private Declare Function LLVMInitializeNativeAsmPrinter Lib "vbllvm" () As Long
Private Declare Function LLVMInitializeNativeAsmParser Lib "vbllvm" () As Long
Private Declare Function LLVMCreateExecutionEngineForModule Lib "vbllvm" (OutEE As Long, ByVal hM As Long, OutError As Long) As Long
Private Declare Function LLVMCreateInterpreterForModule Lib "vbllvm" (OutInterp As Long, ByVal hM As Long, OutError As Long) As Long
Private Declare Function LLVMCreateGenericValueOfInt Lib "vbllvm" (ByVal Ty As Long, ByVal ValN As Currency, ByVal IsSigned As Long) As Long
Private Declare Function LLVMRunFunction Lib "vbllvm" (ByVal hEE As Long, ByVal hF As Long, ByVal NumArgs As Long, Args As Any) As Long
Private Declare Function LLVMGenericValueToInt Lib "vbllvm" (ByVal GenVal As Long, ByVal IsSigned As Long) As Currency
Private Declare Function LLVMWriteBitcodeToFile Lib "vbllvm" (ByVal hM As Long, ByVal Path As String) As Long
Private Declare Sub LLVMDisposeBuilder Lib "vbllvm" (ByVal Builder As Long)
Private Declare Sub LLVMDisposeExecutionEngine Lib "vbllvm" (ByVal hEE As Long)
Private Declare Function LLVMSetFunctionCallConv Lib "vbllvm" (ByVal hFn As Long, ByVal CC As Long) As Long
Private Declare Function LLVMGetPointerToGlobal Lib "vbllvm" (ByVal hEE As Long, ByVal hGlobal As Long) As Long
Private Declare Function LLVMPrintModuleToString Lib "vbllvm" (ByVal hM As Long) As Long
                               
Sub Main()
'    LoadLibrary App.Path & "\..\..\bin\Release\VBLLVM.dll"
    LoadLibrary App.Path & "\..\..\bin\Debug\VBLLVM.dll"
    pvTestJIT
End Sub

Private Sub pvTestJIT()
    Dim hMod            As Long
    Dim hFnSum          As Long
    Dim lMsgPtr         As Long
    Dim hInterp         As Long
    Dim aValParam(0 To 1) As Long
    Dim hValResult      As Long
    Dim hEngine         As Long
    Dim pfnSum          As Long
    
    '--- build Function sum(a, b) return a+b End Function
    If Not pvBuildFunction(hMod, hFnSum) Then
        GoTo QH
    End If
    '--- interpret sum
    If LLVMCreateInterpreterForModule(hInterp, hMod, lMsgPtr) <> 0 Then
        ConsoleError "Interpreter Error: %1" & vbCrLf, pvToString(lMsgPtr)
        Call LLVMDisposeMessage(lMsgPtr)
        GoTo QH
    End If
    aValParam(0) = LLVMCreateGenericValueOfInt(LLVMInt32Type(), 2, 0) '--- note: this passes 20000 for Int32
    aValParam(1) = LLVMCreateGenericValueOfInt(LLVMInt32Type(), 3, 0) '--- note: this passes 30000 for Int32
    hValResult = LLVMRunFunction(hInterp, hFnSum, 2, aValParam(0))
    ConsolePrint "Interpreter Result: %1" & vbCrLf, LLVMGenericValueToInt(hValResult, 0)
    Call LLVMDisposeExecutionEngine(hInterp)
    '--- build another Function sum(a, b) return a+b End Function
    If Not pvBuildFunction(hMod, hFnSum) Then
        GoTo QH
    End If
    '--- JIT compile and execute sum
    Call LLVMInitializeNativeTarget
    Call LLVMInitializeNativeAsmPrinter
    Call LLVMInitializeNativeAsmParser
    If LLVMCreateExecutionEngineForModule(hEngine, hMod, lMsgPtr) <> 0 Then
        ConsoleError "MCJIT Error: %1" & vbCrLf, pvToString(lMsgPtr)
        Call LLVMDisposeMessage(lMsgPtr)
        GoTo QH
    End If
    pfnSum = LLVMGetPointerToGlobal(hEngine, hFnSum)
    ConsolePrint "MCJIT Result: %1" & vbCrLf, pvCallSum(pfnSum, 4, 5)
    '--- persist bitcode & dump IR
    Call LLVMWriteBitcodeToFile(hMod, App.EXEName & ".bc")
    lMsgPtr = LLVMPrintModuleToString(hMod)
    If lMsgPtr <> 0 Then
        ConsolePrint pvToString(lMsgPtr) & vbCrLf
        Call LLVMDisposeMessage(lMsgPtr)
    End If
    Call LLVMDisposeExecutionEngine(hEngine)
QH:
End Sub

Private Function pvBuildFunction(hMod As Long, hFnSum As Long) As Long
    Dim hTempMod        As LLVMModuleRef
    Dim aFnParams(0 To 1) As Long
    Dim hTypeFn         As Long
    Dim hBlockEntry     As Long
    Dim hBuilder        As Long
    Dim hValueTmp       As Long
    Dim lMsgPtr         As Long
    
    hTempMod = LLVMModuleCreateWithName("my_module")
    aFnParams(0) = LLVMInt32Type()
    aFnParams(1) = LLVMInt32Type()
    hTypeFn = LLVMFunctionType(LLVMInt32Type(), aFnParams(0), 2, 0)
    hFnSum = LLVMAddFunction(hTempMod, "sum", hTypeFn)
    Call LLVMSetFunctionCallConv(hFnSum, LLVMX86StdcallCallConv)
    hBlockEntry = LLVMAppendBasicBlock(hFnSum, "entry")
    hBuilder = LLVMCreateBuilder()
    Call LLVMPositionBuilderAtEnd(hBuilder, hBlockEntry)
    hValueTmp = LLVMBuildAdd(hBuilder, LLVMGetParam(hFnSum, 0), LLVMGetParam(hFnSum, 1), "tmp")
    Call LLVMBuildRet(hBuilder, hValueTmp)
    If LLVMVerifyModule(hTempMod, LLVMAbortProcessAction, lMsgPtr) <> 0 Then
        ConsoleError "LLVMVerifyModule: %1" & vbCrLf, pvToString(lMsgPtr)
        Call LLVMDisposeMessage(lMsgPtr)
        GoTo QH
    End If
    '--- success
    hMod = hTempMod
    hTempMod = 0
    pvBuildFunction = True
QH:
    Call LLVMDisposeBuilder(hBuilder)
    Call LLVMDisposeModule(hTempMod)
End Function

'= utility ===============================================================

Public Function ConsolePrint(ByVal sText As String, ParamArray A() As Variant) As String
    ConsolePrint = pvConsoleOutput(GetStdHandle(STD_OUTPUT_HANDLE), sText, CVar(A))
End Function

Public Function ConsoleError(ByVal sText As String, ParamArray A() As Variant) As String
    ConsoleError = pvConsoleOutput(GetStdHandle(STD_ERROR_HANDLE), sText, CVar(A))
End Function

Private Function pvConsoleOutput(ByVal hOut As Long, ByVal sText As String, A As Variant) As String
    Dim lIdx            As Long
    Dim sArg            As String
    Dim baBuffer()      As Byte
    Dim dwDummy         As Long

    '--- format
    For lIdx = UBound(A) To LBound(A) Step -1
        sArg = Replace(A(lIdx), "%", ChrW$(&H101))
        sText = Replace(sText, "%" & (lIdx - LBound(A) + 1), sArg)
    Next
    pvConsoleOutput = Replace(sText, ChrW$(&H101), "%")
    '--- output
    If hOut = 0 Then
        Debug.Print pvConsoleOutput;
    Else
        ReDim baBuffer(0 To Len(pvConsoleOutput) - 1) As Byte
        If CharToOemBuff(pvConsoleOutput, baBuffer(0), UBound(baBuffer) + 1) Then
            Call WriteFile(hOut, baBuffer(0), UBound(baBuffer) + 1, dwDummy, ByVal 0&)
        End If
    End If
End Function

Private Function pvToString(ByVal lPtr As Long) As String
    If lPtr <> 0 Then
        pvToString = String(lstrlen(lPtr), Chr(0))
        Call CopyMemory(ByVal pvToString, ByVal lPtr, lstrlen(lPtr))
    End If
End Function

Private Function pvCallSum(ByVal pfn As Long, ByVal lFirst As Long, ByVal lSecond As Long) As Long
    '--- on first call will self-patch to call `pfn` with args `lFirst` and `lSecond` directly
    RtccPatchProto AddressOf Module1.pvCallSum
    pvCallSum = pvCallSum(pfn, lFirst, lSecond)
End Function

Public Sub RtccPatchProto(ByVal pfn As Long) '--- Helper by The trick
    Dim bInIDE          As Boolean
 
    Debug.Assert pvSetTrue(bInIDE)
    If bInIDE Then
        Call CopyMemory(pfn, ByVal UnsignedAdd(pfn, &H16), 4)
    Else
        Call VirtualProtect(pfn, 8, PAGE_EXECUTE_READWRITE, 0)
    End If
    ' 0:  58                      pop    eax
    ' 1:  59                      pop    ecx
    ' 2:  50                      push   eax
    ' 3:  ff e1                   jmp    ecx
    ' 5:  90                      nop
    ' 6:  90                      nop
    ' 7:  90                      nop
    Call CopyMemory(ByVal pfn, -802975883527609.7192@, 8)
End Sub

Private Function pvSetTrue(bValue As Boolean) As Boolean
    bValue = True
    pvSetTrue = True
End Function

Private Function UnsignedAdd(ByVal lUnsignedPtr As Long, ByVal lSignedOffset As Long) As Long
    '--- note: safely add *signed* offset to *unsigned* ptr for *unsigned* retval w/o overflow in LARGEADDRESSAWARE processes
    UnsignedAdd = ((lUnsignedPtr Xor &H80000000) + lSignedOffset) Xor &H80000000
End Function
