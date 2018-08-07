VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cJIT"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
'=========================================================================
'
' VBLLVM Project
' kscope (c) 2018 by wqweto@gmail.com
'
' Kaleidoscope toy language for VBLLVM
'
' cJIT.cls - JIT engine RAII
'
'=========================================================================
Option Explicit
DefObj A-Z

'=========================================================================
' Constants and member variables
'=========================================================================

Private m_sLastError            As String
Private m_hEngine               As LLVMExecutionEngineRef
Private m_hModule               As LLVMModuleRef

'=========================================================================
' Properties
'=========================================================================

Property Get LastError() As String
    LastError = m_sLastError
End Property

'=========================================================================
' Methods
'=========================================================================

Public Function Init(oMachine As cTargetMachine) As Boolean
    Dim hModule         As Long
    Dim lMsgPtr         As Long
    
    hModule = LLVMModuleCreateWithName("empty module")
    If hModule = 0 Then
        m_sLastError = "Cannot create empty module"
        GoTo QH
    End If
    Call LLVMSetTarget(hModule, oMachine.Triple)
    Call LLVMSetModuleDataLayout(hModule, LLVMCreateTargetDataLayout(oMachine.GetMachine()))
    pvAddRuntimeFunction hModule, "putchard", AddressOf RuntimePutchard
    pvAddRuntimeFunction hModule, "printd", AddressOf RuntimePrintd
    If LLVMCreateExecutionEngineForModule(m_hEngine, hModule, lMsgPtr) <> 0 Then
        m_sLastError = ToString(lMsgPtr)
        Call LLVMDisposeMessage(lMsgPtr)
        GoTo QH
    End If
    '--- success
    Init = True
QH:
End Function

Public Function AddModule(oCodegen As cCodegen) As Boolean
    Dim hModule         As Long
    
    hModule = oCodegen.GetModule()
    If hModule = 0 Then
        m_sLastError = oCodegen.LastError
        GoTo QH
    End If
    Call LLVMAddModule(m_hEngine, hModule)
    m_hModule = hModule
    '--- success
    AddModule = True
QH:
End Function

Public Function Invoke(sFuncName As String, dblResult As Double) As Boolean
    Dim hFunc           As LLVMValueRef
    Dim pfnFunc         As Long
    
    If m_hModule <> 0 Then
        hFunc = LLVMGetNamedFunction(m_hModule, sFuncName)
    End If
    If hFunc = 0 Then
        GoTo QH
    End If
    If m_hEngine <> 0 Then
        pfnFunc = LLVMGetPointerToGlobal(m_hEngine, hFunc)
    End If
    If pfnFunc = 0 Then
        GoTo QH
    End If
    dblResult = CallNoParam(pfnFunc)
    '--- success
    Invoke = True
QH:
End Function

Public Sub RemoveModule(oCodegen As cCodegen)
    Call LLVMRemoveModule(m_hEngine, oCodegen.GetModule(), m_hModule, 0)
End Sub

Private Function pvAddRuntimeFunction(ByVal hModule As LLVMModuleRef, sName As String, ByVal lAddr As Long)
    Dim hBuilder        As LLVMBuilderRef
    Dim hFnType         As LLVMTypeRef
    Dim hFunc           As LLVMValueRef
    Dim hBlock          As LLVMBasicBlockRef
    Dim hCast           As LLVMValueRef
    Dim aParams()       As LLVMTypeRef
    Dim aArgs()         As LLVMValueRef
    Dim hResult         As LLVMValueRef
    
    hBuilder = LLVMCreateBuilder()
    ReDim aParams(0 To 0) As LLVMTypeRef
    aParams(0) = LLVMDoubleType()
    hFnType = LLVMFunctionType(LLVMDoubleType(), aParams(0), UBound(aParams) + 1, 0)
    hFunc = LLVMAddFunction(hModule, sName, hFnType)
    hBlock = LLVMAppendBasicBlock(hFunc, "entry")
    Call LLVMPositionBuilderAtEnd(hBuilder, hBlock)
    hCast = pvCastToFunction(hBuilder, lAddr, hFnType)
    ReDim aArgs(0 To 0) As LLVMValueRef
    aArgs(0) = LLVMGetParam(hFunc, 0)
    hResult = LLVMBuildCall(hBuilder, hCast, aArgs(0), UBound(aArgs) + 1, "calltmp")
    Call LLVMSetInstructionCallConv(hResult, LLVMX86StdcallCallConv)
    Call LLVMBuildRet(hBuilder, hResult)
    Call LLVMDisposeBuilder(hBuilder)
End Function

Private Function pvCastToFunction(ByVal hBuilder As Long, ByVal lPtr As Long, ByVal hFuncType As Long) As LLVMValueRef
    Dim hType           As Long
    Dim hIntPtr         As Long
    
    hType = LLVMIntType(8 * 4)
    hIntPtr = LLVMBuildIntToPtr(hBuilder, LLVMConstInt(hType, lPtr / 10000@, 0), LLVMPointerType(hType, 0), "cast int to int_ptr")
    pvCastToFunction = LLVMBuildBitCast(hBuilder, hIntPtr, LLVMPointerType(hFuncType, 0), "cast int_ptr to pfn")
End Function

'=========================================================================
' Base class events
'=========================================================================

Private Sub Class_Terminate()
    If m_hEngine <> 0 Then
        Call LLVMDisposeExecutionEngine(m_hEngine)
    End If
End Sub
