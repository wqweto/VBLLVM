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
' cJIT.cls - JIT engine functions
'
'=========================================================================
Option Explicit
DefObj A-Z

Private m_sLastError            As String
Private m_oCodegen              As cCodegen
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

Public Function AddModule(oCodegen As cCodegen) As Boolean
    Dim hModule         As Long
    Dim lMsgPtr         As Long
    
    Set m_oCodegen = oCodegen
    hModule = oCodegen.GetModule()
    If hModule = 0 Then
        m_sLastError = oCodegen.LastError
        GoTo QH
    End If
    If m_hEngine = 0 Then
        If LLVMCreateExecutionEngineForModule(m_hEngine, hModule, lMsgPtr) <> 0 Then
            m_sLastError = ToString(lMsgPtr)
            Call LLVMDisposeMessage(lMsgPtr)
            GoTo QH
        End If
    Else
        Call LLVMAddModule(m_hEngine, hModule)
    End If
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

'=========================================================================
' Base class events
'=========================================================================

Private Sub Class_Terminate()
    If m_hEngine <> 0 Then
        Call LLVMDisposeExecutionEngine(m_hEngine)
    End If
End Sub
