VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cTargetMachine"
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
' cTargetMachine.cls - Target machine RAII
'
'=========================================================================
Option Explicit
DefObj A-Z

'=========================================================================
' Constants and member variables
'=========================================================================

Private m_sLastError        As String
Private m_sTriple           As String
Private m_hMachine          As LLVMTargetMachineRef

'=========================================================================
' Properties
'=========================================================================

Property Get LastError() As String
    LastError = m_sLastError
End Property

Property Get Triple() As String
    Triple = m_sTriple
End Property

'=========================================================================
' Methods
'=========================================================================

Public Function Init(sTriple As String, Optional ByVal IsJIT As Boolean) As Boolean
    Dim hTarget         As Long
    Dim lMsgPtr         As Long

    m_sTriple = sTriple
    If LLVMGetTargetFromTriple(sTriple, hTarget, lMsgPtr) <> 0 Then
        m_sLastError = ToString(lMsgPtr)
        GoTo QH
    End If
    If IsJIT Then
        m_hMachine = LLVMCreateTargetMachine(hTarget, sTriple, _
            ToString(LLVMGetHostCPUName), _
            ToString(LLVMGetHostCPUFeatures), _
            LLVMCodeGenLevelAggressive, _
            LLVMRelocDefault, _
            LLVMCodeModelJITDefault)
    Else
        m_hMachine = LLVMCreateTargetMachine(hTarget, sTriple, _
            "generic", "", LLVMCodeGenLevelNone, LLVMRelocDefault, LLVMCodeModelDefault)
    End If
    If m_hMachine = 0 Then
        m_sLastError = "LLVMCreateTargetMachine failed for " & sTriple
        GoTo QH
    End If
    '--- success
    Init = True
QH:
End Function

Public Function GetMachine() As LLVMTargetMachineRef
    GetMachine = m_hMachine
End Function

'=========================================================================
' Base class events
'=========================================================================

Private Sub Class_Terminate()
    If m_hMachine <> 0 Then
        Call LLVMDisposeTargetMachine(m_hMachine)
    End If
End Sub
