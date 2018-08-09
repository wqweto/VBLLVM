Attribute VB_Name = "mdRuntime"
'=========================================================================
'
' VBLLVM Project
' kscope (c) 2018 by wqweto@gmail.com
'
' Kaleidoscope toy language for VBLLVM
'
' mdRuntime.bas - Runtime functions impl for JIT execution
'
'=========================================================================
Option Explicit
DefObj A-Z

Public Function RuntimePutchard(ByVal dblValue As Double) As Double
    If dblValue = 10 And Not InIde Then
        ConsoleError vbCr
    End If
    ConsoleError ChrW(dblValue)
End Function

Public Function RuntimePrintd(ByVal dblValue As Double) As Double
    ConsoleError "%1" & vbCrLf, dblValue
End Function


