Attribute VB_Name = "mdMain"
'=========================================================================
'
' VBLLVM Project
' kscope (c) 2018 by wqweto@gmail.com
'
' Kaleidoscope toy language for VBLLVM
'
' mdMain.bas - Main functions
'
'=========================================================================
Option Explicit
DefObj A-Z

'=========================================================================
' API
'=========================================================================

Private Declare Sub ExitProcess Lib "kernel32" (ByVal uExitCode As Long)

'=========================================================================
' Constants and member variables
'=========================================================================

Private Const STR_VERSION           As String = "0.1"

Private m_oParser               As cParser
Private m_oOpt                  As Scripting.Dictionary

'=========================================================================
' Functions
'=========================================================================

Private Sub Main()
    Dim lExitCode       As Long
    
    Call LLVMInitializeAllTargetInfos
    Call LLVMInitializeAllTargets
    Call LLVMInitializeAllTargetMCs
    Call LLVMInitializeAllAsmPrinters
    Call LLVMInitializeAllAsmParsers
    lExitCode = Process(SplitArgs(Trim$(Command$)))
    If Not InIde Then
        Call ExitProcess(lExitCode)
    End If
End Sub

Private Function Process(vArgs As Variant) As Long
    Dim sOutFile        As String
    Dim nFile           As Integer
    Dim sOutput         As String
    Dim lIdx            As Long
    Dim oTree           As Object
    Dim oCodegen        As cCodegen
    Dim oJIT            As cJIT
    Dim vNode           As Variant
    Dim dblResult       As Double
    Dim oMachine        As cTargetMachine
    
    On Error GoTo EH
    Set m_oParser = New cParser
    Set m_oOpt = GetOpt(vArgs, "o:O")
    If Not m_oOpt.Item("-nologo") And Not m_oOpt.Item("-q") Then
        ConsoleError App.ProductName & " VB6 port " & STR_VERSION & " (c) 2018 by wqweto@gmail.com (" & m_oParser.ParserVersion & ")" & vbCrLf & vbCrLf
    End If
    If LenB(m_oOpt.Item("error")) <> 0 Then
        ConsoleError "Error in command line: " & m_oOpt.Item("error") & vbCrLf & vbCrLf
        If Not (m_oOpt.Item("-h") Or m_oOpt.Item("-?") Or m_oOpt.Item("arg1") = "?") Then
            Exit Function
        End If
    End If
    If m_oOpt.Item("numarg") = 0 Or m_oOpt.Item("-h") Or m_oOpt.Item("-?") Or m_oOpt.Item("arg1") = "?" Then
        ConsoleError "Usage: %1.exe [options] <in_file.ks>" & vbCrLf & vbCrLf, App.EXEName
        ConsoleError "Options:" & vbCrLf & _
            "  -o OUTFILE      write result to OUTFILE [default: stdout]" & vbCrLf & _
            "  -emit-tree      output parse tree" & vbCrLf & _
            "  -emit-llvm      output intermediate represetation" & vbCrLf & _
            "  -q              in quiet operation outputs only errors" & vbCrLf & _
            "  -nologo         suppress startup banner" & vbCrLf & _
            "If no -emit-xxx is used emits executable. If no -o is used writes result to console." & vbCrLf
        If m_oOpt.Item("numarg") = 0 Then
            Process = 100
        End If
        GoTo QH
    End If
    sOutFile = m_oOpt.Item("-o")
    Set oMachine = New cTargetMachine
    If Not oMachine.Init(ToString(LLVMGetDefaultTargetTriple())) Then
        ConsoleError "%1" & vbCrLf, oMachine.LastError
        GoTo QH
    End If
    If Not m_oOpt.Item("-emit-tree") And Not m_oOpt.Item("-emit-llvm") And LenB(sOutFile) = 0 Then
        Set oJIT = New cJIT
        If Not oJIT.Init(oMachine) Then
            Err.Raise vbObjectError, , "Cannot init JIT: " & oJIT.LastError
        End If
    End If
    For lIdx = 1 To m_oOpt.Item("numarg")
        Set oTree = m_oParser.MatchFile(m_oOpt.Item("arg" & lIdx))
        If LenB(m_oParser.LastError) <> 0 Then
            ConsoleError "%2: %3: %1" & vbCrLf, m_oParser.LastError, Join(m_oParser.CalcLine(m_oParser.LastOffset + 1), ":"), IIf(oTree Is Nothing, "error", "warning")
            GoTo QH
        End If
        If m_oOpt.Item("-emit-tree") Then
            sOutput = sOutput & JsonDump(oTree) & vbCrLf
        ElseIf Not oTree Is Nothing Then
            For Each vNode In oTree.Items
                If oCodegen Is Nothing Then
                    Set oCodegen = New cCodegen
                    If Not oCodegen.Init(oTree, oMachine, GetFilePart(m_oOpt.Item("arg" & lIdx))) Then
                        Err.Raise vbObjectError, , "Cannot init codegen: " & oCodegen.LastError
                    End If
                    If Not oCodegen.SetOptimize(C_Lng(m_oOpt.Item("-O"))) Then
                        Err.Raise vbObjectError, , "Cannot set optimize: " & oCodegen.LastError
                    End If
                End If
                If Not oCodegen.CodeGenTop(vNode) Then
                    ConsoleError "%2: %3: %1" & vbCrLf, oCodegen.LastError, Join(m_oParser.CalcLine(JsonItem(C_Obj(vNode), "Offset")), ":"), "codegen"
                    GoTo QH
                End If
                If Not oJIT Is Nothing Then
                    If oCodegen.GetFunction("__anon_expr") <> 0 Then
                        If Not oJIT.AddModule(oCodegen) Then
                            ConsoleError "%2: %3: %1" & vbCrLf, oJIT.LastError, Join(m_oParser.CalcLine(JsonItem(C_Obj(vNode), "Offset")), ":"), "JIT error"
                            GoTo QH
                        End If
                        If oJIT.Invoke("__anon_expr", dblResult) Then
                            ConsolePrint "Evaluated to %1" & vbCrLf, dblResult
                        End If
                        oJIT.RemoveModule oCodegen
                        Set oCodegen = Nothing
                    End If
                End If
            Next
        End If
        If Not oCodegen Is Nothing Then
            If m_oOpt.Item("-emit-llvm") Then
                sOutput = sOutput & oCodegen.GetString() & vbCrLf
            Else
                If Not oCodegen.EmitToFile(sOutFile) Then
                    ConsoleError "%2: %3: %1" & vbCrLf, oCodegen.LastError, Join(m_oParser.CalcLine(1), ":"), "emit"
                    GoTo QH
                End If
                If Not m_oOpt.Item("-q") Then
                    ConsoleError "File " & sOutFile & " emitted successfully" & vbCrLf
                End If
            End If
        End If
    Next
    If LenB(sOutput) = 0 Then
        GoTo QH
    End If
    '--- write output
    If InIde Then
        Clipboard.Clear
        Clipboard.SetText sOutput
    End If
    If LenB(sOutFile) > 0 Then
        SetFileLen sOutFile, Len(sOutput)
        nFile = FreeFile
        Open sOutFile For Binary Access Write Shared As nFile
        Put nFile, , sOutput
        Close nFile
        If Not m_oOpt.Item("-q") Then
            ConsoleError "File " & sOutFile & " emitted successfully" & vbCrLf
        End If
    Else
        ConsolePrint sOutput
    End If
QH:
    Exit Function
EH:
    ConsoleError "Critical error: " & Err.Description & vbCrLf
    Process = 100
End Function

Public Function CallNoParam(ByVal pfn As Long) As Double
    RtccPatchProto AddressOf mdMain.CallNoParam
    CallNoParam = CallNoParam(pfn)
End Function
