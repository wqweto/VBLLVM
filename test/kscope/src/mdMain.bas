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
Private Declare Function DeleteFile Lib "kernel32" Alias "DeleteFileA" (ByVal lpFileName As String) As Long

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
    Dim lJdx            As Long
    Dim oTree           As Object
    Dim oCodegen        As cCodegen
    Dim oJIT            As cJIT
    Dim vNode           As Variant
    Dim dblResult       As Double
    Dim oMachine        As cTargetMachine
    Dim cObjFiles       As Collection
    Dim sObjPath        As String
    Dim sObjFile        As String
    Dim sCmd            As String
    Dim vElem           As Variant
    Dim sTriple         As String
    Dim sLinker         As String
    Dim lOptLevel       As Long
    Dim lSizeLevel      As Long
    
    On Error GoTo EH
    Set m_oParser = New cParser
    Set m_oOpt = GetOpt(vArgs, "o:O")
    If Not m_oOpt.Item("-nologo") And Not m_oOpt.Item("-q") Then
        ConsoleError App.ProductName & " VB6 port " & STR_VERSION & ", " & ToString(LLVMGetDefaultTargetTriple()) & " (c) 2018 by wqweto@gmail.com (" & m_oParser.ParserVersion & ")" & vbCrLf & vbCrLf
    End If
    If LenB(m_oOpt.Item("error")) <> 0 Then
        ConsoleError "Error in command line: " & m_oOpt.Item("error") & vbCrLf & vbCrLf
        If Not (m_oOpt.Item("-h") Or m_oOpt.Item("-?") Or m_oOpt.Item("arg1") = "?") Then
            Process = 100
            GoTo QH
        End If
    End If
    If m_oOpt.Item("-version") Then
        ConsoleError "Available targets:" & vbCrLf
        ConsoleError "  %1" & vbCrLf, Join(AvailableTargets, vbCrLf & "  ")
        GoTo QH
    End If
    If m_oOpt.Item("numarg") = 0 Or m_oOpt.Item("-h") Or m_oOpt.Item("-?") Or m_oOpt.Item("arg1") = "?" Then
        ConsoleError "Usage: %1.exe [options] <in_file.ks>" & vbCrLf & vbCrLf, App.EXEName
        ConsoleError "Options:" & vbCrLf & _
            "  -o OUTFILE      write result to OUTFILE [default: stdout]" & vbCrLf & _
            "  -c, -emit-obj   compile to COFF .obj file only [default: exe]" & vbCrLf & _
            "  -m32            compile for win32 target [default: x64]" & vbCrLf & _
            "  -O NUM          optimization level [default: none]" & vbCrLf & _
            "  -Os -Oz         optimize for size" & vbCrLf & _
            "  -emit-tree      output parse tree" & vbCrLf & _
            "  -emit-llvm      output intermediate represetation" & vbCrLf & _
            "  -q              in quiet operation outputs only errors" & vbCrLf & _
            "  -nologo         suppress startup banner" & vbCrLf & _
            "  -version        dump available targets" & vbCrLf & _
            "If no -emit-xxx is used emits executable. If no -o is used writes result to console." & vbCrLf
        If m_oOpt.Item("numarg") = 0 Then
            Process = 100
        End If
        GoTo QH
    End If
    If m_oOpt.Item("-emit-obj") Then
        m_oOpt.Item("-c") = True
    End If
    sOutFile = m_oOpt.Item("-o")
    If InStrRev(sOutFile, "\") > 0 Then
        sObjPath = Left$(sOutFile, InStrRev(sOutFile, "\") - 1)
    End If
    Set cObjFiles = New Collection
    If Not m_oOpt.Item("-emit-tree") And Not m_oOpt.Item("-emit-llvm") And Not m_oOpt.Item("-c") And LenB(sOutFile) = 0 Then
        sTriple = ToString(LLVMGetDefaultTargetTriple())
        Set oMachine = New cTargetMachine
        If Not oMachine.Init(sTriple, IsJIT:=True) Then
            Err.Raise vbObjectError, , "Cannot init " & sTriple & ": " & oMachine.LastError
        End If
        Set oJIT = New cJIT
        If Not oJIT.Init(oMachine) Then
            Err.Raise vbObjectError, , "Cannot init JIT: " & oJIT.LastError
        End If
        ConsoleError "Using MCJIT on %1" & vbCrLf, sTriple
    Else
        sTriple = IIf(m_oOpt.Item("-m32"), "i686-pc-windows-msvc", "x86_64-pc-windows-msvc")
        Set oMachine = New cTargetMachine
        If Not oMachine.Init(sTriple) Then
            Err.Raise vbObjectError, , "Cannot init " & sTriple & ": " & oMachine.LastError
        End If
    End If
    '-- parse optimization levels/size
    lOptLevel = -1
    lSizeLevel = -1
    For lIdx = 0 To m_oOpt.Item("#O")
        vElem = m_oOpt.Item("-O" & IIf(lIdx > 0, lIdx, vbNullString))
        If IsNumeric(vElem) Then ' -O0, -O1, -O2 and  -O3
            lOptLevel = C_Lng(vElem)
            If lOptLevel < 0 Or lOptLevel > 3 Then
                ConsoleError "Invalid optimization level: %1" & vbCrLf, vElem
                GoTo QH
            End If
        ElseIf vElem = "s" Then ' -Os
            lSizeLevel = 1
        ElseIf vElem = "z" Then ' -Oz
            lSizeLevel = 2
        Else
            ConsoleError "Unknown optimization setting: %1" & vbCrLf, vElem
            GoTo QH
        End If
    Next
    For lIdx = 1 To m_oOpt.Item("numarg")
        If Not m_oOpt.Item("-q") Then
            ConsoleError "%1" & vbCrLf, PathDifference(CurDir$, m_oOpt.Item("arg" & lIdx))
        End If
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
                    If Not oCodegen.SetOptimize(lOptLevel, lSizeLevel) Then
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
            ElseIf m_oOpt.Item("-c") Then
                If Not oCodegen.EmitToFile(sOutFile) Then
                    ConsoleError "%2: %3: %1" & vbCrLf, oCodegen.LastError, Join(m_oParser.CalcLine(1), ":"), "emit"
                    GoTo QH
                End If
                If Not m_oOpt.Item("-q") Then
                    ConsoleError "File %1 emitted successfully" & vbCrLf, sOutFile
                End If
                sOutFile = vbNullString
            ElseIf LenB(sOutFile) <> 0 Then
                For lJdx = 1 To 1000
                    sObjFile = sObjPath & oCodegen.ModuleName & IIf(lJdx > 1, lJdx, vbNullString) & ".obj"
                    If Not FileExists(sObjFile) Then
                        Exit For
                    End If
                Next
                If Not oCodegen.EmitToFile(sObjFile) Then
                    ConsoleError "%2: %3: %1" & vbCrLf, oCodegen.LastError, Join(m_oParser.CalcLine(1), ":"), "emit"
                Else
                    cObjFiles.Add sObjFile
                End If
            End If
        End If
    Next
    '--- shell linker
    If cObjFiles.Count > 0 Then
        For Each vElem In cObjFiles
            sCmd = sCmd & " " & ArgvQuote(C_Str(vElem))
        Next
        sLinker = PathCombine(App.Path, IIf(InIde, "..\bin\", vbNullString) & "lib\link.bat")
        If Not FileExists(sLinker) Then
            Err.Raise vbObjectError, , "Linker " & sLinker & " not found"
        End If
        sCmd = ArgvQuote(sLinker) & " " & IIf(m_oOpt.Item("-m32"), "win32", "x64") & " " & ArgvQuote(sOutFile) & sCmd
        Call DeleteFile(sOutFile)
        CreateObject("WScript.Shell").Run sCmd, 0, True
        If FileExists(sOutFile) And Not m_oOpt.Item("-q") Then
            ConsoleError "File %1 emitted successfully" & vbCrLf, sOutFile
        End If
        For Each vElem In cObjFiles
            Call DeleteFile(vElem)
        Next
    End If
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
            ConsoleError "File %1 emitted successfully" & vbCrLf, sOutFile
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

Property Get AvailableTargets() As Variant
    Dim hTarget         As LLVMTargetRef
    Dim vRet            As Variant
    
    vRet = Array()
    hTarget = LLVMGetFirstTarget()
    Do While hTarget <> 0
        ReDim Preserve vRet(0 To UBound(vRet) + 1) As Variant
        vRet(UBound(vRet)) = ToStringCopy(LLVMGetTargetName(hTarget))
        hTarget = LLVMGetNextTarget(hTarget)
    Loop
    AvailableTargets = vRet
End Property

