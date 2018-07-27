Attribute VB_Name = "mdMain"
'=========================================================================
'
' kscope (c) 2018 by wqweto@gmail.com
'
' Kaleidoscope toy language for VBLLVM
'
' mdMain.bas - Global functions
'
'=========================================================================
Option Explicit
DefObj A-Z

#Const HasIVbCollection = False

'=========================================================================
' API
'=========================================================================

'--- for CreateFile
Private Const GENERIC_WRITE                 As Long = &H40000000
Private Const OPEN_EXISTING                 As Long = 3
Private Const FILE_SHARE_READ               As Long = &H1

Private Declare Function CommandLineToArgvW Lib "shell32" (ByVal lpCmdLine As Long, pNumArgs As Long) As Long
Private Declare Function LocalFree Lib "kernel32" (ByVal hMem As Long) As Long
Private Declare Sub CopyMemory Lib "kernel32" Alias "RtlMoveMemory" (Destination As Any, Source As Any, ByVal Length As Long)
Private Declare Function ApiSysAllocString Lib "oleaut32" Alias "SysAllocString" (ByVal Ptr As Long) As Long
Private Declare Function GetFileAttributes Lib "kernel32" Alias "GetFileAttributesA" (ByVal lpFileName As String) As Long
Private Declare Function IsTextUnicode Lib "advapi32" (lpBuffer As Any, ByVal cb As Long, lpi As Long) As Long
Public Declare Function EmptyLongArray Lib "oleaut32" Alias "SafeArrayCreateVector" (Optional ByVal vt As VbVarType = vbLong, Optional ByVal lLow As Long = 0, Optional ByVal lCount As Long = 0) As Long()
Private Declare Function CreateFile Lib "kernel32" Alias "CreateFileA" (ByVal lpFileName As String, ByVal dwDesiredAccess As Long, ByVal dwShareMode As Long, ByVal lpSecurityAttributes As Long, ByVal dwCreationDisposition As Long, ByVal dwFlagsAndAttributes As Long, ByVal hTemplateFile As Long) As Long
Private Declare Function SetFilePointer Lib "kernel32" (ByVal hFile As Long, ByVal lDistanceToMove As Long, ByVal lpDistanceToMoveHigh As Long, ByVal dwMoveMethod As Long) As Long
Private Declare Function SetEndOfFile Lib "kernel32" (ByVal hFile As Long) As Long
Private Declare Function CloseHandle Lib "kernel32" (ByVal hObject As Long) As Long
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
    
    lExitCode = Process(SplitArgs(Command$))
    If Not InIde Then
        Call ExitProcess(lExitCode)
    End If
End Sub

Private Function Process(vArgs As Variant) As Long
    Dim sOutFile        As String
    Dim nFile           As Integer
    Dim sOutput         As String
    Dim lIdx            As Long
    Dim oTree           As Scripting.Dictionary
    
    On Error GoTo EH
    Set m_oParser = New cParser
    Set m_oOpt = GetOpt(vArgs, "o")
    If Not m_oOpt.Item("-nologo") And Not m_oOpt.Item("-q") Then
        ConsoleError App.ProductName & " " & STR_VERSION & " (c) 2018 by wqweto@gmail.com (" & m_oParser.ParserVersion & ")" & vbCrLf & vbCrLf
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
            "  -emit-ir        output intermediate represetation" & vbCrLf & _
            "  -q              in quiet operation outputs only errors" & vbCrLf & _
            "  -nologo         suppress startup banner" & vbCrLf & _
            "If no -emit-xxx is used emits executable. If no -o is used writes result to console." & vbCrLf
        If m_oOpt.Item("numarg") = 0 Then
            Process = 100
        End If
        Exit Function
    End If
    sOutFile = m_oOpt.Item("-o")
    For lIdx = 1 To m_oOpt.Item("numarg")
        Set oTree = m_oParser.MatchFile(m_oOpt.Item("arg" & lIdx))
        If LenB(m_oParser.LastError) <> 0 Then
            ConsoleError "%2: %3: %1" & vbCrLf, m_oParser.LastError, Join(m_oParser.CalcLine(m_oParser.LastOffset + 1), ":"), IIf(oTree Is Nothing, "error", "warning")
        End If
    Next
    If m_oOpt.Item("-emit-tree") Then
        sOutput = JsonDump(oTree)
    Else
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
    Exit Function
EH:
    ConsoleError "Critical error: " & Err.Description & vbCrLf
    Process = 100
End Function

Private Function GetOpt(vArgs As Variant, Optional OptionsWithArg As String) As Object
    Dim oRetVal         As Object
    Dim lIdx            As Long
    Dim bNoMoreOpt      As Boolean
    Dim vOptArg         As Variant
    Dim vElem           As Variant
    Dim sValue          As String

    vOptArg = Split(OptionsWithArg, ":")
    Set oRetVal = CreateObject("Scripting.Dictionary")
    With oRetVal
        .CompareMode = vbTextCompare
        For lIdx = 0 To UBound(vArgs)
            Select Case Left$(At(vArgs, lIdx), 1 + bNoMoreOpt)
            Case "-", "/"
                For Each vElem In vOptArg
                    If Mid$(At(vArgs, lIdx), 2, Len(vElem)) = vElem Then
                        If Mid(At(vArgs, lIdx), Len(vElem) + 2, 1) = ":" Then
                            sValue = Mid$(At(vArgs, lIdx), Len(vElem) + 3)
                        ElseIf Len(At(vArgs, lIdx)) > Len(vElem) + 1 Then
                            sValue = Mid$(At(vArgs, lIdx), Len(vElem) + 2)
                        ElseIf LenB(At(vArgs, lIdx + 1)) <> 0 Then
                            sValue = At(vArgs, lIdx + 1)
                            lIdx = lIdx + 1
                        Else
                            .Item("error") = "Option -" & vElem & " requires an argument"
                        End If
                        If Not .Exists("-" & vElem) Then
                            .Item("-" & vElem) = sValue
                        Else
                            .Item("#" & vElem) = .Item("#" & vElem) + 1
                            .Item("-" & vElem & .Item("#" & vElem)) = sValue
                        End If
                        GoTo Continue
                    End If
                Next
                .Item("-" & Mid$(At(vArgs, lIdx), 2)) = True
            Case Else
                .Item("numarg") = .Item("numarg") + 1
                .Item("arg" & .Item("numarg")) = At(vArgs, lIdx)
            End Select
Continue:
        Next
    End With
    Set GetOpt = oRetVal
End Function

Public Function SplitArgs(sText As String) As Variant
    Dim vRetVal         As Variant
    Dim lPtr            As Long
    Dim lArgc           As Long
    Dim lIdx            As Long
    Dim lArgPtr         As Long

    If LenB(sText) <> 0 Then
        lPtr = CommandLineToArgvW(StrPtr(sText), lArgc)
    End If
    If lArgc > 0 Then
        ReDim vRetVal(0 To lArgc - 1) As String
        For lIdx = 0 To UBound(vRetVal)
            Call CopyMemory(lArgPtr, ByVal lPtr + 4 * lIdx, 4)
            vRetVal(lIdx) = SysAllocString(lArgPtr)
        Next
    Else
        vRetVal = Split(vbNullString)
    End If
    Call LocalFree(lPtr)
    SplitArgs = vRetVal
End Function

Private Function SysAllocString(ByVal lPtr As Long) As String
    Dim lTemp           As Long

    lTemp = ApiSysAllocString(lPtr)
    Call CopyMemory(ByVal VarPtr(SysAllocString), lTemp, 4)
End Function

Public Function ReadTextFile(sFile As String) As String
    Const ForReading    As Long = 1
    Const BOM_UTF       As String = "﻿"   '--- "\xEF\xBB\xBF"
    Const BOM_UNICODE   As String = "��"    '--- "\xFF\xFE"
    Dim lSize           As Long
    Dim sPrefix         As String
    Dim nFile           As Integer
    Dim sCharset        As String
    Dim oStream         As Object
    
    '--- get file size
    On Error GoTo EH
    If FileExists(sFile) Then
        lSize = FileLen(sFile)
    End If
    If lSize = 0 Then
        Exit Function
    End If
    '--- read first 50 chars
    nFile = FreeFile
    Open sFile For Binary Access Read Shared As nFile
    sPrefix = String$(IIf(lSize < 50, lSize, 50), 0)
    Get nFile, , sPrefix
    Close nFile
    '--- figure out charset
    If Left$(sPrefix, 3) = BOM_UTF Then
        sCharset = "UTF-8"
    ElseIf Left$(sPrefix, 2) = BOM_UNICODE Or IsTextUnicode(ByVal sPrefix, Len(sPrefix), &HFFFF& - 2) <> 0 Then
        sCharset = "Unicode"
    ElseIf InStr(1, sPrefix, "<?xml", vbTextCompare) > 0 And InStr(1, sPrefix, "utf-8", vbTextCompare) > 0 Then
        '--- special xml encoding test
        sCharset = "UTF-8"
    End If
    '--- plain text: direct VB6 read
    If LenB(ReadTextFile) = 0 And LenB(sCharset) = 0 Then
        nFile = FreeFile
        Open sFile For Binary Access Read Shared As nFile
        ReadTextFile = String$(lSize, 0)
        Get nFile, , ReadTextFile
        Close nFile
    End If
    '--- plain text + unicode: use FileSystemObject
    If LenB(ReadTextFile) = 0 And sCharset <> "UTF-8" Then
        On Error Resume Next  '--- checked
        ReadTextFile = CreateObject("Scripting.FileSystemObject").OpenTextFile(sFile, ForReading, False, sCharset = "Unicode").ReadAll()
        On Error GoTo EH
    End If
    '--- plain text + unicode + utf-8: use ADODB.Stream
    If LenB(ReadTextFile) = 0 Then
        Set oStream = CreateObject("ADODB.Stream")
        With oStream
            .Open
            If LenB(sCharset) <> 0 Then
                .Charset = sCharset
            End If
            .LoadFromFile sFile
            ReadTextFile = .ReadText()
        End With
    End If
    Exit Function
EH:
End Function

Public Function FileExists(sFile As String) As Boolean
    If GetFileAttributes(sFile) = -1 Then ' INVALID_FILE_ATTRIBUTES
    Else
        FileExists = True
    End If
End Function

Public Function At(vArray As Variant, ByVal lIdx As Long) As Variant
    On Error GoTo QH
    If lIdx >= LBound(vArray) And lIdx <= UBound(vArray) Then
        At = vArray(lIdx)
    End If
QH:
End Function

Public Function ConcatCollection(oCol As Collection, Optional Separator As String) As String
    Dim lSize           As Long
    Dim vElem           As Variant
    
    For Each vElem In oCol
        lSize = lSize + Len(vElem) + Len(Separator)
    Next
    If lSize > 0 Then
        ConcatCollection = String$(lSize - Len(Separator), 0)
        lSize = 1
        For Each vElem In oCol
            If lSize <= Len(ConcatCollection) Then
                Mid$(ConcatCollection, lSize, Len(vElem) + Len(Separator)) = vElem & Separator
            End If
            lSize = lSize + Len(vElem) + Len(Separator)
        Next
    End If
End Function

Public Property Get InIde() As Boolean
    Debug.Assert pvSetTrue(InIde)
End Property

Private Function pvSetTrue(bValue As Boolean) As Boolean
    bValue = True
    pvSetTrue = True
End Function

Public Function GetFilePart(sFileName As String) As String
    GetFilePart = Mid$(sFileName, InStrRev(sFileName, "\") + 1)
    If InStrRev(GetFilePart, ".") > 0 Then
        GetFilePart = Left$(GetFilePart, InStrRev(GetFilePart, ".") - 1)
    End If
End Function

Public Function SetFileLen(sFile As String, ByVal lSize As Long) As Boolean
    Dim hFile       As Long
    
    hFile = CreateFile(sFile, GENERIC_WRITE, FILE_SHARE_READ, 0, OPEN_EXISTING, 0, 0)
    If hFile <> 0 Then
        If SetFilePointer(hFile, lSize, 0, 0) <> -1 Then
            If SetEndOfFile(hFile) <> 0 Then
                SetFileLen = True
            End If
        End If
        Call CloseHandle(hFile)
    End If
End Function

#If HasIVbCollection Then
    Public Function SearchCollection(oCol As IVbCollection, Index As Variant) As Boolean
        SearchCollection = (oCol.Item(Index) >= 0)
    End Function
#Else
    Public Function SearchCollection(oCol As Collection, Index As Variant) As Boolean
        On Error GoTo QH
        oCol.Item Index
        SearchCollection = True
QH:
    End Function
#End If

Public Function Zn(sText As String, Optional IfEmptyString As Variant = Null) As Variant
    Zn = IIf(LenB(sText) = 0, IfEmptyString, sText)
End Function

Public Function C_Bool(Value As Variant) As Boolean
    On Error GoTo QH
    If LenB(Value) <> 0 Then
        C_Bool = CBool(Value)
    End If
QH:
End Function

Public Function C_Lng(Value As Variant) As Long
    On Error GoTo QH
    C_Lng = CLng(Value)
QH:
End Function

Public Function C_Str(Value As Variant) As String
    On Error GoTo QH
    C_Str = CStr(Value)
QH:
End Function

Public Function C_Dbl(Value As Variant) As Double
    On Error GoTo QH
    C_Dbl = CDbl(Value)
QH:
End Function

Public Function CanonicalPath(sPath As String) As String
    With CreateObject("Scripting.FileSystemObject")
        CanonicalPath = .GetAbsolutePathName(sPath)
    End With
End Function

Public Function PathDifference(sBase As String, sFolder As String) As String
    Dim vBase           As Variant
    Dim vFolder         As Variant
    Dim lIdx            As Long
    Dim lJdx            As Long
    
    If LCase$(Left$(sBase, 2)) <> LCase$(Left$(sFolder, 2)) Then
        PathDifference = sFolder
    Else
        vBase = Split(sBase, "\")
        vFolder = Split(sFolder, "\")
        For lIdx = 0 To UBound(vFolder)
            If lIdx <= UBound(vBase) Then
                If LCase$(vBase(lIdx)) <> LCase$(vFolder(lIdx)) Then
                    Exit For
                End If
            Else
                Exit For
            End If
        Next
        If lIdx > UBound(vBase) Then
'            PathDifference = "."
        Else
            For lJdx = lIdx To UBound(vBase)
                PathDifference = PathDifference & IIf(LenB(PathDifference) <> 0, "\", vbNullString) & ".."
            Next
        End If
        For lJdx = lIdx To UBound(vFolder)
            PathDifference = PathDifference & IIf(LenB(PathDifference) <> 0, "\", vbNullString) & vFolder(lJdx)
        Next
    End If
End Function

Public Function PathMerge(sBase As String, sFolder As String) As String
    If Mid$(sFolder, 2, 1) = ":" Or Left$(sFolder, 2) = "\\" Then
        PathMerge = sFolder
    ElseIf Left$(sFolder, 1) = "\" Then
        PathMerge = Left$(sBase, 2) & sFolder
    Else
        PathMerge = PathCombine(sBase, sFolder)
    End If
    PathMerge = CanonicalPath(PathMerge)
End Function

Public Function PathCombine(sPath As String, sFile As String) As String
    PathCombine = sPath & IIf(LenB(sPath) <> 0 And Right$(sPath, 1) <> "\" And LenB(sFile) <> 0, "\", vbNullString) & sFile
End Function

Public Function Split2(sText As String, sDelim As String) As Variant
    Dim lPos            As Long
    
    lPos = InStr(sText, sDelim)
    If lPos > 0 Then
        Split2 = Array(Left$(sText, lPos - 1), Mid$(sText, lPos + Len(sDelim)))
    Else
        Split2 = Array(sText)
    End If
End Function

Public Sub AssignVariant(vDest As Variant, vSrc As Variant)
    If IsObject(vSrc) Then
        Set vDest = vSrc
    Else
        vDest = vSrc
    End If
End Sub

