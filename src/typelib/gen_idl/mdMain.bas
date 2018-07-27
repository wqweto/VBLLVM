Attribute VB_Name = "mdMain"
'=========================================================================
'
' VBLLVM Project
' GenIdl (c) 2018 by wqweto@gmail.com
'
' Typelib Generator
'
'=========================================================================
Option Explicit
DefObj A-Z

'=========================================================================
' Constants and member variables
'=========================================================================

Private Const STR_VERSION           As String = "0.1"

Private Type UcsStateType
    ExportFuncs     As Collection
    Typedefs        As Collection
    Funcs           As Collection
    MapTypes        As Collection
    SrcTypedefs     As Collection
    SrcStructs      As Collection
    SrcFunc         As Collection
End Type

'=========================================================================
' Functions
'=========================================================================

Private Sub Main()
    Dim oOpt            As Object
    Dim vResult         As Variant
    Dim sContents       As String
    Dim cFiles          As Collection
    Dim vElem           As Variant
    Dim sFile           As String
    Dim lIdx            As Long
    Dim cOutput         As Collection
    Dim uState          As UcsStateType
    
    On Error GoTo EH
    Set oOpt = GetOpt(SplitArgs(Command$), "o:def")
    If Not oOpt.Item("-nologo") And Not oOpt.Item("-q") Then
        ConsoleError "GenIdl " & STR_VERSION & " (c) 2018 by wqweto@gmail.com (" & VbPegParserVersion & ")" & vbCrLf & vbCrLf
    End If
    If LenB(oOpt.Item("error")) <> 0 Then
        ConsoleError "Error in command line: " & oOpt.Item("error") & vbCrLf & vbCrLf
        If Not (oOpt.Item("-h") Or oOpt.Item("-?") Or oOpt.Item("arg1") = "?") Then
            Exit Sub
        End If
    End If
    If oOpt.Item("numarg") = 0 Or oOpt.Item("-h") Or oOpt.Item("-?") Or oOpt.Item("arg1") = "?" Then
        ConsoleError "Usage: %1.exe [options] <include_files> <include_dirs> ..." & vbCrLf & vbCrLf, App.EXEName
        ConsoleError "Options:" & vbCrLf & _
            "  -o OUTFILE      output .idl file [default: stdout]" & vbCrLf & _
            "  -def DEFFILE    input .def file" & vbCrLf & _
            "  -json           dump includes parser result" & vbCrLf & _
            "  -types          dump types in use" & vbCrLf
        Exit Sub
    End If
    If LenB(oOpt.Item("-def")) <> 0 Then
        Set uState.ExportFuncs = New Collection
        For Each vElem In Split(ReadTextFile(oOpt.Item("-def")), vbCrLf)
            vElem = Split(Trim$(vElem), "=")
            If LenB(At(vElem, 1)) <> 0 Then
                uState.ExportFuncs.Add At(vElem, 0), At(vElem, 0)
            End If
        Next
        ConsoleError "Info: %1 exports collected" & vbCrLf, uState.ExportFuncs.Count
    End If
    Set cFiles = New Collection
    For lIdx = 1 To oOpt.Item("numarg")
        sFile = oOpt.Item("arg" & lIdx)
        If FileExists(sFile) Then
            If (GetAttr(sFile) And vbDirectory) <> 0 Then
                EnumRecursiveFiles sFile, "*.h", RetVal:=cFiles
            Else
                cFiles.Add sFile
            End If
        End If
    Next
    If cFiles.Count = 0 Then
        ConsoleError "Error: No include files found" & vbCrLf
    Else
        ConsoleError "Info: %1 includes processing" & vbCrLf, cFiles.Count
    End If
    Set uState.Typedefs = New Collection
    Set uState.Funcs = New Collection
    Set uState.MapTypes = New Collection
    Set uState.SrcTypedefs = New Collection
    Set uState.SrcStructs = New Collection
    Set uState.SrcFunc = New Collection
    Set cOutput = New Collection
    For Each vElem In cFiles
        sContents = ReadTextFile(CStr(vElem))
        If VbPegMatch(sContents & ";", Result:=vResult) = 0 Then
            ConsoleError "Error parsing %1" & vbCrLf, vElem
        End If
        If oOpt.Item("-json") Then
            cOutput.Add JsonDump(vResult)
        Else
            pvIncludeFile uState, C_Obj(vResult)
        End If
    Next
    If oOpt.Item("-json") Then
        GoTo QH
    End If
    ConsoleError "Info: %1 typedefs and %2 functions found" & vbCrLf, uState.Typedefs.Count, uState.Funcs.Count
    For Each vElem In uState.Funcs
        pvOutputFunc uState, C_Obj(vElem)
    Next
    If Not uState.ExportFuncs Is Nothing Then
        If uState.ExportFuncs.Count > 0 Then
            ConsoleError "Info: %1 exports not found in includes: %2" & vbCrLf, uState.ExportFuncs.Count, ConcatCollection(uState.ExportFuncs, ", ")
        End If
    End If
    For Each vElem In uState.Typedefs
        pvOutputStruct uState, C_Obj(vElem)
    Next
    If oOpt.Item("-types") Then
        For Each vElem In uState.MapTypes
            cOutput.Add Join(vElem, ", ")
        Next
        GoTo QH
    End If
    For Each vElem In uState.Typedefs
        pvOutputTypedef uState, C_Obj(vElem)
    Next
    uState.SrcTypedefs.Add vbNullString
    For Each vElem In uState.Typedefs
        pvOutputEnum uState, C_Obj(vElem)
    Next
    cOutput.Add _
            "[" & vbCrLf & _
            "  uuid(674f20c8-73f6-4a9f-84d0-e6f273faab84)," & vbCrLf & _
            "  version(1.0)," & vbCrLf & _
            "  helpstring(""VBLLVM Typelib 1.0 (wqweto@gmail.com)"")" & vbCrLf & _
            "]" & vbCrLf & _
            "library VBLLVM" & vbCrLf & _
            "{" & vbCrLf & _
            "    importlib(""stdole2.tlb"");" & vbCrLf & vbCrLf & _
            "    typedef unsigned char BYTE;"
    cOutput.Add ConcatCollection(uState.SrcTypedefs, vbCrLf)
    cOutput.Add ConcatCollection(uState.SrcStructs, vbCrLf)
    cOutput.Add _
            "" & vbCrLf & _
            "    [dllname(""vbllvm"")]" & vbCrLf & _
            "    module VBLLVM" & vbCrLf & _
            "    {"
    cOutput.Add ConcatCollection(uState.SrcFunc, vbCrLf)
    cOutput.Add _
            "    }" & vbCrLf & _
            "}" & vbCrLf
QH:
    If InIde Then
        Clipboard.Clear
        Clipboard.SetText ConcatCollection(cOutput, vbCrLf)
    End If
    If LenB(oOpt.Item("-o")) <> 0 Then
        WriteTextFile oOpt.Item("-o"), ConcatCollection(cOutput, vbCrLf), ucsFltAnsi
        If Not FileExists(oOpt.Item("-o")) Then
            ConsoleError "Error: Writing %1" & vbCrLf, oOpt.Item("-o")
            Exit Sub
        End If
        ConsoleError "Succesfully generated '%1'" & vbCrLf & vbCrLf, oOpt.Item("-o")
    Else
        ConsolePrint ConcatCollection(cOutput, vbCrLf)
    End If
    Exit Sub
EH:
    ConsoleError "Critical error: " & Err.Description & vbCrLf
End Sub

Private Sub pvIncludeFile(uState As UcsStateType, oInclude As Object)
    Dim vKey            As Variant
    Dim oItem           As Object
    
    For Each vKey In JsonKeys(oInclude)
        Set oItem = JsonItem(oInclude, vKey)
        Select Case JsonItem(oItem, "Tag")
        Case "TypedefDecl"
            If Not SearchCollection(uState.Typedefs, JsonItem(oItem, "Name")) Then
                uState.Typedefs.Add oItem, JsonItem(oItem, "Name")
            End If
        Case "TypedefCallback"
            uState.Typedefs.Add oItem, JsonItem(oItem, "Name")
        Case "StructDecl"
            uState.Typedefs.Add oItem, JsonItem(oItem, "Name")
        Case "EnumDecl"
            If Not IsEmpty(JsonItem(oItem, "Name")) Then
                uState.Typedefs.Add oItem, JsonItem(oItem, "Name")
            End If
        Case "FunDecl"
            If Not SearchCollection(uState.Funcs, JsonItem(oItem, "Name")) Then
                uState.Funcs.Add oItem, JsonItem(oItem, "Name")
            End If
        End Select
    Next
End Sub

Private Sub pvOutputFunc(uState As UcsStateType, oItem As Object)
    Const STR_LLVMISA   As String = "Argument|BasicBlock|InlineAsm|User|Constant|BlockAddress|ConstantAggregateZero|ConstantArray|ConstantDataSequential|ConstantDataArray|ConstantDataVector|ConstantExpr|ConstantFP|ConstantInt|ConstantPointerNull|ConstantStruct|ConstantTokenNone|ConstantVector|GlobalValue|GlobalAlias|GlobalObject|Function|GlobalVariable|UndefValue|Instruction|BinaryOperator|CallInst|IntrinsicInst|DbgInfoIntrinsic|DbgDeclareInst|MemIntrinsic|MemCpyInst|MemMoveInst|MemSetInst|CmpInst|FCmpInst|ICmpInst|ExtractElementInst" & _
                                    "|GetElementPtrInst|InsertElementInst|InsertValueInst|LandingPadInst|PHINode|SelectInst|ShuffleVectorInst|StoreInst|TerminatorInst|BranchInst|IndirectBrInst|InvokeInst|ReturnInst|SwitchInst|UnreachableInst|ResumeInst|CleanupReturnInst|CatchReturnInst|FuncletPadInst|CatchPadInst|CleanupPadInst|UnaryInstruction|AllocaInst|CastInst|AddrSpaceCastInst|BitCastInst|FPExtInst|FPToSIInst|FPToUIInst|FPTruncInst|IntToPtrInst|PtrToIntInst|SExtInst|SIToFPInst|TruncInst|UIToFPInst|ZExtInst|ExtractValueInst|LoadInst|VAArgInst"
    Const IDENT         As Long = 8
    Dim sName           As String
    Dim sText           As String
    Dim lCount          As Long
    Dim lIdx            As Long
    Dim vKey            As Variant
    Dim oParam          As Object
    Dim sType           As String
    Dim sDirection      As String
    
    If IsObject(JsonItem(oItem, "Params")) Then
        lCount = UBound(JsonKeys(oItem, "Params")) + 1
        If lCount = 1 And JsonItem(oItem, "Params/0/Type") = "void" Then
            lCount = 0
        End If
    End If
    sName = Replace(JsonItem(oItem, "Name"), "##TargetName##", "X86")
    If sName = "LLVMSetFunctionCallConv" Then
        JsonItem(oItem, "Params/1/Type") = "LLVMCallConv"
    ElseIf InStr(sName, "##name") > 0 Then  ' LLVMIsA##name
        For Each vKey In Split(STR_LLVMISA, "|")
            JsonItem(oItem, "Name") = Replace(sName, "##name", vKey)
            pvOutputFunc uState, oItem
        Next
        Exit Sub
    ElseIf InStr(sName, "#") > 0 Then
        ConsoleError "Info: Skipping function %1" & vbCrLf, sName
        Exit Sub
    ElseIf sName = "LLVMGetTargetDescription" Then
        sName = sName
    End If
    sText = Space$(IDENT) & "[entry(""" & sName & """)]" & vbCrLf & _
            Space$(IDENT + 8) & pvToIdlType(uState, JsonItem(oItem, "Type"), ReturnType:=True) & " " & _
                sName & "(" & IIf(lCount > 0, vbCrLf, vbNullString)
    If lCount > 0 Then
        lIdx = 1
        For Each vKey In JsonKeys(oItem, "Params")
            Set oParam = JsonItem(oItem, "Params/" & vKey)
            sType = pvToIdlType(uState, JsonItem(oParam, "Type"), sDirection)
            sText = sText & Space$(IDENT + 16) & "[" & sDirection & "] " & sType & _
                " " & Zn(JsonItem(oParam, "Name"), "p" & lIdx) & IIf(lIdx < lCount, "," & vbCrLf, vbNullString)
            lIdx = lIdx + 1
        Next
    End If
    sText = sText & ");"
    If Not uState.ExportFuncs Is Nothing Then
        If Not SearchCollection(uState.ExportFuncs, sName) Then
            ConsoleError "Info: Not exported function %1" & vbCrLf, sName
            sText = "/*" & Mid$(sText, 3) & " // not exported" & vbCrLf & "*/"
        Else
            uState.ExportFuncs.Remove sName
        End If
    End If
    uState.SrcFunc.Add sText
End Sub

Private Sub pvOutputStruct(uState As UcsStateType, oItem As Object)
    Const IDENT         As Long = 4
    Dim sText           As String
    Dim lCount          As Long
    Dim vKey            As Variant
    Dim oElem           As Object
    Dim lIdx            As Long
        
    If IsObject(JsonItem(oItem, "Items")) Then
        lCount = UBound(JsonKeys(oItem, "Items")) + 1
    End If
    If LenB(JsonItem(oItem, "Name")) = 0 Or lCount = 0 Then
        Exit Sub
    End If
    Select Case JsonItem(oItem, "Tag")
    Case "StructDecl"
        sText = Space$(IDENT) & "typedef struct {" & vbCrLf
        lIdx = 1
        For Each vKey In JsonKeys(oItem, "Items")
            Set oElem = JsonItem(oItem, "Items/" & vKey)
            sText = sText & Space$(IDENT + 4) & pvToIdlType(uState, JsonItem(oElem, "Type")) & _
                " " & Zn(JsonItem(oElem, "Name"), "m" & lIdx) & ";" & vbCrLf
            lIdx = lIdx + 1
        Next
        sText = sText & Space$(IDENT) & "} " & JsonItem(oItem, "Name") & ";"
    End Select
    If LenB(sText) <> 0 Then
        If Not JsonItem(oItem, "Used") Then
            sText = "/*" & Mid$(sText, 3) & " // not used" & vbCrLf & "*/"
        Else
            sText = sText & vbCrLf
        End If
        uState.SrcStructs.Add sText
    End If
End Sub

Private Sub pvOutputTypedef(uState As UcsStateType, oItem As Object)
    Const IDENT         As Long = 4
    Dim sText           As String
    
    Select Case JsonItem(oItem, "Tag")
    Case "TypedefDecl"
        sText = Space$(IDENT) & "typedef [public] LONG " & JsonItem(oItem, "Name") & ";"
    Case "TypedefCallback"
        sText = Space$(IDENT) & "typedef [public] LONG " & JsonItem(oItem, "Name") & "; // callback"
    End Select
    If LenB(sText) <> 0 Then
        If Not JsonItem(oItem, "Used") Then
            sText = "/* " & Mid$(sText, 3) & " // not used" & vbCrLf & "*/"
        End If
        uState.SrcTypedefs.Add sText
    End If
End Sub

Private Sub pvOutputEnum(uState As UcsStateType, oItem As Object)
    Const IDENT         As Long = 4
    Dim sText           As String
    Dim lCount          As Long
    Dim vKey            As Variant
    Dim oElem           As Object
        
    If IsObject(JsonItem(oItem, "Items")) Then
        lCount = UBound(JsonKeys(oItem, "Items")) + 1
    End If
    If LenB(JsonItem(oItem, "Name")) = 0 Or lCount = 0 Then
        Exit Sub
    End If
    Select Case JsonItem(oItem, "Tag")
    Case "EnumDecl"
        sText = Space$(IDENT) & "typedef enum {" & vbCrLf
        For Each vKey In JsonKeys(oItem, "Items")
            Set oElem = JsonItem(oItem, "Items/" & vKey)
            sText = sText & Space$(IDENT + 4) & JsonItem(oElem, "Name")
            If Not IsEmpty(JsonItem(oElem, "Value")) Then
                sText = sText & " = " & JsonItem(oElem, "Value") & "," & vbCrLf
            Else
                sText = sText & "," & vbCrLf
            End If
        Next
        sText = sText & Space$(IDENT) & "} " & JsonItem(oItem, "Name") & ";"
    End Select
    If LenB(sText) <> 0 Then
        If Not JsonItem(oItem, "Used") Then
            sText = "/*" & Mid$(sText, 3) & " // not used" & vbCrLf & "*/"
        Else
            sText = sText & vbCrLf
        End If
        uState.SrcTypedefs.Add sText
    End If
End Sub

Private Function pvToIdlType( _
            uState As UcsStateType, _
            sType As String, _
            Optional sDirection As String, _
            Optional ByVal ReturnType As Boolean) As String
    Dim sKey            As String
    Dim vArray          As Variant
    Dim sSuffix         As String
    
    If SearchCollection(uState.MapTypes, ReturnType & "#" & sType) Then
        vArray = uState.MapTypes.Item(ReturnType & "#" & sType)
        pvToIdlType = vArray(0)
        sDirection = vArray(1)
        Exit Function
    End If
    sDirection = "in"
    sKey = Replace(Replace(Replace(Replace(sType, _
                "const ", vbNullString), _
                "struct ", vbNullString), _
                "enum ", vbNullString), _
                " ", vbNullString)
    Select Case sKey
    Case "char*"
        If ReturnType Then
            pvToIdlType = "LONG"
        Else
            pvToIdlType = "LPSTR"
        End If
    Case "void*"
        pvToIdlType = "LONG"
    Case "char**"
        sDirection = "in, out"
        pvToIdlType = "LONG *"
    Case Else
        If Right$(sKey, 1) = "*" Then
            sKey = Left$(sKey, Len(sKey) - 1)
            sDirection = "in, out"
            sSuffix = " *"
        End If
        Select Case sKey
        Case "int", "unsigned", "size_t", "uint32_t", "uintptr_t"
            pvToIdlType = "LONG" & sSuffix
        Case "uint64_t", "longlong", "unsignedlonglong"
            pvToIdlType = "CURRENCY" & sSuffix
        Case "uint8_t"
            pvToIdlType = "BYTE" & sSuffix
        Case "double"
            pvToIdlType = "DOUBLE" & sSuffix
        Case "void"
            pvToIdlType = Trim$(sType)
        Case Else
            If SearchCollection(uState.Typedefs, sKey) Then
                JsonItem(uState.Typedefs.Item(sKey), "Used") = True
                pvToIdlType = Replace(Replace(Trim$(sType), _
                                    "struct ", vbNullString), _
                                    "enum ", vbNullString)
            Else
                ConsoleError "Unknown data-type '" & sType & "'. Will default to LONG" & vbCrLf
                sDirection = "in"
                pvToIdlType = "LONG"
            End If
        End Select
    End Select
    uState.MapTypes.Add Array(pvToIdlType, sDirection, sType), ReturnType & "#" & sType
End Function

Public Function IsRefType(sType As String) As Boolean
    If Left$(sType, 4) = "LLVM" Then
        IsRefType = True
    ElseIf Left$(sType, 5) = "llvm_" Then
        IsRefType = True
    End If
End Function



