Attribute VB_Name = "mdJson"
'=========================================================================
'
' VBLLVM Project
' GenIdl (c) 2018 by wqweto@gmail.com
'
' JSON functions
'
'=========================================================================
Option Explicit
DefObj A-Z
Private Const MODULE_NAME As String = "mdJson"

#Const ImplCollection = JSON_USE_COLLECTION <> 0
#Const ImplRichClient = JSON_USE_RICHCLIENT <> 0
#Const ImplScripting = False
#Const ImplUseShared = DebugMode <> 0

'=========================================================================
' API
'=========================================================================

#If VBA7 Then
Private Declare PtrSafe Sub CopyMemory Lib "kernel32" Alias "RtlMoveMemory" (Destination As Any, Source As Any, ByVal Length As LongPtr)
#Else
Private Declare Sub CopyMemory Lib "kernel32" Alias "RtlMoveMemory" (Destination As Any, Source As Any, ByVal Length As Long)
#End If

Private Const PROGID_DICTIONARY     As String = "Scripting.Dictionary"
Private Const TEXT_COMPARE          As Long = 1
Private Const STR_PREFIX            As String = "__json__"
Private Const STR_COL_KEYS          As String = STR_PREFIX & "keys"
Private Const STR_ATTR_EMPTY        As String = STR_PREFIX & "empty"
Private Const STR_ATTR_ARRAY        As String = STR_PREFIX & "array"
Private Const STR_ATTR_NAME         As String = STR_PREFIX & "name"
Private Const STR_ATTR_NIL          As String = STR_PREFIX & "nil"
Private Const STR_ATTR_BOOL         As String = STR_PREFIX & "bool"
Private Const STR_NODE_ARRAY        As String = STR_PREFIX & "array"
Private Const ERR_EXTRA_SYMBOL      As String = "Extra '%1' found at position %2"
Private Const ERR_DUPLICATE_KEY     As String = "Duplicate key '%1' found at position %2"
Private Const ERR_UNEXPECTED_SYMBOL As String = "Unexpected '%1' at position %2"
Private Const ERR_CONVERSION        As String = "%1 at position %2"
Private Const ERR_UNTERMIN_COMMENT  As String = "Unterminated comment at position %1"
Private Const ERR_MISSING_EOS       As String = "Missing end of string at position %1"
Private Const ERR_INVALID_ESCAPE    As String = "Invalid escape at position %1"
Private Const ERR_MISSING_KEY       As String = "Missing key at position %1"
Private Const ERR_EXPECTED_SYMBOL   As String = "Expected '%1' at position %2"
Private Const ERR_EXPECTED_TWO      As String = "Expected '%1' or '%2' at position %3"

Private Type JsonContext
    StrictMode          As Boolean
    Text()              As Integer
    Pos                 As Long
    Error               As String
    LastChar            As Integer
End Type

'=========================================================================
' Error management
'=========================================================================

Private Function RaiseError(sFunction As String) As VbMsgBoxResult
#If ImplUseShared Then
    Dim vErr            As Variant
    
    PushError vErr
    RaiseError = GApp.HandleOutOfMemory(vErr)
    If RaiseError <> vbRetry Then
        PopRaiseError sFunction, MODULE_NAME, vErr
    End If
#Else
    Err.Raise Err.Number, MODULE_NAME & "." & sFunction & vbCrLf & Err.Source, Err.Description
#End If
End Function

Private Function PrintError(sFunction As String) As VbMsgBoxResult
#If ImplUseShared Then
    Dim vErr            As Variant
    
    PushError vErr
    PrintError = GApp.HandleOutOfMemory(vErr)
    If PrintError <> vbRetry Then
        PopPrintError sFunction, MODULE_NAME, vErr
    End If
#Else
    Debug.Print "Error: " & Err.Description & " [" & MODULE_NAME & "." & sFunction & "]", Timer
    Debug.Assert MsgBox(Err.Description, vbCritical, MODULE_NAME & "." & sFunction) <> -1
#End If
End Function

'=========================================================================
' Functions
'=========================================================================

Public Function JsonParse( _
            sText As String, _
            Optional RetVal As Variant, _
            Optional Error As String, _
            Optional ByVal StrictMode As Boolean) As Boolean
    Const FUNC_NAME     As String = "JsonParse"
    Dim uCtx            As JsonContext

    On Error GoTo EH
    With uCtx
        .StrictMode = StrictMode
        '--- include 4 extra null chars (max look-ahead)
        ReDim .Text(0 To Len(sText) + 3) As Integer
        Call CopyMemory(.Text(0), ByVal StrPtr(sText), LenB(sText))
        AssignVariant RetVal, pvJsonParse(uCtx)
        If LenB(.Error) Then
            Error = .Error
            GoTo QH
        End If
        If pvJsonGetChar(uCtx) <> 0 Then
            Error = Printf(ERR_EXTRA_SYMBOL, ChrW$(.LastChar), .Pos)
            GoTo QH
        End If
    End With
    '--- success
    JsonParse = True
QH:
    Exit Function
EH:
    If PrintError(FUNC_NAME) = vbRetry Then
        Resume
    End If
    Resume Next
End Function

Private Function pvJsonParse(uCtx As JsonContext) As Variant
    Const FUNC_NAME     As String = "pvJsonParse"
    Dim lIdx            As Long
    Dim sKey            As String
    Dim sText           As String
    Dim vValue          As Variant
#If ImplCollection And ImplRichClient Then
    Dim oRetVal         As cCollection
#ElseIf ImplCollection Then
    Dim oRetVal          As VBA.Collection
#ElseIf ImplRichClient Then
    Dim oRetVal          As cSortedDictionary
#ElseIf ImplScripting Then
    Dim oRetVal          As Scripting.Dictionary
#Else
    Dim oRetVal          As Object
#End If
    
    On Error GoTo EH
    With uCtx
        Select Case pvJsonGetChar(uCtx)
        Case 34 '--- "
            pvJsonParse = pvJsonGetString(uCtx)
            If .LastChar = 0 Then
                GoTo QH
            End If
        Case 91 '--- [
            Set oRetVal = pvJsonCreateObject()
            Do
                AssignVariant vValue, pvJsonParse(uCtx)
                If LenB(.Error) <> 0 Then
                    If .LastChar = 93 Then '--- ]
                        If Not .StrictMode Then
                            Exit Do
                        End If
                        #If ImplCollection Then
                            lIdx = oRetVal.Count - 1
                        #Else
                            lIdx = oRetVal.Count
                        #End If
                        If lIdx = 0 Then
                            Exit Do
                        End If
                    End If
                    GoTo QH
                End If
                #If ImplCollection Then
                    oRetVal.Add vValue
                #Else
                    oRetVal.Add lIdx, vValue
                #End If
                Select Case pvJsonGetChar(uCtx)
                Case 44 '--- ,
                    lIdx = lIdx + 1
                Case 93 '--- ]
                    Exit Do
                Case Else
                    .Error = Printf(ERR_EXPECTED_TWO, ",", "]", .Pos)
                    Exit Function
                End Select
            Loop
            .Error = vbNullString
            Set pvJsonParse = oRetVal
        Case 123 '--- {
            Set oRetVal = pvJsonCreateObject(TEXT_COMPARE)
            Do
                If pvJsonGetChar(uCtx) <> 34 Then '--- "
                    If .LastChar = 125 Then '--- }
                        If Not .StrictMode Then
                            Exit Do
                        End If
                        #If ImplCollection Then
                            lIdx = oRetVal.Count - 1
                        #Else
                            lIdx = oRetVal.Count
                        #End If
                        If lIdx = 0 Then
                            Exit Do
                        End If
                    End If
                    .Error = Printf(ERR_MISSING_KEY, .Pos)
                    GoTo QH
                End If
                sKey = pvJsonGetString(uCtx)
                If .LastChar = 0 Then
                    GoTo QH
                End If
                If pvJsonGetChar(uCtx) <> 58 Then '--- :
                    .Error = Printf(ERR_EXPECTED_SYMBOL, ":", .Pos)
                    GoTo QH
                End If
                AssignVariant vValue, pvJsonParse(uCtx)
                If LenB(.Error) <> 0 Then
                    GoTo QH
                End If
                Select Case pvJsonGetChar(uCtx)
                Case 44, 125 '--- , }
                    #If ImplCollection Then
                        If RemoveCollection(oRetVal, "#" & sKey) Then
                    #Else
                        If oRetVal.Exists(sKey) Then
                    #End If
                        .Error = Printf(ERR_DUPLICATE_KEY, sKey, .Pos)
                        GoTo QH
                    End If
                    #If ImplCollection Then
                        oRetVal.Add vValue, "#" & sKey
                        oRetVal.Item(STR_COL_KEYS).Add sKey, "#" & sKey
                    #Else
                        oRetVal.Add sKey, vValue
                    #End If
                    If .LastChar = 125 Then '--- }
                        Exit Do
                    End If
                Case Else
                    .Error = Printf(ERR_EXPECTED_TWO, ",", "}", .Pos)
                    GoTo QH
                End Select
            Loop
            .Error = vbNullString
            Set pvJsonParse = oRetVal
        Case 116, 84  '--- "t", "T"
            If Not ((.Text(.Pos + 0) Or &H20) = 114 And (.Text(.Pos + 1) Or &H20) = 117 And (.Text(.Pos + 2) Or &H20) = 101) Then
                GoTo UnexpectedSymbol
            End If
            .Pos = .Pos + 3
            pvJsonParse = True
        Case 102, 70 '--- "f", "F"
            If Not ((.Text(.Pos + 0) Or &H20) = 97 And (.Text(.Pos + 1) Or &H20) = 108 And (.Text(.Pos + 2) Or &H20) = 115 And (.Text(.Pos + 3) Or &H20) = 101) Then
                GoTo UnexpectedSymbol
            End If
            .Pos = .Pos + 4
            pvJsonParse = False
        Case 110, 78 '--- "n", "N"
            If Not ((.Text(.Pos + 0) Or &H20) = 117 And (.Text(.Pos + 1) Or &H20) = 108 And (.Text(.Pos + 2) Or &H20) = 108) Then
                GoTo UnexpectedSymbol
            End If
            .Pos = .Pos + 3
            pvJsonParse = Null
        Case 48 To 57, 43, 45, 46 '--- 0-9 + - .
            For lIdx = 0 To 1000
                Select Case .Text(.Pos + lIdx)
                Case 48 To 57, 43, 45, 46, 101, 69, 120, 88, 97 To 102, 65 To 70 '--- 0-9 + - . e E x X a-f A-F
                Case Else
                    Exit For
                End Select
            Next
            sText = Space$(lIdx + 1)
            Call CopyMemory(ByVal StrPtr(sText), .Text(.Pos - 1), LenB(sText))
            If LCase$(Left$(sText, 2)) = "0x" Then
                Mid$(sText, 1, 2) = "&H"
            End If
            On Error GoTo ErrorConvert
            pvJsonParse = CDbl(sText)
            On Error GoTo 0
            .Pos = .Pos + lIdx
        Case 0
            If LenB(.Error) <> 0 Then
                GoTo QH
            End If
        Case Else
            GoTo UnexpectedSymbol
        End Select
QH:
        Exit Function
UnexpectedSymbol:
        .Error = Printf(ERR_UNEXPECTED_SYMBOL, ChrW$(.LastChar), .Pos)
        Exit Function
ErrorConvert:
        .Error = Printf(ERR_CONVERSION, Err.Description, .Pos)
    End With
    Exit Function
EH:
    If RaiseError(FUNC_NAME) = vbRetry Then
        Resume
    End If
End Function

Private Function pvJsonGetChar(uCtx As JsonContext) As Integer
    Const FUNC_NAME     As String = "pvJsonGetChar"
    Dim lIdx            As Long
    
    On Error GoTo EH
    With uCtx
        Do While .Pos <= UBound(.Text)
            .LastChar = .Text(.Pos)
            .Pos = .Pos + 1
            Select Case .LastChar
            Case 0
                Exit Function
            Case 9, 10, 13, 32 '--- vbTab, vbCr, vbLf, " "
                '--- do nothing
            Case 47 '--- /
                If Not .StrictMode Then
                    Select Case .Text(.Pos)
                    Case 47 '--- //
                        .Pos = .Pos + 1
                        Do
                            .LastChar = .Text(.Pos)
                            .Pos = .Pos + 1
                            If .LastChar = 0 Then
                                Exit Function
                            End If
                        Loop While Not (.LastChar = 10 Or .LastChar = 13)  '--- vbLf or vbCr
                    Case 42 '--- /*
                        lIdx = .Pos + 1
                        Do
                            .LastChar = .Text(lIdx)
                            lIdx = lIdx + 1
                            If .LastChar = 0 Then
                                .Error = Printf(ERR_UNTERMIN_COMMENT, .Pos)
                                Exit Function
                            End If
                        Loop While Not (.LastChar = 42 And .Text(lIdx) = 47) '--- */
                        .LastChar = .Text(lIdx)
                        .Pos = lIdx + 1
                    Case Else
                        pvJsonGetChar = .LastChar
                        Exit Do
                    End Select
                Else
                    pvJsonGetChar = .LastChar
                    Exit Do
                End If
            Case Else
                pvJsonGetChar = .LastChar
                Exit Do
            End Select
        Loop
    End With
    Exit Function
EH:
    If RaiseError(FUNC_NAME) = vbRetry Then
        Resume
    End If
End Function

Private Function pvJsonGetString(uCtx As JsonContext) As String
    Const FUNC_NAME     As String = "pvJsonGetString"
    Dim lIdx            As Long
    Dim nChar           As Integer
    Dim sText           As String
    
    On Error GoTo EH
    With uCtx
        For lIdx = 0 To &H7FFFFFFF
            nChar = .Text(.Pos + lIdx)
            Select Case nChar
            Case 0, 34, 92 '--- " \
                sText = Space$(lIdx)
                Call CopyMemory(ByVal StrPtr(sText), .Text(.Pos), LenB(sText))
                pvJsonGetString = pvJsonGetString & sText
                If nChar = 34 Then '--- "
                    .Pos = .Pos + lIdx + 1
                    Exit For
                ElseIf nChar <> 92 Then '--- \
                    nChar = 0
                    .Pos = .Pos + lIdx + 1
                    .Error = Printf(ERR_MISSING_EOS, .Pos)
                    Exit For
                End If
                lIdx = lIdx + 1
                nChar = .Text(.Pos + lIdx)
                Select Case nChar
                Case 98  '--- b
                    pvJsonGetString = pvJsonGetString & ChrW$(8)
                Case 102 '--- f
                    pvJsonGetString = pvJsonGetString & ChrW$(12)
                Case 110 '--- n
                    pvJsonGetString = pvJsonGetString & vbLf
                Case 114 '--- r
                    pvJsonGetString = pvJsonGetString & vbCr
                Case 116 '--- t
                    pvJsonGetString = pvJsonGetString & vbTab
                Case 34  '--- "
                    pvJsonGetString = pvJsonGetString & """"
                Case 92  '--- \
                    pvJsonGetString = pvJsonGetString & "\"
                Case 47  '--- /
                    pvJsonGetString = pvJsonGetString & "/"
                Case 117 '--- u
                    pvJsonGetString = pvJsonGetString & ChrW$(CLng("&H" & ChrW$(.Text(.Pos + lIdx + 1)) & ChrW$(.Text(.Pos + lIdx + 2)) & ChrW$(.Text(.Pos + lIdx + 3)) & ChrW$(.Text(.Pos + lIdx + 4))))
                    lIdx = lIdx + 4
                Case 120 '--- x
                    pvJsonGetString = pvJsonGetString & ChrW$(CLng("&H" & ChrW$(.Text(.Pos + lIdx + 1)) & ChrW$(.Text(.Pos + lIdx + 2))))
                    lIdx = lIdx + 2
                Case Else
                    nChar = 0
                    .Pos = .Pos + lIdx + 1
                    .Error = Printf(ERR_INVALID_ESCAPE, .Pos)
                    Exit For
                End Select
                .Pos = .Pos + lIdx + 1
                lIdx = -1
            End Select
        Next
        .LastChar = nChar
    End With
    Exit Function
EH:
    If RaiseError(FUNC_NAME) = vbRetry Then
        Resume
    End If
End Function

Public Function JsonDump(vJson As Variant, Optional ByVal Level As Long, Optional ByVal Minimize As Boolean) As String
    Const STR_CODES     As String = "\u0000|\u0001|\u0002|\u0003|\u0004|\u0005|\u0006|\u0007|\b|\t|\n|\u000B|\f|\r|\u000E|\u000F|\u0010|\u0011|\u0012|\u0013|\u0014|\u0015|\u0016|\u0017|\u0018|\u0019|\u001A|\u001B|\u001C|\u001D|\u001E|\u001F"
    Const LNG_INDENT    As Long = 4
    Static vTranscode   As Variant
    Dim vKeys           As Variant
    Dim vItems          As Variant
    Dim lIdx            As Long
    Dim lSize           As Long
    Dim sCompound       As String
    Dim sSpace          As String
    Dim lAsc            As Long
    Dim lCompareMode    As Long
    Dim lCount          As Long
#If ImplCollection And ImplRichClient Then
    Dim oJson           As cCollection
#ElseIf ImplCollection Then
    Dim oJson           As VBA.Collection
#ElseIf ImplRichClient Then
    Dim oJson           As cSortedDictionary
#ElseIf ImplScripting Then
    Dim oJson           As Scripting.Dictionary
#Else
    Dim oJson           As Object
#End If
    
    '--- note: skip error handling not to clear Err because used in error handlers
'    On Error GoTo EH
    Select Case VarType(vJson)
    Case vbObject
        Set oJson = vJson
        If oJson Is Nothing Then
            Exit Function
        End If
        lCompareMode = pvJsonCompareMode(oJson)
        sCompound = IIf(lCompareMode = TEXT_COMPARE, "{}", "[]")
        #If ImplCollection Then
            lCount = oJson.Count - 1
        #Else
            lCount = oJson.Count
        #End If
        If lCount <= 0 Then
            JsonDump = sCompound
        Else
            sSpace = IIf(Minimize, vbNullString, " ")
            ReDim vItems(0 To lCount - 1) As String
            If lCompareMode = TEXT_COMPARE Then
                #If ImplCollection Then
                    Set vKeys = oJson.Item(STR_COL_KEYS)
                #ElseIf Not ImplRichClient Then
                    vKeys = oJson.Keys
                #End If
            Else
                #If Not ImplCollection And Not ImplRichClient Then
                    vKeys = oJson.Keys
                #End If
            End If
            For lIdx = 0 To lCount - 1
                #If ImplCollection Then
                    vItems(lIdx) = JsonDump(oJson.Item(lIdx + 2), Level + 1, Minimize)
                #ElseIf ImplRichClient Then
                    vItems(lIdx) = JsonDump(oJson.ItemByIndex(lIdx), Level + 1, Minimize)
                #Else
                    vItems(lIdx) = JsonDump(oJson.Item(vKeys(lIdx)), Level + 1, Minimize)
                #End If
                If lCompareMode = TEXT_COMPARE Then
                    #If ImplCollection Then
                        vItems(lIdx) = JsonDump(vKeys.Item(lIdx + 1)) & ":" & sSpace & vItems(lIdx)
                    #ElseIf ImplRichClient Then
                        vItems(lIdx) = JsonDump(oJson.KeyByIndex(lIdx)) & ":" & sSpace & vItems(lIdx)
                    #Else
                        vItems(lIdx) = JsonDump(vKeys(lIdx)) & ":" & sSpace & vItems(lIdx)
                    #End If
                End If
                lSize = lSize + Len(vItems(lIdx))
            Next
            If lSize > 100 And Not Minimize Then
                JsonDump = Left$(sCompound, 1) & vbCrLf & _
                    Space$((Level + 1) * LNG_INDENT) & Join(vItems, "," & vbCrLf & Space$((Level + 1) * LNG_INDENT)) & vbCrLf & _
                    Space$(Level * LNG_INDENT) & Right$(sCompound, 1)
            Else
                JsonDump = Left$(sCompound, 1) & sSpace & Join(vItems, "," & sSpace) & sSpace & Right$(sCompound, 1)
            End If
        End If
    Case vbNull
        JsonDump = "Null"
    Case vbEmpty
        JsonDump = "Empty"
    Case vbString
        '--- one-time initialization of transcoding array
        If IsEmpty(vTranscode) Then
            vTranscode = Split(STR_CODES, "|")
        End If
        For lIdx = 1 To Len(vJson)
            lAsc = AscW(Mid$(vJson, lIdx, 1))
            If lAsc = 92 Or lAsc = 34 Then '--- \ and "
                JsonDump = JsonDump & "\" & ChrW$(lAsc)
            ElseIf lAsc >= 32 And lAsc < 256 Then
                JsonDump = JsonDump & ChrW$(lAsc)
            ElseIf lAsc >= 0 And lAsc < 32 Then
                JsonDump = JsonDump & vTranscode(lAsc)
            ElseIf Asc(Mid$(vJson, lIdx, 1)) <> 63 Or Mid$(vJson, lIdx, 1) = "?" Then '--- ?
                JsonDump = JsonDump & ChrW$(AscW(Mid$(vJson, lIdx, 1)))
            Else
                JsonDump = JsonDump & "\u" & Right$("0000" & Hex$(lAsc), 4)
            End If
        Next
        JsonDump = """" & JsonDump & """"
    Case Else
        If IsArray(vJson) Then
            JsonDump = Join(vJson)
        Else
            JsonDump = vJson & ""
        End If
    End Select
End Function

Public Property Get JsonItem(oJson As Object, ByVal sKey As String) As Variant
    Const FUNC_NAME     As String = "JsonItem [get]"
    Dim vSplit          As Variant
    Dim lIdx            As Long
    Dim lJdx            As Long
    Dim vKey            As Variant
    Dim lKey            As Long
    Dim vItem           As Variant
#If ImplCollection And ImplRichClient Then
    Dim oParam          As cCollection
#ElseIf ImplCollection Then
    Dim oParam          As VBA.Collection
#ElseIf ImplRichClient Then
    Dim oParam          As cSortedDictionary
#ElseIf ImplScripting Then
    Dim oParam          As Scripting.Dictionary
#Else
    Dim oParam          As Object
#End If
    
    On Error GoTo EH
    If oJson Is Nothing Then
        GoTo ReturnEmpty
    End If
    vSplit = Split(sKey, "/")
    Set oParam = oJson
    For lIdx = 0 To UBound(vSplit)
        vKey = vSplit(lIdx)
        If C_Str(vKey) = "-1" Then
            #If ImplCollection Then
                JsonItem = oParam.Count - 1
            #Else
                JsonItem = oParam.Count
            #End If
            GoTo QH
        ElseIf IsOnlyDigits(vKey) Then
            vKey = C_Lng(vKey)
        End If
        AssignVariant vItem, pvJsonItem(oParam, vKey)
        If Not IsEmpty(vItem) Then
            If lIdx < UBound(vSplit) Then
                If Not IsObject(vItem) Then
                    GoTo ReturnEmpty
                End If
                Set oParam = vItem
            Else
                AssignVariant JsonItem, vItem
            End If
        ElseIf C_Str(vKey) = "0" Then
            '--- do nothing & continue
        Else
            If LenB(vKey) = 0 Then
                Set JsonItem = oParam
            ElseIf C_Str(vKey) = "*" Then
                vKey = vbNullString
                For lJdx = lIdx + 1 To UBound(vSplit)
                    vKey = vKey & "/" & vSplit(lJdx)
                Next
                #If ImplCollection Then
                    ReDim vItem(0 To oParam.Count - 2) As Variant
                #Else
                    ReDim vItem(0 To oParam.Count - 1) As Variant
                #End If
                lJdx = 0
                For lKey = 0 To UBound(vItem)
                    AssignVariant vItem(lJdx), JsonItem(oParam, lKey & vKey)
                    lJdx = lJdx + 1
                Next
                If lJdx = 0 Then
                    JsonItem = Array()
                Else
                    If lJdx - 1 <> UBound(vItem) Then
                        ReDim Preserve vItem(0 To lJdx - 1) As Variant
                    End If
                    JsonItem = vItem
                End If
            Else
ReturnEmpty:
                If Right$(sKey, 1) = "/" Then
                    Set JsonItem = pvJsonCreateObject(TEXT_COMPARE)
                ElseIf Right$(sKey, 3) = "/-1" Then
                    JsonItem = 0&
                ElseIf InStr(sKey, "*") > 0 Then
                    JsonItem = Array()
                End If
            End If
            GoTo QH
        End If
    Next
QH:
    Exit Property
EH:
    If RaiseError(FUNC_NAME) = vbRetry Then
        Resume
    End If
End Property

Public Property Let JsonItem(oJson As Object, ByVal sKey As String, vValue As Variant)
    Const FUNC_NAME     As String = "JsonItem [let]"
    Dim vSplit          As Variant
    Dim lIdx            As Long
    Dim lJdx            As Long
    Dim vKey            As Variant
    Dim lKey            As Long
    Dim vItem           As Variant
#If ImplCollection And ImplRichClient Then
    Dim oParam          As cCollection
#ElseIf ImplCollection Then
    Dim oParam          As VBA.Collection
#ElseIf ImplRichClient Then
    Dim oParam          As cSortedDictionary
#ElseIf ImplScripting Then
    Dim oParam          As Scripting.Dictionary
#Else
    Dim oParam          As Object
#End If

    On Error GoTo EH
    vSplit = Split(sKey, "/")
    If oJson Is Nothing Then
        If UBound(vSplit) < 0 Then
            Set oJson = pvJsonCreateObject(TEXT_COMPARE)
        Else
            Set oJson = pvJsonCreateObject(-(Not IsOnlyDigits(vSplit(0)) And vSplit(0) <> "*" And vSplit(0) <> "-1"))
        End If
    End If
    Set oParam = oJson
    For lIdx = 0 To UBound(vSplit)
        vKey = vSplit(lIdx)
        If C_Str(vKey) = "-1" Then
            #If ImplCollection Then
                vKey = oParam.Count - 1
            #Else
                vKey = oParam.Count
            #End If
        ElseIf IsOnlyDigits(vKey) Then
            vKey = C_Lng(vKey)
        End If
        If C_Str(vKey) = "*" Then
HandleArray:
            vKey = vbNullString
            For lJdx = lIdx + 1 To UBound(vSplit)
                vKey = vKey & "/" & vSplit(lJdx)
            Next
            lKey = 0
            If IsEmpty(vValue) Then
                For Each vItem In JsonKeys(oParam)
                    JsonItem(oParam, lKey & vKey) = Empty
                    lKey = lKey + 1
                Next
            Else
                For Each vItem In vValue
                    JsonItem(oParam, lKey & vKey) = vItem
                    lKey = lKey + 1
                Next
            End If
            Exit For
        ElseIf lIdx < UBound(vSplit) Then
            If Not IsObject(pvJsonItem(oParam, vKey)) Then
                pvJsonItem(oParam, vKey) = pvJsonCreateObject(-(Not IsOnlyDigits(vSplit(lIdx + 1)) And vSplit(lIdx + 1) <> "*" And vSplit(lIdx + 1) <> "-1"))
            End If
            Set oParam = pvJsonItem(oParam, vKey)
        ElseIf IsEmpty(vValue) Then
            #If ImplCollection Then
                If VarType(vKey) = vbLong Then
                    RemoveCollection oParam, vKey + 2
                Else
                    RemoveCollection oParam, "#" & vKey
                    RemoveCollection oParam.Item(STR_COL_KEYS), "#" & vKey
                End If
            #Else
                If oParam.Exists(vKey) Then
                    oParam.Remove vKey
                End If
            #End If
        ElseIf IsArray(vValue) Then
            pvJsonItem(oParam, vKey) = pvJsonCreateObject()
            Set oParam = pvJsonItem(oParam, vKey)
            GoTo HandleArray
        Else
            pvJsonItem(oParam, vKey) = vValue
        End If
    Next
    Exit Property
EH:
    If RaiseError(FUNC_NAME) = vbRetry Then
        Resume
    End If
End Property

Public Function JsonKeys(oJson As Object, Optional ByVal sKey As String) As Variant
    Const FUNC_NAME     As String = "JsonKeys [get]"
    Dim vSplit          As Variant
    Dim lIdx            As Long
    Dim vKey            As Variant
    Dim vItem           As Variant
    Dim lCount          As Long
#If ImplCollection And ImplRichClient Then
    Dim oParam          As cCollection
#ElseIf ImplCollection Then
    Dim oParam          As VBA.Collection
#ElseIf ImplRichClient Then
    Dim oParam          As cSortedDictionary
#ElseIf ImplScripting Then
    Dim oParam          As Scripting.Dictionary
#Else
    Dim oParam          As Object
#End If
    
    On Error GoTo EH
    If oJson Is Nothing Then
        JsonKeys = Array()
        Exit Function
    End If
    vSplit = Split(sKey, "/")
    Set oParam = oJson
    For lIdx = 0 To UBound(vSplit)
        vKey = vSplit(lIdx)
        If IsOnlyDigits(vKey) Then
            vKey = C_Lng(vKey)
        End If
        AssignVariant vItem, pvJsonItem(oParam, vKey)
        If IsObject(vItem) Then
            Set oParam = vItem
        Else
            JsonKeys = Array()
            Exit Function
        End If
    Next
    #If ImplCollection Then
        lCount = oParam.Count - 1
    #Else
        lCount = oParam.Count
    #End If
    If lCount = 0 Then
        JsonKeys = Array()
        Exit Function
    End If
    ReDim vItem(0 To lCount - 1) As Variant
    If pvJsonCompareMode(oParam) = TEXT_COMPARE Then
        #If ImplCollection Then
            lIdx = 0
            For Each vKey In oParam.Item(STR_COL_KEYS)
                vItem(lIdx) = vKey
                lIdx = lIdx + 1
            Next
        #ElseIf ImplRichClient Then
            For lIdx = 0 To lCount - 1
                vItem(lIdx) = oParam.KeyByIndex(lIdx)
            Next
        #Else
            vItem = oParam.Keys
        #End If
    Else
        For lIdx = 0 To UBound(vItem)
            vItem(lIdx) = lIdx
        Next
    End If
    JsonKeys = vItem
    Exit Function
EH:
    If RaiseError(FUNC_NAME) = vbRetry Then
        Resume
    End If
End Function

Public Function JsonObjectType(oJson As Object, Optional ByVal sKey As String) As Long
    Const FUNC_NAME     As String = "JsonObjectType [get]"
    Dim vSplit          As Variant
    Dim lIdx            As Long
    Dim vKey            As Variant
    Dim vItem           As Variant
#If ImplCollection And ImplRichClient Then
    Dim oParam          As cCollection
#ElseIf ImplCollection Then
    Dim oParam          As VBA.Collection
#ElseIf ImplRichClient Then
    Dim oParam          As cSortedDictionary
#ElseIf ImplScripting Then
    Dim oParam          As Scripting.Dictionary
#Else
    Dim oParam          As Object
#End If
    
    On Error GoTo EH
    If oJson Is Nothing Then
        JsonObjectType = -1
        Exit Function
    End If
    vSplit = Split(sKey, "/")
    Set oParam = oJson
    For lIdx = 0 To UBound(vSplit)
        vKey = vSplit(lIdx)
        If IsOnlyDigits(vKey) Then
            vKey = C_Lng(vKey)
        End If
        AssignVariant vItem, pvJsonItem(oParam, vKey)
        If IsObject(vItem) Then
            Set oParam = vItem
        Else
            JsonObjectType = -1
            Exit Function
        End If
    Next
    JsonObjectType = pvJsonCompareMode(oParam)
    Exit Function
EH:
    If RaiseError(FUNC_NAME) = vbRetry Then
        Resume
    End If
End Function

Public Function JsonToXmlDocument(vJson As Variant, Optional oRoot As Object, Optional oDoc As Object) As Object
    Const FUNC_NAME     As String = "JsonToXmlDocument"
    Dim vElem           As Variant
    Dim vItem           As Variant
    Dim oArray          As Object
    Dim oItem           As Object
#If ImplCollection And ImplRichClient Then
    Dim oJson           As cCollection
#ElseIf ImplCollection Then
    Dim oJson           As VBA.Collection
#ElseIf ImplRichClient Then
    Dim oJson           As cSortedDictionary
#ElseIf ImplScripting Then
    Dim oJson           As Scripting.Dictionary
#Else
    Dim oJson           As Object
#End If
    Dim lCount          As Long
    Dim lIdx            As Long
    
    On Error GoTo EH
    If oDoc Is Nothing Then
        Set oDoc = CreateObject("MSXML2.DOMDocument")
        oDoc.appendChild oDoc.createProcessingInstruction("xml", "version=""1.0"" encoding=""UTF-16""")
    End If
    If oRoot Is Nothing Then
        Set oRoot = oDoc.appendChild(oDoc.createElement("Root"))
    End If
    If IsObject(vJson) Then
        Set oJson = vJson
        If Not oJson Is Nothing Then
            #If ImplCollection Then
                lCount = oJson.Count - 1
            #Else
                lCount = oJson.Count
            #End If
            If lCount = 0 Then
                If Not oRoot Is oDoc.documentElement Then
                    oRoot.setAttribute STR_ATTR_EMPTY & pvJsonCompareMode(oJson), 1
                End If
                If pvJsonCompareMode(oJson) <> TEXT_COMPARE Then
                    oRoot.setAttribute STR_ATTR_ARRAY, 1
                End If
            Else
                If pvJsonCompareMode(oJson) <> TEXT_COMPARE Then
                    Set oArray = oRoot
                    If oArray Is oDoc.documentElement Then
                        Set oArray = oArray.appendChild(oDoc.createElement(STR_NODE_ARRAY))
                    ElseIf lCount = 1 Then
                        oArray.setAttribute STR_ATTR_ARRAY, 1
                    End If
                    Set oItem = oArray
                    For lIdx = 0 To lCount - 1
                        #If ImplCollection Then
                            AssignVariant vItem, oJson.Item(lIdx + 2)
                        #ElseIf ImplRichClient Then
                            AssignVariant vItem, oJson.ItemByIndex(lIdx)
                        #Else
                            AssignVariant vItem, oJson.Item(lIdx)
                        #End If
                        If oItem Is Nothing Then
                            Set oItem = oArray.ParentNode.appendChild(oDoc.createElement(oArray.nodeName))
                        End If
                        JsonToXmlDocument vItem, oItem, oDoc
                        Set oItem = Nothing
                    Next
                Else
                    #If ImplCollection Then
                        For Each vElem In oJson.Item(STR_COL_KEYS)
                    #ElseIf ImplRichClient Then
                        For lIdx = 0 To lCount - 1
                            vElem = oJson.KeyByIndex(lIdx)
                    #Else
                        For Each vElem In oJson.Keys
                    #End If
                        AssignVariant vItem, pvJsonItem(oJson, vElem)
                        If Left$(vElem, 1) = "@" Then
                            If IsObject(vItem) Then
                                oRoot.setAttribute Mid$(vElem, 2), JsonDump(vItem, Minimize:=True)
                            Else
                                oRoot.setAttribute Mid$(vElem, 2), vItem
                            End If
                        Else
                            If IsOnlyDigits(vElem) Or LenB(vElem) = 0 Then
                                Set oItem = oRoot.appendChild(oDoc.createElement(STR_PREFIX & oRoot.childNodes.Length))
                                oItem.setAttribute STR_ATTR_NAME, vElem
                            Else
                                Set oItem = oRoot.appendChild(oDoc.createElement(vElem))
                            End If
                            JsonToXmlDocument vItem, oItem, oDoc
                        End If
                    Next
                End If
            End If
        End If
    ElseIf IsEmpty(vJson) Then
        oRoot.setAttribute STR_ATTR_EMPTY, 1
    ElseIf IsNull(vJson) Then
        oRoot.setAttribute STR_ATTR_NIL, 1
    ElseIf VarType(vJson) = vbBoolean Then
        oRoot.setAttribute STR_ATTR_BOOL, 1
        oRoot.nodeTypedValue = -vJson
    ElseIf VarType(vJson) = vbDate Then
        oRoot.DataType = "dateTime.tz"
        oRoot.nodeTypedValue = vJson
    ElseIf IsArray(vJson) Then
        oRoot.nodeTypedValue = Join(vJson)
    Else
        oRoot.nodeTypedValue = vJson
    End If
    Set JsonToXmlDocument = oRoot
    Exit Function
EH:
    If RaiseError(FUNC_NAME) = vbRetry Then
        Resume
    End If
End Function

Public Function JsonFromXmlDocument(vXml As Variant) As Variant
    Const FUNC_NAME     As String = "JsonFromXmlDocument"
    Dim oRoot           As Object
    Dim oNode           As Object
    Dim sKey            As String
    Dim bHasAttributes  As Boolean
    Dim vItem           As Variant
#If ImplCollection And ImplRichClient Then
    Dim oDict           As cCollection
    Dim oArray          As cCollection
#ElseIf ImplCollection Then
    Dim oDict           As VBA.Collection
    Dim oArray          As VBA.Collection
#ElseIf ImplRichClient Then
    Dim oDict           As cSortedDictionary
    Dim oArray          As cSortedDictionary
#ElseIf ImplScripting Then
    Dim oDict           As Scripting.Dictionary
    Dim oArray          As Scripting.Dictionary
#Else
    Dim oDict           As Object
    Dim oArray          As Object
#End If

    On Error GoTo EH
    If IsObject(vXml) Then
        Set oRoot = vXml
    Else
        With CreateObject("MSXML2.DOMDocument")
            .LoadXml C_Str(vXml)
            Set oRoot = .documentElement
        End With
        If oRoot Is Nothing Then
            Exit Function
        End If
    End If
    If Not oRoot.firstChild Is Nothing Then
        bHasAttributes = Not oRoot.firstChild.Attributes Is Nothing
    Else
        bHasAttributes = oRoot.Attributes.Length > 0 Or oRoot Is oRoot.ownerDocument.documentElement
    End If
    If C_Bool(oRoot.getAttribute(STR_ATTR_EMPTY & TEXT_COMPARE)) Then
        Set JsonFromXmlDocument = pvJsonCreateObject(TEXT_COMPARE)
    ElseIf C_Bool(oRoot.getAttribute(STR_ATTR_EMPTY)) Then
        JsonFromXmlDocument = Empty
    ElseIf C_Bool(oRoot.getAttribute(STR_ATTR_NIL)) Then
        JsonFromXmlDocument = Null
    ElseIf C_Bool(oRoot.getAttribute(STR_ATTR_BOOL)) Then
        JsonFromXmlDocument = C_Bool(oRoot.Text)
    ElseIf bHasAttributes Then
        If oRoot.firstChild Is Nothing Then
            Set oDict = pvJsonCreateObject(-Not C_Bool(oRoot.getAttribute(STR_ATTR_ARRAY)))
        Else
            Set oDict = pvJsonCreateObject(TEXT_COMPARE)
        End If
        For Each oNode In oRoot.Attributes
            sKey = C_Str(oNode.nodeName)
            If Left$(sKey, Len(STR_PREFIX)) <> STR_PREFIX Then
                sKey = "@" & sKey
                If Left$(oNode.Text, 1) = "{" Or Left$(oNode.Text, 1) = "[" Then
                    If JsonParse(oNode.Text, vItem, StrictMode:=True) Then
                        pvJsonItem(oDict, sKey) = vItem
                    Else
                        pvJsonItem(oDict, sKey) = oNode.Text
                    End If
                ElseIf C_Str(C_Lng(oNode.Text)) = oNode.Text Then
                    pvJsonItem(oDict, sKey) = C_Lng(oNode.Text)
                ElseIf C_Str(C_Dbl(oNode.Text)) = oNode.Text Then
                    pvJsonItem(oDict, sKey) = C_Dbl(oNode.Text)
                Else
                    pvJsonItem(oDict, sKey) = oNode.nodeTypedValue
                End If
            End If
        Next
        sKey = vbNullString
        For Each oNode In oRoot.childNodes
            If Not IsNull(oNode.getAttribute(STR_ATTR_NAME)) Then
                sKey = C_Str(oNode.getAttribute(STR_ATTR_NAME))
            Else
                sKey = C_Str(oNode.nodeName)
            End If
            If Not IsEmpty(pvJsonItem(oDict, sKey)) Or sKey = STR_NODE_ARRAY Or C_Bool(oNode.getAttribute(STR_ATTR_ARRAY)) Then
                AssignVariant vItem, pvJsonItem(oDict, sKey)
                If IsEmpty(vItem) Then
                    Set oArray = pvJsonCreateObject()
                    pvJsonItem(oDict, sKey) = oArray
                ElseIf Not IsObject(vItem) Then
CreateArray:
                    Set oArray = pvJsonCreateObject()
                    #If ImplCollection Then
                        oArray.Add vItem
                    #Else
                        oArray.Add 0&, vItem
                    #End If
                    pvJsonItem(oDict, sKey) = oArray
                ElseIf pvJsonCompareMode(C_Obj(vItem)) = TEXT_COMPARE Then
                    GoTo CreateArray
                Else
                    Set oArray = C_Obj(vItem)
                End If
                If Not C_Bool(oNode.getAttribute(STR_ATTR_EMPTY & 0)) Then
                    #If ImplCollection Then
                        oArray.Add JsonFromXmlDocument(oNode)
                    #Else
                        oArray.Add oArray.Count, JsonFromXmlDocument(oNode)
                    #End If
                End If
            Else
                pvJsonItem(oDict, sKey) = JsonFromXmlDocument(oNode)
            End If
        Next
        If sKey = STR_NODE_ARRAY Then
            Set JsonFromXmlDocument = pvJsonItem(oDict, sKey)
        Else
            Set JsonFromXmlDocument = oDict
        End If
    ElseIf C_Str(C_Lng(oRoot.Text)) = oRoot.Text Then
        JsonFromXmlDocument = C_Lng(oRoot.Text)
    ElseIf C_Str(C_Dbl(oRoot.Text)) = oRoot.Text Then
        JsonFromXmlDocument = C_Dbl(oRoot.Text)
    ElseIf oRoot.Text Like "####-##-##T##:##:##*" Then
        vItem = Split(Replace(Replace(Replace(Replace(oRoot.Text, "T", "-"), ":", "-"), ".", "-"), "+", "-"), "-")
        JsonFromXmlDocument = DateSerial(C_Lng(vItem(0)), C_Lng(vItem(1)), C_Lng(vItem(2))) + TimeSerial(C_Lng(vItem(3)), C_Lng(vItem(4)), Val(vItem(5)))
    Else
        JsonFromXmlDocument = oRoot.nodeTypedValue
    End If
    Exit Function
EH:
    If RaiseError(FUNC_NAME) = vbRetry Then
        Resume
    End If
End Function

#If ImplCollection And ImplRichClient Then
    Private Function pvJsonCreateObject(Optional ByVal lCompareMode As Long) As cCollection
        Set pvJsonCreateObject = New cCollection
        If lCompareMode = 0 Then
            pvJsonCreateObject.Add Empty, STR_COL_KEYS
        Else
            pvJsonCreateObject.Add New VBA.Collection, STR_COL_KEYS
        End If
    End Function
    
    Private Function pvJsonCompareMode(oJson As cCollection) As Long
        pvJsonCompareMode = -IsObject(oJson.Item(STR_COL_KEYS))
    End Function
#ElseIf ImplCollection Then
    Private Function pvJsonCreateObject(Optional ByVal lCompareMode As Long) As VBA.Collection
        Set pvJsonCreateObject = New VBA.Collection
        If lCompareMode = 0 Then
            pvJsonCreateObject.Add Empty, STR_COL_KEYS
        Else
            pvJsonCreateObject.Add New VBA.Collection, STR_COL_KEYS
        End If
    End Function
    
    Private Function pvJsonCompareMode(oJson As VBA.Collection) As Long
        pvJsonCompareMode = -IsObject(oJson.Item(STR_COL_KEYS))
    End Function
#ElseIf ImplRichClient Then
    Private Function pvJsonCreateObject(Optional ByVal lCompareMode As Long) As cSortedDictionary
        Set pvJsonCreateObject = New cSortedDictionary
        pvJsonCreateObject.StringCompareMode = lCompareMode
    End Function
    
    Private Function pvJsonCompareMode(oJson As cSortedDictionary) As Long
        pvJsonCompareMode = oJson.StringCompareMode
    End Function
#ElseIf ImplScripting Then
    Private Function pvJsonCreateObject(Optional ByVal lCompareMode As Long) As Scripting.Dictionary
        Set pvJsonCreateObject = New Scripting.Dictionary
        pvJsonCreateObject.CompareMode = lCompareMode
    End Function
    
    Private Function pvJsonCompareMode(oJson As Scripting.Dictionary) As Long
        pvJsonCompareMode = oJson.CompareMode
    End Function
#Else
    Private Function pvJsonCreateObject(Optional ByVal lCompareMode As Long) As Object
        Set pvJsonCreateObject = CreateObject(PROGID_DICTIONARY)
        pvJsonCreateObject.CompareMode = lCompareMode
    End Function
    
    Private Function pvJsonCompareMode(oJson As Object) As Long
        pvJsonCompareMode = oJson.CompareMode
    End Function
#End If

#If ImplCollection And ImplRichClient Then
    Private Property Get pvJsonItem(oParam As cCollection, vKey As Variant) As Variant
#ElseIf ImplCollection Then
    Private Property Get pvJsonItem(oParam As VBA.Collection, vKey As Variant) As Variant
#ElseIf ImplRichClient Then
    Private Property Get pvJsonItem(oParam As cSortedDictionary, vKey As Variant) As Variant
#ElseIf ImplScripting Then
    Private Property Get pvJsonItem(oParam As Scripting.Dictionary, vKey As Variant) As Variant
#Else
    Private Property Get pvJsonItem(oParam As Object, vKey As Variant) As Variant
#End If
    #If ImplCollection And ImplUseShared Then
        If VarType(vKey) = vbLong Then
            SearchCollection oParam, vKey + 2, RetVal:=pvJsonItem
        Else
            SearchCollection oParam, "#" & vKey, RetVal:=pvJsonItem
        End If
    #ElseIf ImplCollection Then
        On Error GoTo QH
        If VarType(vKey) = vbLong Then
            AssignVariant pvJsonItem, oParam.Item(vKey + 2)
        Else
            AssignVariant pvJsonItem, oParam.Item("#" & vKey)
        End If
    #Else
        If oParam.Exists(vKey) Then
            AssignVariant pvJsonItem, oParam.Item(vKey)
        End If
    #End If
QH:
End Property

#If ImplCollection And ImplRichClient Then
    Private Property Let pvJsonItem(oParam As cCollection, vKey As Variant, vValue As Variant)
#ElseIf ImplCollection Then
    Private Property Let pvJsonItem(oParam As VBA.Collection, vKey As Variant, vValue As Variant)
#ElseIf ImplRichClient Then
    Private Property Let pvJsonItem(oParam As cSortedDictionary, vKey As Variant, vValue As Variant)
#ElseIf ImplScripting Then
    Private Property Let pvJsonItem(oParam As Scripting.Dictionary, vKey As Variant, vValue As Variant)
#Else
    Private Property Let pvJsonItem(oParam As Object, vKey As Variant, vValue As Variant)
#End If
    #If ImplCollection Then
        If VarType(vKey) = vbLong Then
            RemoveCollection oParam, vKey + 2
            If vKey + 2 < oParam.Count Then
                oParam.Add vValue, Before:=vKey + 2
            Else
                oParam.Add vValue
            End If
        Else
            RemoveCollection oParam, "#" & vKey
            oParam.Add vValue, "#" & vKey
            RemoveCollection oParam.Item(STR_COL_KEYS), "#" & vKey
            oParam.Item(STR_COL_KEYS).Add vKey, "#" & vKey
        End If
    #Else
        If VarType(vKey) = vbLong Then
            If IsObject(vValue) Then
                Set oParam.Item(vKey) = vValue
            Else
                oParam.Item(vKey) = vValue
            End If
        Else
            If oParam.Exists(vKey) Then
                oParam.Remove vKey
            End If
            oParam.Add vKey, vValue
        End If
    #End If
End Property

#If ImplUseShared Then
#Else

Private Function IsOnlyDigits(ByVal sText As String) As Boolean
    If LenB(sText) <> 0 Then
        IsOnlyDigits = Not (sText Like "*[!0-9]*")
    End If
End Function

Private Sub AssignVariant(vDest As Variant, vSrc As Variant)
    On Error GoTo QH
    If IsObject(vSrc) Then
        Set vDest = vSrc
    Else
        vDest = vSrc
    End If
QH:
End Sub

Private Function C_Str(Value As Variant) As String
    On Error GoTo QH
    C_Str = CStr(Value)
QH:
End Function

Private Function C_Bool(Value As Variant) As Boolean
    On Error GoTo QH
    C_Bool = CBool(Value)
QH:
End Function

Private Function C_Lng(Value As Variant) As Long
    On Error GoTo QH
    C_Lng = CLng(Value)
QH:
End Function

Private Function C_Dbl(Value As Variant) As Double
    On Error GoTo QH
    C_Dbl = CDbl(Value)
QH:
End Function

Private Function C_Obj(Value As Variant) As Object
    On Error GoTo QH
    Set C_Obj = Value
QH:
End Function

Private Function RemoveCollection(oCol As Object, vKey As Variant) As Boolean
    On Error GoTo QH
    oCol.Remove vKey
    RemoveCollection = True
QH:
End Function

Private Function Printf(ByVal sText As String, ParamArray A() As Variant) As String
    Const LNG_PRIVATE   As Long = &HE1B6 '-- U+E000 to U+F8FF - Private Use Area (PUA)
    Dim lIdx            As Long
    
    For lIdx = UBound(A) To LBound(A) Step -1
        sText = Replace(sText, "%" & (lIdx - LBound(A) + 1), Replace(A(lIdx), "%", ChrW$(LNG_PRIVATE)))
    Next
    Printf = Replace(sText, ChrW$(LNG_PRIVATE), "%")
End Function

#End If
