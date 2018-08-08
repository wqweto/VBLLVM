Attribute VB_Name = "mdParser"
' Auto-generated on 8.8.2018 16:10:00
Option Explicit
DefObj A-Z

'=========================================================================
' API
'=========================================================================

Private Const LOCALE_USER_DEFAULT           As Long = &H400
Private Const NORM_IGNORECASE               As Long = 1
Private Const CSTR_EQUAL                    As Long = 2

Private Declare Function CompareStringW Lib "kernel32" (ByVal Locale As Long, ByVal dwCmpFlags As Long, lpString1 As Any, ByVal cchCount1 As Long, lpString2 As Any, ByVal cchCount2 As Long) As Long
Private Declare Sub CopyMemory Lib "kernel32" Alias "RtlMoveMemory" (Destination As Any, Source As Any, ByVal Length As Long)

'=========================================================================
' Constants and member variables
'=========================================================================

Private Const LNG_MAXINT            As Long = 2 ^ 31 - 1

'= generated enum ========================================================

Private Enum UcsParserActionsEnum
    ucsAct_3_Stmt
    ucsAct_2_Stmt
    ucsAct_1_Stmt
    ucsAct_1_TypedefDecl
    ucsAct_1_TypedefCallback
    ucsAct_3_EnumDecl
    ucsAct_2_EnumDecl
    ucsAct_1_EnumDecl
    ucsAct_3_StructDecl
    ucsAct_2_StructDecl
    ucsAct_1_StructDecl
    ucsAct_1_FunDecl
    ucsAct_1_SkipStmt
    ucsAct_1_Type
    ucsAct_1_ID
    ucsAct_1_TypeUnlimited
    ucsAct_3_Params
    ucsAct_2_Params
    ucsAct_1_Params
    ucsAct_4_EnumValue
    ucsAct_3_EnumValue
    ucsAct_2_EnumValue
    ucsAct_1_EnumValue
    ucsAct_1_EMPTY
    ucsAct_1_Param
    ucsAct_1_ArraySuffix
    ucsAct_1_EnumValueToken
    ucsActVarAlloc = -1
    ucsActVarSet = -2
    ucsActResultClear = -3
    ucsActResultSet = -4
End Enum

Private Type UcsParserThunkType
    Action              As Long
    CaptureBegin        As Long
    CaptureEnd          As Long
End Type

Private Type UcsParserType
    Contents            As String
    BufData()           As Integer
    BufPos              As Long
    BufSize             As Long
    ThunkData()         As UcsParserThunkType
    ThunkPos            As Long
    CaptureBegin        As Long
    CaptureEnd          As Long
    LastExpected        As String
    LastError           As String
    LastBufPos          As Long
    UserData            As Variant
    VarResult           As Variant
    VarStack()          As Variant
    VarPos              As Long
End Type

Private ctx                     As UcsParserType

'=========================================================================
' Properties
'=========================================================================

Property Get VbPegLastError() As String
    VbPegLastError = ctx.LastError
End Property

Property Get VbPegLastOffset() As Long
    VbPegLastOffset = ctx.LastBufPos + 1
End Property

Property Get VbPegParserVersion() As String
    VbPegParserVersion = "8.8.2018 16:10:00"
End Property

Property Get VbPegContents(Optional ByVal lOffset As Long = 1, Optional ByVal lSize As Long = LNG_MAXINT) As String
    VbPegContents = Mid$(ctx.Contents, lOffset, lSize)
End Property

'=========================================================================
' Methods
'=========================================================================

Public Function VbPegMatch(sSubject As String, Optional ByVal StartPos As Long, Optional UserData As Variant, Optional Result As Variant) As Long
    If VbPegBeginMatch(sSubject, StartPos, UserData) Then
        If VbPegParseStmt() Then
            VbPegMatch = VbPegEndMatch(Result)
        Else
            With ctx
                If LenB(.LastError) = 0 Then
                    If LenB(.LastExpected) = 0 Then
                        .LastError = "Fail"
                    Else
                        .LastError = "Expected " & Join(Split(Mid$(.LastExpected, 2, Len(.LastExpected) - 2), vbNullChar), " or ")
                    End If
                End If
            End With
        End If
    End If
End Function

Public Function VbPegBeginMatch(sSubject As String, Optional ByVal StartPos As Long, Optional UserData As Variant) As Boolean
    With ctx
        .LastBufPos = 0
        If LenB(sSubject) = 0 Then
            .LastError = "Cannot match empty input"
            Exit Function
        End If
        .Contents = sSubject
        ReDim .BufData(0 To Len(sSubject) + 3) As Integer
        Call CopyMemory(.BufData(0), ByVal StrPtr(sSubject), LenB(sSubject))
        .BufPos = StartPos
        .BufSize = Len(sSubject)
        .BufData(.BufSize) = -1 '-- EOF anchor
        ReDim .ThunkData(0 To 4) As UcsParserThunkType
        .ThunkPos = 0
        .CaptureBegin = 0
        .CaptureEnd = 0
        If IsObject(UserData) Then
            Set .UserData = UserData
        Else
            .UserData = UserData
        End If
    End With
    VbPegBeginMatch = True
End Function

Public Function VbPegEndMatch(Optional Result As Variant) As Long
    Dim lIdx            As Long

    With ctx
        ReDim .VarStack(0 To 1024) As Variant
        For lIdx = 0 To .ThunkPos - 1
            Select Case .ThunkData(lIdx).Action
            Case ucsActVarAlloc
                .VarPos = .VarPos + .ThunkData(lIdx).CaptureBegin
            Case ucsActVarSet
                If IsObject(.VarResult) Then
                    Set .VarStack(.VarPos - .ThunkData(lIdx).CaptureBegin) = .VarResult
                Else
                    .VarStack(.VarPos - .ThunkData(lIdx).CaptureBegin) = .VarResult
                End If
            Case ucsActResultClear
                .VarResult = Empty
            Case ucsActResultSet
                With .ThunkData(lIdx)
                    ctx.VarResult = Mid$(ctx.Contents, .CaptureBegin + 1, .CaptureEnd - .CaptureBegin)
                End With
            Case Else
                With .ThunkData(lIdx)
                    pvImplAction .Action, .CaptureBegin + 1, .CaptureEnd - .CaptureBegin
                End With
            End Select
        Next
        If IsObject(.VarResult) Then
            Set Result = .VarResult
        Else
            Result = .VarResult
        End If
        VbPegEndMatch = .BufPos + 1
        .Contents = vbNullString
        Erase .BufData
        .BufPos = 0
        .BufSize = 0
        Erase .ThunkData
        .ThunkPos = 0
        .CaptureBegin = 0
        .CaptureEnd = 0
    End With
End Function

Private Sub pvPushThunk(ByVal eAction As UcsParserActionsEnum, Optional ByVal lBegin As Long, Optional ByVal lEnd As Long)
    With ctx
        If UBound(.ThunkData) < .ThunkPos Then
            ReDim Preserve .ThunkData(0 To 2 * UBound(.ThunkData)) As UcsParserThunkType
        End If
        With .ThunkData(.ThunkPos)
            .Action = eAction
            .CaptureBegin = lBegin
            .CaptureEnd = lEnd
        End With
        .ThunkPos = .ThunkPos + 1
    End With
End Sub

Private Function pvMatchString(sText As String, Optional ByVal CmpFlags As Long) As Boolean
    With ctx
        If .BufPos + Len(sText) <= .BufSize Then
            pvMatchString = CompareStringW(LOCALE_USER_DEFAULT, CmpFlags, ByVal StrPtr(sText), Len(sText), .BufData(.BufPos), Len(sText)) = CSTR_EQUAL
        End If
    End With
End Function

Private Sub pvSetAdvance()
    With ctx
        If .BufPos > .LastBufPos Then
            .LastExpected = vbNullString
            .LastError = vbNullString
            .LastBufPos = .BufPos
        End If
    End With
End Sub

'= generated functions ===================================================

Public Function VbPegParseStmt() As Boolean
    Dim p22 As Long
    Dim q22 As Long
    Dim p12 As Long
    Dim q12 As Long

    With ctx
        pvPushThunk ucsActVarAlloc, 2
        pvPushThunk ucsActResultClear
        pvPushThunk ucsActVarSet, 1
        pvPushThunk ucsAct_1_Stmt, .CaptureBegin, .CaptureEnd
        Do
            p22 = .BufPos
            q22 = .ThunkPos
            pvPushThunk ucsActResultClear
            p12 = .BufPos
            q12 = .ThunkPos
            If VbPegParseTypedefDecl() Then
                pvPushThunk ucsActVarSet, 2
                GoTo L1
            Else
                .BufPos = p12
                .ThunkPos = q12
            End If
            If VbPegParseTypedefCallback() Then
                pvPushThunk ucsActVarSet, 2
                GoTo L1
            Else
                .BufPos = p12
                .ThunkPos = q12
            End If
            If VbPegParseEnumDecl() Then
                pvPushThunk ucsActVarSet, 2
                GoTo L1
            Else
                .BufPos = p12
                .ThunkPos = q12
            End If
            If VbPegParseStructDecl() Then
                pvPushThunk ucsActVarSet, 2
                GoTo L1
            Else
                .BufPos = p12
                .ThunkPos = q12
            End If
            If VbPegParseFunDecl() Then
                pvPushThunk ucsActVarSet, 2
                GoTo L1
            Else
                .BufPos = p12
                .ThunkPos = q12
            End If
            If VbPegParseSkipStmt() Then
                pvPushThunk ucsActVarSet, 2
                GoTo L1
            Else
                .BufPos = p12
                .ThunkPos = q12
            End If
            .BufPos = p22
            .ThunkPos = q22
            Exit Do
L1:
            pvPushThunk ucsAct_2_Stmt, .CaptureBegin, .CaptureEnd
        Loop
        If ParseEOL() Then
            pvPushThunk ucsAct_3_Stmt, .CaptureBegin, .CaptureEnd
            pvPushThunk ucsActVarAlloc, -2
            VbPegParseStmt = True
        End If
    End With
End Function

Public Function VbPegParseTypedefDecl() As Boolean
    With ctx
        pvPushThunk ucsActVarAlloc, 2
        If ParseTYPEDEF() Then
            pvPushThunk ucsActResultClear
            If VbPegParseType() Then
                pvPushThunk ucsActVarSet, 1
                pvPushThunk ucsActResultClear
                If ParseID() Then
                    pvPushThunk ucsActVarSet, 2
                    If ParseSEMI() Then
                        pvPushThunk ucsAct_1_TypedefDecl, .CaptureBegin, .CaptureEnd
                        pvPushThunk ucsActVarAlloc, -2
                        VbPegParseTypedefDecl = True
                    End If
                End If
            End If
        End If
    End With
End Function

Public Function VbPegParseTypedefCallback() As Boolean
    Dim p66 As Long
    Dim q66 As Long

    With ctx
        pvPushThunk ucsActVarAlloc, 3
        If ParseTYPEDEF() Then
            pvPushThunk ucsActResultClear
            If VbPegParseTypeUnlimited() Then
                pvPushThunk ucsActVarSet, 1
                If ParseLPAREN() Then
                    If ParseLLVM_STDCALL() Then
                        If ParseSTAR() Then
                            pvPushThunk ucsActResultClear
                            If ParseID() Then
                                pvPushThunk ucsActVarSet, 2
                                If ParseRPAREN() Then
                                    If ParseLPAREN() Then
                                        pvPushThunk ucsActResultClear
                                        p66 = .BufPos
                                        q66 = .ThunkPos
                                        If Not (VbPegParseParams()) Then
                                            .BufPos = p66
                                            .ThunkPos = q66
                                        End If
                                        pvPushThunk ucsActVarSet, 3
                                        If ParseRPAREN() Then
                                            If ParseSEMI() Then
                                                pvPushThunk ucsAct_1_TypedefCallback, .CaptureBegin, .CaptureEnd
                                                pvPushThunk ucsActVarAlloc, -3
                                                VbPegParseTypedefCallback = True
                                            End If
                                        End If
                                    End If
                                End If
                            End If
                        End If
                    End If
                End If
            End If
        End If
    End With
End Function

Public Function VbPegParseEnumDecl() As Boolean
    Dim p73 As Long
    Dim p79 As Long
    Dim q79 As Long
    Dim i108 As Long
    Dim p93 As Long
    Dim q93 As Long
    Dim p103 As Long
    Dim q103 As Long
    Dim p105 As Long
    Dim q105 As Long
    Dim p113 As Long
    Dim q113 As Long

    With ctx
        pvPushThunk ucsActVarAlloc, 4
        p73 = .BufPos
        If Not (ParseTYPEDEF()) Then
            .BufPos = p73
        End If
        If ParseENUM() Then
            pvPushThunk ucsActResultClear
            p79 = .BufPos
            q79 = .ThunkPos
            If Not (ParseID()) Then
                .BufPos = p79
                .ThunkPos = q79
            End If
            pvPushThunk ucsActVarSet, 1
            If ParseLBRACE() Then
                pvPushThunk ucsActResultClear
                pvPushThunk ucsActVarSet, 2
                pvPushThunk ucsAct_1_EnumDecl, .CaptureBegin, .CaptureEnd
                For i108 = 0 To LNG_MAXINT
                    p93 = .BufPos
                    q93 = .ThunkPos
                    pvPushThunk ucsActResultClear
                    If ParseID() Then
                        pvPushThunk ucsActVarSet, 3
                    Else
                        .BufPos = p93
                        .ThunkPos = q93
                        Exit For
                    End If
                    p103 = .BufPos
                    q103 = .ThunkPos
                    pvPushThunk ucsActResultClear
                    If VbPegParseEnumValue() Then
                        pvPushThunk ucsActVarSet, 4
                    Else
                        .BufPos = p103
                        .ThunkPos = q103
                        pvPushThunk ucsActResultClear
                        Call ParseEMPTY
                        pvPushThunk ucsActVarSet, 4
                    End If
                    p105 = .BufPos
                    q105 = .ThunkPos
                    If Not (ParseCOMMA()) Then
                        .BufPos = p105
                        .ThunkPos = q105
                    End If
                    pvPushThunk ucsAct_2_EnumDecl, .CaptureBegin, .CaptureEnd
                Next
                If i108 <> 0 Then
                    If ParseRBRACE() Then
                        p113 = .BufPos
                        q113 = .ThunkPos
                        pvPushThunk ucsActResultClear
                        If ParseID() Then
                            pvPushThunk ucsActVarSet, 1
                        Else
                            .BufPos = p113
                            .ThunkPos = q113
                        End If
                        If ParseSEMI() Then
                            pvPushThunk ucsAct_3_EnumDecl, .CaptureBegin, .CaptureEnd
                            pvPushThunk ucsActVarAlloc, -4
                            VbPegParseEnumDecl = True
                        End If
                    End If
                End If
            End If
        End If
    End With
End Function

Public Function VbPegParseStructDecl() As Boolean
    Dim p117 As Long
    Dim p123 As Long
    Dim q123 As Long
    Dim i143 As Long
    Dim p136 As Long
    Dim q136 As Long
    Dim p147 As Long
    Dim q147 As Long

    With ctx
        pvPushThunk ucsActVarAlloc, 4
        p117 = .BufPos
        If Not (ParseTYPEDEF()) Then
            .BufPos = p117
        End If
        If ParseSTRUCT() Then
            pvPushThunk ucsActResultClear
            p123 = .BufPos
            q123 = .ThunkPos
            If Not (ParseID()) Then
                .BufPos = p123
                .ThunkPos = q123
            End If
            pvPushThunk ucsActVarSet, 1
            If ParseLBRACE() Then
                pvPushThunk ucsActResultClear
                pvPushThunk ucsActVarSet, 2
                pvPushThunk ucsAct_1_StructDecl, .CaptureBegin, .CaptureEnd
                For i143 = 0 To LNG_MAXINT
                    p136 = .BufPos
                    q136 = .ThunkPos
                    pvPushThunk ucsActResultClear
                    If VbPegParseType() Then
                        pvPushThunk ucsActVarSet, 3
                    Else
                        .BufPos = p136
                        .ThunkPos = q136
                        Exit For
                    End If
                    pvPushThunk ucsActResultClear
                    If ParseID() Then
                        pvPushThunk ucsActVarSet, 4
                    Else
                        .BufPos = p136
                        .ThunkPos = q136
                        Exit For
                    End If
                    If Not (ParseSEMI()) Then
                        .BufPos = p136
                        .ThunkPos = q136
                        Exit For
                    End If
                    pvPushThunk ucsAct_2_StructDecl, .CaptureBegin, .CaptureEnd
                Next
                If i143 <> 0 Then
                    If ParseRBRACE() Then
                        p147 = .BufPos
                        q147 = .ThunkPos
                        pvPushThunk ucsActResultClear
                        If ParseID() Then
                            pvPushThunk ucsActVarSet, 1
                        Else
                            .BufPos = p147
                            .ThunkPos = q147
                        End If
                        If ParseSEMI() Then
                            pvPushThunk ucsAct_3_StructDecl, .CaptureBegin, .CaptureEnd
                            pvPushThunk ucsActVarAlloc, -4
                            VbPegParseStructDecl = True
                        End If
                    End If
                End If
            End If
        End If
    End With
End Function

Public Function VbPegParseFunDecl() As Boolean
    Dim p152 As Long
    Dim p168 As Long
    Dim q168 As Long
    Dim p173 As Long
    Dim q173 As Long

    With ctx
        pvPushThunk ucsActVarAlloc, 3
        p152 = .BufPos
        If Not (VbPegParseLinkage()) Then
            .BufPos = p152
        End If
        pvPushThunk ucsActResultClear
        If VbPegParseType() Then
            pvPushThunk ucsActVarSet, 1
            If ParseLLVM_STDCALL() Then
                pvPushThunk ucsActResultClear
                If ParseID() Then
                    pvPushThunk ucsActVarSet, 2
                    If ParseLPAREN() Then
                        p168 = .BufPos
                        q168 = .ThunkPos
                        pvPushThunk ucsActResultClear
                        If VbPegParseParams() Then
                            pvPushThunk ucsActVarSet, 3
                        Else
                            .BufPos = p168
                            .ThunkPos = q168
                        End If
                        If ParseRPAREN() Then
                            p173 = .BufPos
                            q173 = .ThunkPos
                            If ParseSEMI() Then
                                pvPushThunk ucsAct_1_FunDecl, .CaptureBegin, .CaptureEnd
                                pvPushThunk ucsActVarAlloc, -3
                                VbPegParseFunDecl = True
                                Exit Function
                            Else
                                .BufPos = p173
                                .ThunkPos = q173
                            End If
                            If ParseLBRACE() Then
                                pvPushThunk ucsAct_1_FunDecl, .CaptureBegin, .CaptureEnd
                                pvPushThunk ucsActVarAlloc, -3
                                VbPegParseFunDecl = True
                                Exit Function
                            Else
                                .BufPos = p173
                                .ThunkPos = q173
                            End If
                        End If
                    End If
                End If
            End If
        End If
    End With
End Function

Public Function VbPegParseSkipStmt() As Boolean
    Dim lCaptureBegin As Long
    Dim p189 As Long
    Dim p180 As Long
    Dim p179 As Long
    Dim p185 As Long
    Dim p194 As Long
    Dim lCaptureEnd As Long

    With ctx
        lCaptureBegin = .BufPos
        Do
            p189 = .BufPos
            p180 = .BufPos
            p179 = .BufPos
            If ParseNL() Then
                .BufPos = p189
                Exit Do
            Else
                .BufPos = p179
            End If
            If ParseSEMI() Then
                .BufPos = p189
                Exit Do
            Else
                .BufPos = p179
            End If
            .BufPos = p180
            p185 = .BufPos
            If Not (ParseBLOCKCOMMENT()) Then
                .BufPos = p185
                If Not (ParseLINECOMMENT()) Then
                    .BufPos = p185
                    If Not (ParseWS()) Then
                        .BufPos = p185
                        If .BufPos < .BufSize Then
                            .BufPos = .BufPos + 1
                        Else
                            .BufPos = p189
                            Exit Do
                        End If
                    End If
                End If
            End If
        Loop
        p194 = .BufPos
        If ParseNL() Then
            Call Parse_
            lCaptureEnd = .BufPos
            .CaptureBegin = lCaptureBegin
            .CaptureEnd = lCaptureEnd
            pvPushThunk ucsAct_1_SkipStmt, lCaptureBegin, lCaptureEnd
            VbPegParseSkipStmt = True
            Exit Function
        Else
            .BufPos = p194
        End If
        If ParseSEMI() Then
            Call Parse_
            lCaptureEnd = .BufPos
            .CaptureBegin = lCaptureBegin
            .CaptureEnd = lCaptureEnd
            pvPushThunk ucsAct_1_SkipStmt, lCaptureBegin, lCaptureEnd
            VbPegParseSkipStmt = True
            Exit Function
        Else
            .BufPos = p194
        End If
    End With
End Function

Private Function ParseEOL() As Boolean
    Dim p551 As Long

    With ctx
        p551 = .BufPos
        If Not (.BufPos < .BufSize) Then
            .BufPos = p551
            ParseEOL = True
        End If
    End With
End Function

Private Function ParseTYPEDEF() As Boolean
    Dim p378 As Long

    With ctx
        If pvMatchString("typedef", NORM_IGNORECASE) Then ' "typedef"i
            .BufPos = .BufPos + 7
            p378 = .BufPos
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                '--- do nothing
            Case Else
                .BufPos = p378
                Call Parse_
                Call pvSetAdvance
                ParseTYPEDEF = True
            End Select
        End If
    End With
End Function

Public Function VbPegParseType() As Boolean
    Dim lCaptureBegin As Long
    Dim p215 As Long
    Dim q215 As Long
    Dim p206 As Long
    Dim q206 As Long
    Dim e206 As String
    Dim p208 As Long
    Dim q208 As Long
    Dim e208 As String
    Dim p218 As Long
    Dim q218 As Long
    Dim lCaptureEnd As Long
    Dim p211 As Long
    Dim q211 As Long
    Dim e211 As String
    Dim p214 As Long
    Dim q214 As Long
    Dim e214 As String

    With ctx
        lCaptureBegin = .BufPos
        p215 = .BufPos
        q215 = .ThunkPos
        If VbPegParseTypePrefix() Then
            If VbPegParseTypeBody() Then
                p206 = .BufPos
                q206 = .ThunkPos
                e206 = .LastExpected
                If ParseLPAREN() Then
                    .BufPos = p215
                    .ThunkPos = q215
                Else
                    .BufPos = p206
                    .ThunkPos = q206
                    .LastExpected = e206
                    p208 = .BufPos
                    q208 = .ThunkPos
                    e208 = .LastExpected
                    If ParseSEMI() Then
                        .BufPos = p215
                        .ThunkPos = q215
                    Else
                        .BufPos = p208
                        .ThunkPos = q208
                        .LastExpected = e208
                        p218 = .BufPos
                        q218 = .ThunkPos
                        Call VbPegParseTypeSuffix
                        lCaptureEnd = .BufPos
                        .CaptureBegin = lCaptureBegin
                        .CaptureEnd = lCaptureEnd
                        pvPushThunk ucsAct_1_Type, lCaptureBegin, lCaptureEnd
                        VbPegParseType = True
                        Exit Function
                    End If
                End If
            Else
                .BufPos = p215
                .ThunkPos = q215
            End If
        Else
            .BufPos = p215
            .ThunkPos = q215
        End If
        If VbPegParseTypeBody() Then
            p211 = .BufPos
            q211 = .ThunkPos
            e211 = .LastExpected
            If ParseLPAREN() Then
                .BufPos = p215
                .ThunkPos = q215
            Else
                .BufPos = p211
                .ThunkPos = q211
                .LastExpected = e211
                p214 = .BufPos
                q214 = .ThunkPos
                e214 = .LastExpected
                If ParseSEMI() Then
                    .BufPos = p215
                    .ThunkPos = q215
                Else
                    .BufPos = p214
                    .ThunkPos = q214
                    .LastExpected = e214
                    p218 = .BufPos
                    q218 = .ThunkPos
                    Call VbPegParseTypeSuffix
                    lCaptureEnd = .BufPos
                    .CaptureBegin = lCaptureBegin
                    .CaptureEnd = lCaptureEnd
                    pvPushThunk ucsAct_1_Type, lCaptureBegin, lCaptureEnd
                    VbPegParseType = True
                    Exit Function
                End If
            End If
        Else
            .BufPos = p215
            .ThunkPos = q215
        End If
    End With
End Function

Private Function ParseID() As Boolean
    Dim lCaptureBegin As Long
    Dim lCaptureEnd As Long

    With ctx
        lCaptureBegin = .BufPos
        Select Case .BufData(.BufPos)
        Case 97 To 122, 65 To 90, 95                ' [a-zA-Z_]
            .BufPos = .BufPos + 1
            Do
                Select Case .BufData(.BufPos)
                Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                    .BufPos = .BufPos + 1
                Case Else
                    Exit Do
                End Select
            Loop
            lCaptureEnd = .BufPos
            Call Parse_
            .CaptureBegin = lCaptureBegin
            .CaptureEnd = lCaptureEnd
            pvPushThunk ucsAct_1_ID, lCaptureBegin, lCaptureEnd
            Call pvSetAdvance
            ParseID = True
        End Select
    End With
End Function

Private Function ParseSEMI() As Boolean
    With ctx
        If .BufData(.BufPos) = 59 Then              ' ";"
            .BufPos = .BufPos + 1
            Call Parse_
            Call pvSetAdvance
            ParseSEMI = True
        End If
    End With
End Function

Public Function VbPegParseTypeUnlimited() As Boolean
    Dim lCaptureBegin As Long
    Dim p227 As Long
    Dim q227 As Long
    Dim p229 As Long
    Dim q229 As Long
    Dim lCaptureEnd As Long

    With ctx
        lCaptureBegin = .BufPos
        p227 = .BufPos
        q227 = .ThunkPos
        If VbPegParseTypePrefix() Then
            If VbPegParseTypeBody() Then
                p229 = .BufPos
                q229 = .ThunkPos
                Call VbPegParseTypeSuffix
                lCaptureEnd = .BufPos
                .CaptureBegin = lCaptureBegin
                .CaptureEnd = lCaptureEnd
                pvPushThunk ucsAct_1_TypeUnlimited, lCaptureBegin, lCaptureEnd
                VbPegParseTypeUnlimited = True
                Exit Function
            Else
                .BufPos = p227
                .ThunkPos = q227
            End If
        Else
            .BufPos = p227
            .ThunkPos = q227
        End If
        If VbPegParseTypeBody() Then
            p229 = .BufPos
            q229 = .ThunkPos
            Call VbPegParseTypeSuffix
            lCaptureEnd = .BufPos
            .CaptureBegin = lCaptureBegin
            .CaptureEnd = lCaptureEnd
            pvPushThunk ucsAct_1_TypeUnlimited, lCaptureBegin, lCaptureEnd
            VbPegParseTypeUnlimited = True
            Exit Function
        Else
            .BufPos = p227
            .ThunkPos = q227
        End If
    End With
End Function

Private Function ParseLPAREN() As Boolean
    With ctx
        If .BufData(.BufPos) = 40 Then              ' "("
            .BufPos = .BufPos + 1
            Call Parse_
            Call pvSetAdvance
            ParseLPAREN = True
        End If
    End With
End Function

Private Function ParseLLVM_STDCALL() As Boolean
    Dim p448 As Long

    With ctx
        If pvMatchString("LLVM_STDCALL") Then       ' "LLVM_STDCALL"
            .BufPos = .BufPos + 12
            p448 = .BufPos
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                '--- do nothing
            Case Else
                .BufPos = p448
                Call Parse_
                Call pvSetAdvance
                ParseLLVM_STDCALL = True
            End Select
        End If
    End With
End Function

Private Function ParseSTAR() As Boolean
    With ctx
        If .BufData(.BufPos) = 42 Then              ' "*"
            .BufPos = .BufPos + 1
            Call Parse_
            Call pvSetAdvance
            ParseSTAR = True
        End If
    End With
End Function

Private Function ParseRPAREN() As Boolean
    With ctx
        If .BufData(.BufPos) = 41 Then              ' ")"
            .BufPos = .BufPos + 1
            Call Parse_
            Call pvSetAdvance
            ParseRPAREN = True
        End If
    End With
End Function

Public Function VbPegParseParams() As Boolean
    Dim p287 As Long
    Dim q287 As Long

    With ctx
        pvPushThunk ucsActVarAlloc, 2
        pvPushThunk ucsActResultClear
        If VbPegParseParam() Then
            pvPushThunk ucsActVarSet, 1
            pvPushThunk ucsAct_1_Params, .CaptureBegin, .CaptureEnd
            Do
                p287 = .BufPos
                q287 = .ThunkPos
                If Not (ParseCOMMA()) Then
                    .BufPos = p287
                    .ThunkPos = q287
                    Exit Do
                End If
                pvPushThunk ucsActResultClear
                If VbPegParseParam() Then
                    pvPushThunk ucsActVarSet, 2
                Else
                    .BufPos = p287
                    .ThunkPos = q287
                    Exit Do
                End If
                pvPushThunk ucsAct_2_Params, .CaptureBegin, .CaptureEnd
            Loop
            pvPushThunk ucsAct_3_Params, .CaptureBegin, .CaptureEnd
            pvPushThunk ucsActVarAlloc, -2
            VbPegParseParams = True
        End If
    End With
End Function

Private Function ParseENUM() As Boolean
    Dim p453 As Long

    With ctx
        If pvMatchString("enum") Then               ' "enum"
            .BufPos = .BufPos + 4
            p453 = .BufPos
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                '--- do nothing
            Case Else
                .BufPos = p453
                Call Parse_
                Call pvSetAdvance
                ParseENUM = True
            End Select
        End If
    End With
End Function

Private Function ParseLBRACE() As Boolean
    With ctx
        If .BufData(.BufPos) = 123 Then             ' "{"
            .BufPos = .BufPos + 1
            Call Parse_
            Call pvSetAdvance
            ParseLBRACE = True
        End If
    End With
End Function

Public Function VbPegParseEnumValue() As Boolean
    Dim p323 As Long
    Dim q323 As Long

    With ctx
        pvPushThunk ucsActVarAlloc, 2
        If ParseEQ() Then
            pvPushThunk ucsActResultClear
            pvPushThunk ucsActVarSet, 1
            pvPushThunk ucsAct_1_EnumValue, .CaptureBegin, .CaptureEnd
            pvPushThunk ucsActResultClear
            If VbPegParseEnumValueToken() Then
                pvPushThunk ucsActVarSet, 2
                pvPushThunk ucsAct_2_EnumValue, .CaptureBegin, .CaptureEnd
                Do
                    p323 = .BufPos
                    q323 = .ThunkPos
                    pvPushThunk ucsActResultClear
                    If VbPegParseEnumValueToken() Then
                        pvPushThunk ucsActVarSet, 2
                    Else
                        .BufPos = p323
                        .ThunkPos = q323
                        Exit Do
                    End If
                    pvPushThunk ucsAct_3_EnumValue, .CaptureBegin, .CaptureEnd
                Loop
                pvPushThunk ucsAct_4_EnumValue, .CaptureBegin, .CaptureEnd
                pvPushThunk ucsActVarAlloc, -2
                VbPegParseEnumValue = True
            End If
        End If
    End With
End Function

Private Sub ParseEMPTY()
    Dim lCaptureBegin As Long
    Dim lCaptureEnd As Long

    With ctx
        lCaptureBegin = .BufPos
        lCaptureEnd = .BufPos
        .CaptureBegin = lCaptureBegin
        .CaptureEnd = lCaptureEnd
        pvPushThunk ucsAct_1_EMPTY, lCaptureBegin, lCaptureEnd
L20:
    End With
End Sub

Private Function ParseCOMMA() As Boolean
    With ctx
        If .BufData(.BufPos) = 44 Then              ' ","
            .BufPos = .BufPos + 1
            Call Parse_
            Call pvSetAdvance
            ParseCOMMA = True
        End If
    End With
End Function

Private Function ParseRBRACE() As Boolean
    With ctx
        If .BufData(.BufPos) = 125 Then             ' "}"
            .BufPos = .BufPos + 1
            Call Parse_
            Call pvSetAdvance
            ParseRBRACE = True
        End If
    End With
End Function

Private Function ParseSTRUCT() As Boolean
    Dim p458 As Long

    With ctx
        If pvMatchString("struct") Then             ' "struct"
            .BufPos = .BufPos + 6
            p458 = .BufPos
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                '--- do nothing
            Case Else
                .BufPos = p458
                Call Parse_
                Call pvSetAdvance
                ParseSTRUCT = True
            End Select
        End If
    End With
End Function

Public Function VbPegParseLinkage() As Boolean
    Dim p359 As Long
    Dim p361 As Long

    With ctx
        p359 = .BufPos
        If ParseEXTERN() Then
            p361 = .BufPos
            If Not (ParseINLINE()) Then
                .BufPos = p361
            End If
            VbPegParseLinkage = True
            Exit Function
        Else
            .BufPos = p359
        End If
        If ParseSTATIC() Then
            p361 = .BufPos
            If Not (ParseINLINE()) Then
                .BufPos = p361
            End If
            VbPegParseLinkage = True
            Exit Function
        Else
            .BufPos = p359
        End If
    End With
End Function

Private Function ParseNL() As Boolean
    Dim p520 As Long

    With ctx
        If .BufData(.BufPos) = 13 Then              ' "\r"
            .BufPos = .BufPos + 1
        End If
        If .BufData(.BufPos) = 10 Then              ' "\n"
            .BufPos = .BufPos + 1
            p520 = .BufPos
            If Not (ParsePREPRO()) Then
                .BufPos = p520
            End If
            Call pvSetAdvance
            ParseNL = True
        End If
    End With
End Function

Private Function ParseBLOCKCOMMENT() As Boolean
    Dim p536 As Long
    Dim p529 As Long
    Dim p535 As Long
    Dim p531 As Long

    With ctx
        If .BufData(.BufPos) = 47 And .BufData(.BufPos + 1) = 42 Then ' "/*"
            .BufPos = .BufPos + 2
            Do
                p536 = .BufPos
                p529 = .BufPos
                If .BufData(.BufPos) = 42 And .BufData(.BufPos + 1) = 47 Then ' "*/"
                    .BufPos = p536
                    Exit Do
                Else
                    .BufPos = p529
                End If
                p535 = .BufPos
                p531 = .BufPos
                If .BufData(.BufPos) = 47 And .BufData(.BufPos + 1) = 42 Then ' "/*"
                    .BufPos = p531
                    If Not (ParseBLOCKCOMMENT()) Then
                        .BufPos = p535
                        If .BufPos < .BufSize Then
                            .BufPos = .BufPos + 1
                        Else
                            .BufPos = p536
                            Exit Do
                        End If
                    End If
                Else
                    .BufPos = p535
                    If .BufPos < .BufSize Then
                        .BufPos = .BufPos + 1
                    Else
                        .BufPos = p536
                        Exit Do
                    End If
                End If
            Loop
            If .BufData(.BufPos) = 42 And .BufData(.BufPos + 1) = 47 Then ' "*/"
                .BufPos = .BufPos + 2
                Call pvSetAdvance
                ParseBLOCKCOMMENT = True
            End If
        End If
    End With
End Function

Private Function ParseLINECOMMENT() As Boolean
    With ctx
        If .BufData(.BufPos) = 47 And .BufData(.BufPos + 1) = 47 Then ' "//"
            .BufPos = .BufPos + 2
            Do
                Select Case .BufData(.BufPos)
                Case 13, 10                         ' [\r\n]
                    Exit Do
                Case Else
                    If .BufPos < .BufSize Then
                        .BufPos = .BufPos + 1
                    Else
                        Exit Do
                    End If
                End Select
            Loop
            If ParseNL() Then
                Call pvSetAdvance
                ParseLINECOMMENT = True
            End If
        End If
    End With
End Function

Private Function ParseWS() As Boolean
    Dim i514 As Long
    Dim p513 As Long

    With ctx
        For i514 = 0 To LNG_MAXINT
            p513 = .BufPos
            Select Case .BufData(.BufPos)
            Case 32, 9                              ' [ \t]
                .BufPos = .BufPos + 1
            Case Else
                If Not (ParseNL()) Then
                    .BufPos = p513
                    .BufPos = p513
                    Exit For
                End If
            End Select
        Next
        If i514 <> 0 Then
            Call pvSetAdvance
            ParseWS = True
        End If
    End With
End Function

Private Sub Parse_()
    Dim p508 As Long

    With ctx
        Do
            p508 = .BufPos
            If Not (ParseBLOCKCOMMENT()) Then
                .BufPos = p508
                If Not (ParseLINECOMMENT()) Then
                    .BufPos = p508
                    If Not (ParseWS()) Then
                        .BufPos = p508
                        .BufPos = p508
                        Exit Do
                    End If
                End If
            End If
        Loop
    End With
End Sub

Public Function VbPegParseTypePrefix() As Boolean
    Dim p237 As Long

    With ctx
        p237 = .BufPos
        If ParseCONST() Then
            VbPegParseTypePrefix = True
            Exit Function
        Else
            .BufPos = p237
        End If
        If ParseUNSIGNED() Then
            VbPegParseTypePrefix = True
            Exit Function
        Else
            .BufPos = p237
        End If
        If ParseSTRUCT() Then
            VbPegParseTypePrefix = True
            Exit Function
        Else
            .BufPos = p237
        End If
        If ParseENUM() Then
            VbPegParseTypePrefix = True
            Exit Function
        Else
            .BufPos = p237
        End If
    End With
End Function

Public Function VbPegParseTypeBody() As Boolean
    Dim p244 As Long
    Dim q244 As Long

    With ctx
        p244 = .BufPos
        q244 = .ThunkPos
        If ParseINT() Then
            VbPegParseTypeBody = True
            Exit Function
        Else
            .BufPos = p244
            .ThunkPos = q244
        End If
        If ParseCHAR() Then
            VbPegParseTypeBody = True
            Exit Function
        Else
            .BufPos = p244
            .ThunkPos = q244
        End If
        If ParseUNSIGNED() Then
            VbPegParseTypeBody = True
            Exit Function
        Else
            .BufPos = p244
            .ThunkPos = q244
        End If
        If ParseVOID() Then
            VbPegParseTypeBody = True
            Exit Function
        Else
            .BufPos = p244
            .ThunkPos = q244
        End If
        If ParseUINT_A_T() Then
            VbPegParseTypeBody = True
            Exit Function
        Else
            .BufPos = p244
            .ThunkPos = q244
        End If
        If ParseUINT_B_T() Then
            VbPegParseTypeBody = True
            Exit Function
        Else
            .BufPos = p244
            .ThunkPos = q244
        End If
        If ParseUINT_C_T() Then
            VbPegParseTypeBody = True
            Exit Function
        Else
            .BufPos = p244
            .ThunkPos = q244
        End If
        If ParseUINTPTR_T() Then
            VbPegParseTypeBody = True
            Exit Function
        Else
            .BufPos = p244
            .ThunkPos = q244
        End If
        If ParseSIZE_T() Then
            VbPegParseTypeBody = True
            Exit Function
        Else
            .BufPos = p244
            .ThunkPos = q244
        End If
        If ParseDOUBLE() Then
            VbPegParseTypeBody = True
            Exit Function
        Else
            .BufPos = p244
            .ThunkPos = q244
        End If
        If ParseLONG_LONG() Then
            VbPegParseTypeBody = True
            Exit Function
        Else
            .BufPos = p244
            .ThunkPos = q244
        End If
        If ParseBOOL() Then
            VbPegParseTypeBody = True
            Exit Function
        Else
            .BufPos = p244
            .ThunkPos = q244
        End If
        If VbPegParseRefType() Then
            VbPegParseTypeBody = True
            Exit Function
        Else
            .BufPos = p244
            .ThunkPos = q244
        End If
    End With
End Function

Public Sub VbPegParseTypeSuffix()
    Dim p269 As Long
    Dim p266 As Long

    With ctx
        Do
            p269 = .BufPos
            p266 = .BufPos
            If Not (ParseCONST()) Then
                .BufPos = p266
            End If
            If Not (ParseSTAR()) Then
                .BufPos = p269
                Exit Do
            End If
        Loop
    End With
End Sub

Private Function ParseCONST() As Boolean
    Dim p398 As Long

    With ctx
        If pvMatchString("const") Then              ' "const"
            .BufPos = .BufPos + 5
            p398 = .BufPos
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                '--- do nothing
            Case Else
                .BufPos = p398
                Call Parse_
                Call pvSetAdvance
                ParseCONST = True
            End Select
        End If
    End With
End Function

Private Function ParseUNSIGNED() As Boolean
    Dim p393 As Long

    With ctx
        If pvMatchString("unsigned") Then           ' "unsigned"
            .BufPos = .BufPos + 8
            p393 = .BufPos
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                '--- do nothing
            Case Else
                .BufPos = p393
                Call Parse_
                Call pvSetAdvance
                ParseUNSIGNED = True
            End Select
        End If
    End With
End Function

Private Function ParseINT() As Boolean
    Dim p383 As Long

    With ctx
        If pvMatchString("int") Then                ' "int"
            .BufPos = .BufPos + 3
            p383 = .BufPos
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                '--- do nothing
            Case Else
                .BufPos = p383
                Call Parse_
                Call pvSetAdvance
                ParseINT = True
            End Select
        End If
    End With
End Function

Private Function ParseCHAR() As Boolean
    Dim p388 As Long

    With ctx
        If pvMatchString("char") Then               ' "char"
            .BufPos = .BufPos + 4
            p388 = .BufPos
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                '--- do nothing
            Case Else
                .BufPos = p388
                Call Parse_
                Call pvSetAdvance
                ParseCHAR = True
            End Select
        End If
    End With
End Function

Private Function ParseVOID() As Boolean
    Dim p403 As Long

    With ctx
        If pvMatchString("void") Then               ' "void"
            .BufPos = .BufPos + 4
            p403 = .BufPos
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                '--- do nothing
            Case Else
                .BufPos = p403
                Call Parse_
                Call pvSetAdvance
                ParseVOID = True
            End Select
        End If
    End With
End Function

Private Function ParseUINT_A_T() As Boolean
    Dim p408 As Long

    With ctx
        If pvMatchString("uint8_t") Then            ' "uint8_t"
            .BufPos = .BufPos + 7
            p408 = .BufPos
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                '--- do nothing
            Case Else
                .BufPos = p408
                Call Parse_
                Call pvSetAdvance
                ParseUINT_A_T = True
            End Select
        End If
    End With
End Function

Private Function ParseUINT_B_T() As Boolean
    Dim p413 As Long

    With ctx
        If pvMatchString("uint32_t") Then           ' "uint32_t"
            .BufPos = .BufPos + 8
            p413 = .BufPos
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                '--- do nothing
            Case Else
                .BufPos = p413
                Call Parse_
                Call pvSetAdvance
                ParseUINT_B_T = True
            End Select
        End If
    End With
End Function

Private Function ParseUINT_C_T() As Boolean
    Dim p418 As Long

    With ctx
        If pvMatchString("uint64_t") Then           ' "uint64_t"
            .BufPos = .BufPos + 8
            p418 = .BufPos
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                '--- do nothing
            Case Else
                .BufPos = p418
                Call Parse_
                Call pvSetAdvance
                ParseUINT_C_T = True
            End Select
        End If
    End With
End Function

Private Function ParseUINTPTR_T() As Boolean
    Dim p423 As Long

    With ctx
        If pvMatchString("uintptr_t") Then          ' "uintptr_t"
            .BufPos = .BufPos + 9
            p423 = .BufPos
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                '--- do nothing
            Case Else
                .BufPos = p423
                Call Parse_
                Call pvSetAdvance
                ParseUINTPTR_T = True
            End Select
        End If
    End With
End Function

Private Function ParseSIZE_T() As Boolean
    Dim p428 As Long

    With ctx
        If pvMatchString("size_t") Then             ' "size_t"
            .BufPos = .BufPos + 6
            p428 = .BufPos
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                '--- do nothing
            Case Else
                .BufPos = p428
                Call Parse_
                Call pvSetAdvance
                ParseSIZE_T = True
            End Select
        End If
    End With
End Function

Private Function ParseDOUBLE() As Boolean
    Dim p433 As Long

    With ctx
        If pvMatchString("double") Then             ' "double"
            .BufPos = .BufPos + 6
            p433 = .BufPos
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                '--- do nothing
            Case Else
                .BufPos = p433
                Call Parse_
                Call pvSetAdvance
                ParseDOUBLE = True
            End Select
        End If
    End With
End Function

Private Function ParseLONG_LONG() As Boolean
    Dim p438 As Long

    With ctx
        If pvMatchString("long long") Then          ' "long long"
            .BufPos = .BufPos + 9
            p438 = .BufPos
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                '--- do nothing
            Case Else
                .BufPos = p438
                Call Parse_
                Call pvSetAdvance
                ParseLONG_LONG = True
            End Select
        End If
    End With
End Function

Private Function ParseBOOL() As Boolean
    Dim p443 As Long

    With ctx
        If pvMatchString("bool") Then               ' "bool"
            .BufPos = .BufPos + 4
            p443 = .BufPos
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                '--- do nothing
            Case Else
                .BufPos = p443
                Call Parse_
                Call pvSetAdvance
                ParseBOOL = True
            End Select
        End If
    End With
End Function

Public Function VbPegParseRefType() As Boolean
    Dim p272 As Long
    Dim e272 As String

    With ctx
        p272 = .BufPos
        e272 = .LastExpected
        If Not (ParseLLVM_STDCALL()) Then
            .BufPos = p272
            .LastExpected = e272
            If ParseID() Then
                If IsRefType(Mid$(.Contents, .CaptureBegin + 1, .CaptureEnd - .CaptureBegin)) Then
                    VbPegParseRefType = True
                End If
            End If
        End If
    End With
End Function

Public Function VbPegParseParam() As Boolean
    Dim p296 As Long
    Dim q296 As Long
    Dim p302 As Long
    Dim q302 As Long

    With ctx
        pvPushThunk ucsActVarAlloc, 3
        pvPushThunk ucsActResultClear
        If VbPegParseType() Then
            pvPushThunk ucsActVarSet, 1
            pvPushThunk ucsActResultClear
            p296 = .BufPos
            q296 = .ThunkPos
            If Not (ParseID()) Then
                .BufPos = p296
                .ThunkPos = q296
            End If
            pvPushThunk ucsActVarSet, 2
            pvPushThunk ucsActResultClear
            p302 = .BufPos
            q302 = .ThunkPos
            If Not (VbPegParseArraySuffix()) Then
                .BufPos = p302
                .ThunkPos = q302
            End If
            pvPushThunk ucsActVarSet, 3
            pvPushThunk ucsAct_1_Param, .CaptureBegin, .CaptureEnd
            pvPushThunk ucsActVarAlloc, -3
            VbPegParseParam = True
        End If
    End With
End Function

Public Function VbPegParseArraySuffix() As Boolean
    Dim lCaptureBegin As Long
    Dim p349 As Long
    Dim p347 As Long
    Dim e347 As String
    Dim lCaptureEnd As Long

    With ctx
        lCaptureBegin = .BufPos
        If ParseLBRACKET() Then
            Do
                p349 = .BufPos
                p347 = .BufPos
                e347 = .LastExpected
                If ParseRBRACKET() Then
                    .BufPos = p349
                    Exit Do
                Else
                    .BufPos = p347
                    .LastExpected = e347
                End If
                If .BufPos < .BufSize Then
                    .BufPos = .BufPos + 1
                Else
                    .BufPos = p349
                    Exit Do
                End If
            Loop
            If ParseRBRACKET() Then
                lCaptureEnd = .BufPos
                Call Parse_
                .CaptureBegin = lCaptureBegin
                .CaptureEnd = lCaptureEnd
                pvPushThunk ucsAct_1_ArraySuffix, lCaptureBegin, lCaptureEnd
                VbPegParseArraySuffix = True
            End If
        End If
    End With
End Function

Private Function ParseEQ() As Boolean
    With ctx
        If .BufData(.BufPos) = 61 Then              ' "="
            .BufPos = .BufPos + 1
            Call Parse_
            Call pvSetAdvance
            ParseEQ = True
        End If
    End With
End Function

Public Function VbPegParseEnumValueToken() As Boolean
    Dim lCaptureBegin As Long
    Dim i336 As Long
    Dim p335 As Long
    Dim p333 As Long
    Dim p330 As Long
    Dim lCaptureEnd As Long

    With ctx
        lCaptureBegin = .BufPos
        For i336 = 0 To LNG_MAXINT
            p335 = .BufPos
            p333 = .BufPos
            p330 = .BufPos
            If ParseBLOCKCOMMENT() Then
                .BufPos = p335
                Exit For
            Else
                .BufPos = p330
            End If
            If ParseLINECOMMENT() Then
                .BufPos = p335
                Exit For
            Else
                .BufPos = p330
            End If
            If ParseWS() Then
                .BufPos = p335
                Exit For
            Else
                .BufPos = p330
            End If
            Select Case .BufData(.BufPos)
            Case 44, 125                            ' [,}]
                .BufPos = .BufPos + 1
                .BufPos = p335
                Exit For
            End Select
            .BufPos = p333
            If .BufPos < .BufSize Then
                .BufPos = .BufPos + 1
            Else
                .BufPos = p335
                Exit For
            End If
        Next
        If i336 <> 0 Then
            lCaptureEnd = .BufPos
            Call Parse_
            .CaptureBegin = lCaptureBegin
            .CaptureEnd = lCaptureEnd
            pvPushThunk ucsAct_1_EnumValueToken, lCaptureBegin, lCaptureEnd
            Call pvSetAdvance
            VbPegParseEnumValueToken = True
        End If
    End With
End Function

Private Function ParseLBRACKET() As Boolean
    With ctx
        If .BufData(.BufPos) = 91 Then              ' "["
            .BufPos = .BufPos + 1
            Call Parse_
            Call pvSetAdvance
            ParseLBRACKET = True
        End If
    End With
End Function

Private Function ParseRBRACKET() As Boolean
    With ctx
        If .BufData(.BufPos) = 93 Then              ' "]"
            .BufPos = .BufPos + 1
            Call Parse_
            Call pvSetAdvance
            ParseRBRACKET = True
        End If
    End With
End Function

Private Function ParseEXTERN() As Boolean
    Dim p463 As Long

    With ctx
        If pvMatchString("extern") Then             ' "extern"
            .BufPos = .BufPos + 6
            p463 = .BufPos
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                '--- do nothing
            Case Else
                .BufPos = p463
                Call Parse_
                Call pvSetAdvance
                ParseEXTERN = True
            End Select
        End If
    End With
End Function

Private Function ParseSTATIC() As Boolean
    Dim p468 As Long

    With ctx
        If pvMatchString("static") Then             ' "static"
            .BufPos = .BufPos + 6
            p468 = .BufPos
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                '--- do nothing
            Case Else
                .BufPos = p468
                Call Parse_
                Call pvSetAdvance
                ParseSTATIC = True
            End Select
        End If
    End With
End Function

Private Function ParseINLINE() As Boolean
    Dim p473 As Long

    With ctx
        If pvMatchString("inline") Then             ' "inline"
            .BufPos = .BufPos + 6
            p473 = .BufPos
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                '--- do nothing
            Case Else
                .BufPos = p473
                Call Parse_
                Call pvSetAdvance
                ParseINLINE = True
            End Select
        End If
    End With
End Function

Private Function ParsePREPRO() As Boolean
    With ctx
        If .BufData(.BufPos) = 35 Then              ' "#"
            .BufPos = .BufPos + 1
            Do
                Select Case .BufData(.BufPos)
                Case 13, 10                         ' [\r\n]
                    Exit Do
                Case Else
                    If .BufPos < .BufSize Then
                        .BufPos = .BufPos + 1
                    Else
                        Exit Do
                    End If
                End Select
            Loop
            If ParseNL() Then
                Call pvSetAdvance
                ParsePREPRO = True
            End If
        End If
    End With
End Function

Private Sub pvImplAction(ByVal eAction As UcsParserActionsEnum, ByVal lOffset As Long, ByVal lSize As Long)
    Dim oJson As Object
    Dim oEl As Object
    
    With ctx
    Select Case eAction
    Case ucsAct_3_Stmt
           Set ctx.VarResult = ctx.VarStack(ctx.VarPos - 1)
    Case ucsAct_2_Stmt
           Set oJson = ctx.VarStack(ctx.VarPos - 1) : JsonItem(oJson, -1) = ctx.VarStack(ctx.VarPos - 2)
    Case ucsAct_1_Stmt
           JsonItem(oJson, -1) = Empty
                                                                            Set ctx.VarStack(ctx.VarPos - 1) = oJson
    Case ucsAct_1_TypedefDecl
           JsonItem(oJson, "Tag") = "TypedefDecl"
                                                                            JsonItem(oJson, "Name") = ctx.VarStack(ctx.VarPos - 2)
                                                                            JsonItem(oJson, "Type") = ctx.VarStack(ctx.VarPos - 1)
                                                                            Set ctx.VarResult = oJson
    Case ucsAct_1_TypedefCallback
           JsonItem(oJson, "Tag") = "TypedefCallback"
                                                                            JsonItem(oJson, "Name") = ctx.VarStack(ctx.VarPos - 2)
                                                                            JsonItem(oJson, "Type") = ctx.VarStack(ctx.VarPos - 1)
                                                                            JsonItem(oJson, "Params") = ctx.VarStack(ctx.VarPos - 3)
                                                                            Set ctx.VarResult = oJson
    Case ucsAct_3_EnumDecl
           Set oJson = ctx.VarStack(ctx.VarPos - 2)
                                                                            JsonItem(oJson, "Name") = ctx.VarStack(ctx.VarPos - 1)
                                                                            Set ctx.VarResult = oJson
    Case ucsAct_2_EnumDecl
           JsonItem(oEl, "Name") = ctx.VarStack(ctx.VarPos - 3)
                                                                            JsonItem(oEl, "Value") = zn(CStr(ctx.VarStack(ctx.VarPos - 4)), Empty)
                                                                            Set oJson = ctx.VarStack(ctx.VarPos - 2)
                                                                            JsonItem(oJson, "Items/-1") = oEl
    Case ucsAct_1_EnumDecl
           JsonItem(oJson, "Tag") = "EnumDecl" 
                                                                            JsonItem(oJson, "Items/-1") = Empty
                                                                            Set ctx.VarStack(ctx.VarPos - 2) = oJson
    Case ucsAct_3_StructDecl
           Set oJson = ctx.VarStack(ctx.VarPos - 2)
                                                                            JsonItem(oJson, "Name") = ctx.VarStack(ctx.VarPos - 1)
                                                                            Set ctx.VarResult = oJson
    Case ucsAct_2_StructDecl
           JsonItem(oEl, "Name") = ctx.VarStack(ctx.VarPos - 4)
                                                                            JsonItem(oEl, "Type") = ctx.VarStack(ctx.VarPos - 3)
                                                                            Set oJson = ctx.VarStack(ctx.VarPos - 2)
                                                                            JsonItem(oJson, "Items/-1") = oEl
    Case ucsAct_1_StructDecl
           JsonItem(oJson, "Tag") = "StructDecl" 
                                                                            JsonItem(oJson, "Items/-1") = Empty
                                                                            Set ctx.VarStack(ctx.VarPos - 2) = oJson
    Case ucsAct_1_FunDecl
           JsonItem(oJson, "Tag") = "FunDecl"
                                                                            JsonItem(oJson, "Name") = ctx.VarStack(ctx.VarPos - 2)
                                                                            JsonItem(oJson, "Type") = ctx.VarStack(ctx.VarPos - 1)
                                                                            JsonItem(oJson, "Params") = ctx.VarStack(ctx.VarPos - 3)
                                                                            Set ctx.VarResult = oJson
    Case ucsAct_1_SkipStmt
           JsonItem(oJson, "Tag") = "SkipStmt"
                                                                            JsonItem(oJson, "Text") = Mid$(ctx.Contents, lOffset, lSize)
                                                                            Set ctx.VarResult = oJson
    Case ucsAct_1_Type
         ctx.VarResult = Mid$(ctx.Contents, lOffset, lSize)
    Case ucsAct_1_ID
           ctx.VarResult = Mid$(ctx.Contents, lOffset, lSize)
    Case ucsAct_1_TypeUnlimited
         ctx.VarResult = Mid$(ctx.Contents, lOffset, lSize)
    Case ucsAct_3_Params
           Set ctx.VarResult = ctx.VarStack(ctx.VarPos - 1)
    Case ucsAct_2_Params
           Set oJson = ctx.VarStack(ctx.VarPos - 1) : JsonItem(oJson, -1) = ctx.VarStack(ctx.VarPos - 2)
    Case ucsAct_1_Params
           JsonItem(oJson, -1) = ctx.VarStack(ctx.VarPos - 1) : Set ctx.VarStack(ctx.VarPos - 1) = oJson
    Case ucsAct_4_EnumValue
           Set oJson = ctx.VarStack(ctx.VarPos - 1) : ctx.VarResult = ConcatCollection(oJson, " ")
    Case ucsAct_3_EnumValue
           ctx.VarStack(ctx.VarPos - 1).Add ctx.VarStack(ctx.VarPos - 2)
    Case ucsAct_2_EnumValue
           ctx.VarStack(ctx.VarPos - 1).Add ctx.VarStack(ctx.VarPos - 2)
    Case ucsAct_1_EnumValue
           Set ctx.VarStack(ctx.VarPos - 1) = New Collection
    Case ucsAct_1_EMPTY
           ctx.VarResult = Mid$(ctx.Contents, lOffset, lSize)
    Case ucsAct_1_Param
           JsonItem(oJson, "Type") = ctx.VarStack(ctx.VarPos - 1)
                                                                            JsonItem(oJson, "Name") = ctx.VarStack(ctx.VarPos - 2)
                                                                            JsonItem(oJson, "ArraySuffix") = zn(CStr(ctx.VarStack(ctx.VarPos - 3)), Empty)
                                                                            Set ctx.VarResult = oJson
    Case ucsAct_1_ArraySuffix
           ctx.VarResult = Mid$(ctx.Contents, lOffset, lSize)
    Case ucsAct_1_EnumValueToken
           ctx.VarResult = Mid$(ctx.Contents, lOffset, lSize)
    End Select
    End With
End Sub

