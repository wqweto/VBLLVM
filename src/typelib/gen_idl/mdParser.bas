Attribute VB_Name = "mdParser"
' Auto-generated on 20.7.2018 20:17:03
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
    VbPegParserVersion = "20.7.2018 20:17:03"
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
    Dim p5 As Long
    Dim q5 As Long
    Dim p12 As Long
    Dim q12 As Long

    With ctx
        pvPushThunk ucsActVarAlloc, 2
        p5 = .BufPos
        q5 = .ThunkPos
        pvPushThunk ucsActResultClear
        pvPushThunk ucsActVarSet, 1
        pvPushThunk ucsAct_1_Stmt, .CaptureBegin, .CaptureEnd
        Do
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
            Exit Do
L1:
            pvPushThunk ucsAct_2_Stmt, .CaptureBegin, .CaptureEnd
        Loop
        If ParseEOL() Then
            pvPushThunk ucsAct_3_Stmt, .CaptureBegin, .CaptureEnd
            pvPushThunk ucsActVarAlloc, -2
            Call pvSetAdvance
            VbPegParseStmt = True
            Exit Function
        Else
            .BufPos = p5
            .ThunkPos = q5
        End If
    End With
End Function

Public Function VbPegParseTypedefDecl() As Boolean
    Dim p35 As Long
    Dim q35 As Long

    With ctx
        pvPushThunk ucsActVarAlloc, 2
        p35 = .BufPos
        q35 = .ThunkPos
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
                        Exit Function
                    Else
                        .BufPos = p35
                        .ThunkPos = q35
                    End If
                Else
                    .BufPos = p35
                    .ThunkPos = q35
                End If
            Else
                .BufPos = p35
                .ThunkPos = q35
            End If
        End If
    End With
End Function

Public Function VbPegParseTypedefCallback() As Boolean
    Dim p50 As Long
    Dim q50 As Long

    With ctx
        pvPushThunk ucsActVarAlloc, 3
        p50 = .BufPos
        q50 = .ThunkPos
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
                                        Call VbPegParseParams
                                        pvPushThunk ucsActVarSet, 3
                                        If ParseRPAREN() Then
                                            If ParseSEMI() Then
                                                pvPushThunk ucsAct_1_TypedefCallback, .CaptureBegin, .CaptureEnd
                                                pvPushThunk ucsActVarAlloc, -3
                                                VbPegParseTypedefCallback = True
                                                Exit Function
                                            Else
                                                .BufPos = p50
                                                .ThunkPos = q50
                                            End If
                                        Else
                                            .BufPos = p50
                                            .ThunkPos = q50
                                        End If
                                    Else
                                        .BufPos = p50
                                        .ThunkPos = q50
                                    End If
                                Else
                                    .BufPos = p50
                                    .ThunkPos = q50
                                End If
                            Else
                                .BufPos = p50
                                .ThunkPos = q50
                            End If
                        Else
                            .BufPos = p50
                            .ThunkPos = q50
                        End If
                    Else
                        .BufPos = p50
                        .ThunkPos = q50
                    End If
                Else
                    .BufPos = p50
                    .ThunkPos = q50
                End If
            Else
                .BufPos = p50
                .ThunkPos = q50
            End If
        End If
    End With
End Function

Public Function VbPegParseEnumDecl() As Boolean
    Dim p77 As Long
    Dim q77 As Long
    Dim i108 As Long
    Dim p93 As Long
    Dim q93 As Long
    Dim p98 As Long
    Dim q98 As Long
    Dim p113 As Long
    Dim q113 As Long

    With ctx
        pvPushThunk ucsActVarAlloc, 4
        p77 = .BufPos
        q77 = .ThunkPos
        Call ParseTYPEDEF
        If ParseENUM() Then
            pvPushThunk ucsActResultClear
            Call ParseID
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
                    p98 = .BufPos
                    q98 = .ThunkPos
                    pvPushThunk ucsActResultClear
                    If VbPegParseEnumValue() Then
                        pvPushThunk ucsActVarSet, 4
                    Else
                        .BufPos = p98
                        .ThunkPos = q98
                        pvPushThunk ucsActResultClear
                        Call ParseEMPTY
                        pvPushThunk ucsActVarSet, 4
                    End If
                    Call ParseCOMMA
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
                            Call pvSetAdvance
                            VbPegParseEnumDecl = True
                            Exit Function
                        Else
                            .BufPos = p77
                            .ThunkPos = q77
                        End If
                    Else
                        .BufPos = p77
                        .ThunkPos = q77
                    End If
                Else
                    .BufPos = p77
                    .ThunkPos = q77
                End If
            Else
                .BufPos = p77
                .ThunkPos = q77
            End If
        Else
            .BufPos = p77
            .ThunkPos = q77
        End If
    End With
End Function

Public Function VbPegParseStructDecl() As Boolean
    Dim p121 As Long
    Dim q121 As Long
    Dim i143 As Long
    Dim p136 As Long
    Dim q136 As Long
    Dim p147 As Long
    Dim q147 As Long

    With ctx
        pvPushThunk ucsActVarAlloc, 4
        p121 = .BufPos
        q121 = .ThunkPos
        Call ParseTYPEDEF
        If ParseSTRUCT() Then
            pvPushThunk ucsActResultClear
            Call ParseID
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
                    If Not ParseSEMI() Then
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
                            Call pvSetAdvance
                            VbPegParseStructDecl = True
                            Exit Function
                        Else
                            .BufPos = p121
                            .ThunkPos = q121
                        End If
                    Else
                        .BufPos = p121
                        .ThunkPos = q121
                    End If
                Else
                    .BufPos = p121
                    .ThunkPos = q121
                End If
            Else
                .BufPos = p121
                .ThunkPos = q121
            End If
        Else
            .BufPos = p121
            .ThunkPos = q121
        End If
    End With
End Function

Public Function VbPegParseFunDecl() As Boolean
    Dim p158 As Long
    Dim q158 As Long
    Dim p168 As Long
    Dim q168 As Long

    With ctx
        pvPushThunk ucsActVarAlloc, 3
        p158 = .BufPos
        q158 = .ThunkPos
        Call VbPegParseLinkage
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
                            If ParseSEMI() Then
                                pvPushThunk ucsAct_1_FunDecl, .CaptureBegin, .CaptureEnd
                                pvPushThunk ucsActVarAlloc, -3
                                VbPegParseFunDecl = True
                                Exit Function
                            End If
                            If ParseLBRACE() Then
                                pvPushThunk ucsAct_1_FunDecl, .CaptureBegin, .CaptureEnd
                                pvPushThunk ucsActVarAlloc, -3
                                VbPegParseFunDecl = True
                                Exit Function
                            End If
                            .BufPos = p158
                            .ThunkPos = q158
                        Else
                            .BufPos = p158
                            .ThunkPos = q158
                        End If
                    Else
                        .BufPos = p158
                        .ThunkPos = q158
                    End If
                Else
                    .BufPos = p158
                    .ThunkPos = q158
                End If
            Else
                .BufPos = p158
                .ThunkPos = q158
            End If
        Else
            .BufPos = p158
            .ThunkPos = q158
        End If
    End With
End Function

Public Function VbPegParseSkipStmt() As Boolean
    Dim p191 As Long
    Dim q191 As Long
    Dim lCaptureBegin As Long
    Dim p189 As Long
    Dim lCaptureEnd As Long

    With ctx
        p191 = .BufPos
        q191 = .ThunkPos
        lCaptureBegin = .BufPos
        Do
            p189 = .BufPos
            If ParseNL() Then
                .BufPos = p189
                Exit Do
            End If
            If ParseSEMI() Then
                .BufPos = p189
                Exit Do
            End If
            If Not ParseBLOCKCOMMENT() Then
                If Not ParseLINECOMMENT() Then
                    If Not ParseWS() Then
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
        If ParseNL() Then
            Call Parse_
            lCaptureEnd = .BufPos
            .CaptureBegin = lCaptureBegin
            .CaptureEnd = lCaptureEnd
            pvPushThunk ucsAct_1_SkipStmt, lCaptureBegin, lCaptureEnd
            VbPegParseSkipStmt = True
            Exit Function
        End If
        If ParseSEMI() Then
            Call Parse_
            lCaptureEnd = .BufPos
            .CaptureBegin = lCaptureBegin
            .CaptureEnd = lCaptureEnd
            pvPushThunk ucsAct_1_SkipStmt, lCaptureBegin, lCaptureEnd
            VbPegParseSkipStmt = True
            Exit Function
        End If
        .BufPos = p191
        .ThunkPos = q191
    End With
End Function

Private Function ParseEOL() As Boolean
    With ctx
        If Not .BufPos < .BufSize Then
            ParseEOL = True
        End If
    End With
End Function

Private Function ParseTYPEDEF() As Boolean
    Dim p379 As Long

    With ctx
        p379 = .BufPos
        If pvMatchString("typedef", NORM_IGNORECASE) Then ' "typedef"i
            .BufPos = .BufPos + 7
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p379
            Case Else
                Call Parse_
                Call pvSetAdvance
                ParseTYPEDEF = True
            End Select
        End If
    End With
End Function

Public Function VbPegParseType() As Boolean
    Dim lCaptureBegin As Long
    Dim p204 As Long
    Dim q204 As Long
    Dim e206 As String
    Dim e208 As String
    Dim lCaptureEnd As Long
    Dim p212 As Long
    Dim q212 As Long
    Dim e211 As String
    Dim e214 As String

    With ctx
        lCaptureBegin = .BufPos
        p204 = .BufPos
        q204 = .ThunkPos
        If VbPegParseTypePrefix() Then
            If VbPegParseTypeBody() Then
                e206 = .LastExpected
                If ParseLPAREN() Then
                    .BufPos = p204
                    .ThunkPos = q204
                Else
                    .LastExpected = e206
                    e208 = .LastExpected
                    If ParseSEMI() Then
                        .BufPos = p204
                        .ThunkPos = q204
                    Else
                        .LastExpected = e208
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
                .BufPos = p204
                .ThunkPos = q204
            End If
        End If
        p212 = .BufPos
        q212 = .ThunkPos
        If VbPegParseTypeBody() Then
            e211 = .LastExpected
            If ParseLPAREN() Then
                .BufPos = p212
                .ThunkPos = q212
            Else
                .LastExpected = e211
                e214 = .LastExpected
                If ParseSEMI() Then
                    .BufPos = p212
                    .ThunkPos = q212
                Else
                    .LastExpected = e214
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
            .BufPos = p212
            .ThunkPos = q212
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
    Dim lCaptureEnd As Long

    With ctx
        lCaptureBegin = .BufPos
        p227 = .BufPos
        q227 = .ThunkPos
        If VbPegParseTypePrefix() Then
            If VbPegParseTypeBody() Then
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
        End If
        If VbPegParseTypeBody() Then
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
    Dim p449 As Long

    With ctx
        p449 = .BufPos
        If pvMatchString("LLVM_STDCALL") Then       ' "LLVM_STDCALL"
            .BufPos = .BufPos + 12
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p449
            Case Else
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
    Dim p280 As Long
    Dim q280 As Long
    Dim p287 As Long
    Dim q287 As Long

    With ctx
        pvPushThunk ucsActVarAlloc, 2
        p280 = .BufPos
        q280 = .ThunkPos
        pvPushThunk ucsActResultClear
        If VbPegParseParam() Then
            pvPushThunk ucsActVarSet, 1
            pvPushThunk ucsAct_1_Params, .CaptureBegin, .CaptureEnd
            Do
                p287 = .BufPos
                q287 = .ThunkPos
                If Not ParseCOMMA() Then
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
            Exit Function
        Else
            .BufPos = p280
            .ThunkPos = q280
        End If
    End With
End Function

Private Function ParseENUM() As Boolean
    Dim p454 As Long

    With ctx
        p454 = .BufPos
        If pvMatchString("enum") Then               ' "enum"
            .BufPos = .BufPos + 4
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p454
            Case Else
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
    Dim p313 As Long
    Dim q313 As Long
    Dim p323 As Long
    Dim q323 As Long

    With ctx
        pvPushThunk ucsActVarAlloc, 2
        p313 = .BufPos
        q313 = .ThunkPos
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
                Call pvSetAdvance
                VbPegParseEnumValue = True
                Exit Function
            Else
                .BufPos = p313
                .ThunkPos = q313
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
        Call pvSetAdvance
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
    Dim p459 As Long

    With ctx
        p459 = .BufPos
        If pvMatchString("struct") Then             ' "struct"
            .BufPos = .BufPos + 6
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p459
            Case Else
                Call Parse_
                Call pvSetAdvance
                ParseSTRUCT = True
            End Select
        End If
    End With
End Function

Public Function VbPegParseLinkage() As Boolean
    If ParseEXTERN() Then
        Call ParseINLINE
        VbPegParseLinkage = True
        Exit Function
    End If
    If ParseSTATIC() Then
        Call ParseINLINE
        VbPegParseLinkage = True
    End If
End Function

Private Function ParseNL() As Boolean
    Dim p518 As Long

    With ctx
        p518 = .BufPos
        If .BufData(.BufPos) = 13 Then              ' "\r"
            .BufPos = .BufPos + 1
        End If
        If .BufData(.BufPos) = 10 Then              ' "\n"
            .BufPos = .BufPos + 1
            Call ParsePREPRO
            Call pvSetAdvance
            ParseNL = True
            Exit Function
        Else
            .BufPos = p518
        End If
    End With
End Function

Private Function ParseBLOCKCOMMENT() As Boolean
    Dim p538 As Long

    With ctx
        p538 = .BufPos
        If .BufData(.BufPos) = 47 And .BufData(.BufPos + 1) = 42 Then ' "/*"
            .BufPos = .BufPos + 2
            Do
                If .BufData(.BufPos) = 42 And .BufData(.BufPos + 1) = 47 Then ' "*/"
                    Exit Do
                End If
                If .BufData(.BufPos) = 47 And .BufData(.BufPos + 1) = 42 Then ' "/*"
                    If Not ParseBLOCKCOMMENT() Then
                        If .BufPos < .BufSize Then
                            .BufPos = .BufPos + 1
                        Else
                            Exit Do
                        End If
                    End If
                Else
                    If .BufPos < .BufSize Then
                        .BufPos = .BufPos + 1
                    Else
                        Exit Do
                    End If
                End If
            Loop
            If .BufData(.BufPos) = 42 And .BufData(.BufPos + 1) = 47 Then ' "*/"
                .BufPos = .BufPos + 2
                Call pvSetAdvance
                ParseBLOCKCOMMENT = True
                Exit Function
            Else
                .BufPos = p538
            End If
        End If
    End With
End Function

Private Function ParseLINECOMMENT() As Boolean
    Dim p543 As Long

    With ctx
        p543 = .BufPos
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
                Exit Function
            Else
                .BufPos = p543
            End If
        End If
    End With
End Function

Private Function ParseWS() As Boolean
    Dim i514 As Long

    With ctx
        For i514 = 0 To LNG_MAXINT
            Select Case .BufData(.BufPos)
            Case 32, 9                              ' [ \t]
                .BufPos = .BufPos + 1
            Case Else
                If Not ParseNL() Then
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
    Do
        If Not ParseBLOCKCOMMENT() Then
            If Not ParseLINECOMMENT() Then
                If Not ParseWS() Then
                    Exit Do
                End If
            End If
        End If
    Loop
End Sub

Public Function VbPegParseTypePrefix() As Boolean
    If ParseCONST() Then
        VbPegParseTypePrefix = True
        Exit Function
    End If
    If ParseUNSIGNED() Then
        VbPegParseTypePrefix = True
        Exit Function
    End If
    If ParseSTRUCT() Then
        VbPegParseTypePrefix = True
        Exit Function
    End If
    If ParseENUM() Then
        VbPegParseTypePrefix = True
    End If
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
        End If
        If ParseCHAR() Then
            VbPegParseTypeBody = True
            Exit Function
        End If
        If ParseUNSIGNED() Then
            VbPegParseTypeBody = True
            Exit Function
        End If
        If ParseVOID() Then
            VbPegParseTypeBody = True
            Exit Function
        End If
        If ParseUINT_A_T() Then
            VbPegParseTypeBody = True
            Exit Function
        End If
        If ParseUINT_B_T() Then
            VbPegParseTypeBody = True
            Exit Function
        End If
        If ParseUINT_C_T() Then
            VbPegParseTypeBody = True
            Exit Function
        End If
        If ParseUINTPTR_T() Then
            VbPegParseTypeBody = True
            Exit Function
        End If
        If ParseSIZE_T() Then
            VbPegParseTypeBody = True
            Exit Function
        End If
        If ParseDOUBLE() Then
            VbPegParseTypeBody = True
            Exit Function
        End If
        If ParseLONG_LONG() Then
            VbPegParseTypeBody = True
            Exit Function
        End If
        If ParseBOOL() Then
            VbPegParseTypeBody = True
            Exit Function
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

    With ctx
        Do
            p269 = .BufPos
            Call ParseCONST
            If Not ParseSTAR() Then
                .BufPos = p269
                Exit Do
            End If
        Loop
    End With
End Sub

Private Function ParseCONST() As Boolean
    Dim p399 As Long

    With ctx
        p399 = .BufPos
        If pvMatchString("const") Then              ' "const"
            .BufPos = .BufPos + 5
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p399
            Case Else
                Call Parse_
                Call pvSetAdvance
                ParseCONST = True
            End Select
        End If
    End With
End Function

Private Function ParseUNSIGNED() As Boolean
    Dim p394 As Long

    With ctx
        p394 = .BufPos
        If pvMatchString("unsigned") Then           ' "unsigned"
            .BufPos = .BufPos + 8
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p394
            Case Else
                Call Parse_
                Call pvSetAdvance
                ParseUNSIGNED = True
            End Select
        End If
    End With
End Function

Private Function ParseINT() As Boolean
    Dim p384 As Long

    With ctx
        p384 = .BufPos
        If pvMatchString("int") Then                ' "int"
            .BufPos = .BufPos + 3
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p384
            Case Else
                Call Parse_
                Call pvSetAdvance
                ParseINT = True
            End Select
        End If
    End With
End Function

Private Function ParseCHAR() As Boolean
    Dim p389 As Long

    With ctx
        p389 = .BufPos
        If pvMatchString("char") Then               ' "char"
            .BufPos = .BufPos + 4
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p389
            Case Else
                Call Parse_
                Call pvSetAdvance
                ParseCHAR = True
            End Select
        End If
    End With
End Function

Private Function ParseVOID() As Boolean
    Dim p404 As Long

    With ctx
        p404 = .BufPos
        If pvMatchString("void") Then               ' "void"
            .BufPos = .BufPos + 4
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p404
            Case Else
                Call Parse_
                Call pvSetAdvance
                ParseVOID = True
            End Select
        End If
    End With
End Function

Private Function ParseUINT_A_T() As Boolean
    Dim p409 As Long

    With ctx
        p409 = .BufPos
        If pvMatchString("uint8_t") Then            ' "uint8_t"
            .BufPos = .BufPos + 7
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p409
            Case Else
                Call Parse_
                Call pvSetAdvance
                ParseUINT_A_T = True
            End Select
        End If
    End With
End Function

Private Function ParseUINT_B_T() As Boolean
    Dim p414 As Long

    With ctx
        p414 = .BufPos
        If pvMatchString("uint32_t") Then           ' "uint32_t"
            .BufPos = .BufPos + 8
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p414
            Case Else
                Call Parse_
                Call pvSetAdvance
                ParseUINT_B_T = True
            End Select
        End If
    End With
End Function

Private Function ParseUINT_C_T() As Boolean
    Dim p419 As Long

    With ctx
        p419 = .BufPos
        If pvMatchString("uint64_t") Then           ' "uint64_t"
            .BufPos = .BufPos + 8
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p419
            Case Else
                Call Parse_
                Call pvSetAdvance
                ParseUINT_C_T = True
            End Select
        End If
    End With
End Function

Private Function ParseUINTPTR_T() As Boolean
    Dim p424 As Long

    With ctx
        p424 = .BufPos
        If pvMatchString("uintptr_t") Then          ' "uintptr_t"
            .BufPos = .BufPos + 9
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p424
            Case Else
                Call Parse_
                Call pvSetAdvance
                ParseUINTPTR_T = True
            End Select
        End If
    End With
End Function

Private Function ParseSIZE_T() As Boolean
    Dim p429 As Long

    With ctx
        p429 = .BufPos
        If pvMatchString("size_t") Then             ' "size_t"
            .BufPos = .BufPos + 6
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p429
            Case Else
                Call Parse_
                Call pvSetAdvance
                ParseSIZE_T = True
            End Select
        End If
    End With
End Function

Private Function ParseDOUBLE() As Boolean
    Dim p434 As Long

    With ctx
        p434 = .BufPos
        If pvMatchString("double") Then             ' "double"
            .BufPos = .BufPos + 6
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p434
            Case Else
                Call Parse_
                Call pvSetAdvance
                ParseDOUBLE = True
            End Select
        End If
    End With
End Function

Private Function ParseLONG_LONG() As Boolean
    Dim p439 As Long

    With ctx
        p439 = .BufPos
        If pvMatchString("long long") Then          ' "long long"
            .BufPos = .BufPos + 9
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p439
            Case Else
                Call Parse_
                Call pvSetAdvance
                ParseLONG_LONG = True
            End Select
        End If
    End With
End Function

Private Function ParseBOOL() As Boolean
    Dim p444 As Long

    With ctx
        p444 = .BufPos
        If pvMatchString("bool") Then               ' "bool"
            .BufPos = .BufPos + 4
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p444
            Case Else
                Call Parse_
                Call pvSetAdvance
                ParseBOOL = True
            End Select
        End If
    End With
End Function

Public Function VbPegParseRefType() As Boolean
    Dim p274 As Long
    Dim q274 As Long
    Dim e272 As String

    With ctx
        p274 = .BufPos
        q274 = .ThunkPos
        e272 = .LastExpected
        If ParseLLVM_STDCALL() Then
            .BufPos = p274
            .ThunkPos = q274
        Else
            .LastExpected = e272
            If ParseID() Then
                If IsRefType(Mid$(.Contents, .CaptureBegin + 1, .CaptureEnd - .CaptureBegin)) Then
                    VbPegParseRefType = True
                    Exit Function
                Else
                    .BufPos = p274
                    .ThunkPos = q274
                End If
            Else
                .BufPos = p274
                .ThunkPos = q274
            End If
        End If
    End With
End Function

Public Function VbPegParseParam() As Boolean
    Dim p294 As Long
    Dim q294 As Long

    With ctx
        pvPushThunk ucsActVarAlloc, 3
        p294 = .BufPos
        q294 = .ThunkPos
        pvPushThunk ucsActResultClear
        If VbPegParseType() Then
            pvPushThunk ucsActVarSet, 1
            pvPushThunk ucsActResultClear
            Call ParseID
            pvPushThunk ucsActVarSet, 2
            pvPushThunk ucsActResultClear
            Call VbPegParseArraySuffix
            pvPushThunk ucsActVarSet, 3
            pvPushThunk ucsAct_1_Param, .CaptureBegin, .CaptureEnd
            pvPushThunk ucsActVarAlloc, -3
            VbPegParseParam = True
            Exit Function
        Else
            .BufPos = p294
            .ThunkPos = q294
        End If
    End With
End Function

Public Function VbPegParseArraySuffix() As Boolean
    Dim p344 As Long
    Dim q344 As Long
    Dim lCaptureBegin As Long
    Dim p349 As Long
    Dim e347 As String
    Dim lCaptureEnd As Long

    With ctx
        p344 = .BufPos
        q344 = .ThunkPos
        lCaptureBegin = .BufPos
        If ParseLBRACKET() Then
            Do
                p349 = .BufPos
                e347 = .LastExpected
                If ParseRBRACKET() Then
                    .BufPos = p349
                    Exit Do
                Else
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
                Exit Function
            Else
                .BufPos = p344
                .ThunkPos = q344
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
    Dim lCaptureEnd As Long

    With ctx
        lCaptureBegin = .BufPos
        For i336 = 0 To LNG_MAXINT
            p335 = .BufPos
            If ParseBLOCKCOMMENT() Then
                .BufPos = p335
                Exit For
            End If
            If ParseLINECOMMENT() Then
                .BufPos = p335
                Exit For
            End If
            If ParseWS() Then
                .BufPos = p335
                Exit For
            End If
            Select Case .BufData(.BufPos)
            Case 44, 125                            ' [,}]
                .BufPos = .BufPos + 1
                .BufPos = p335
                Exit For
            End Select
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
    Dim p464 As Long

    With ctx
        p464 = .BufPos
        If pvMatchString("extern") Then             ' "extern"
            .BufPos = .BufPos + 6
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p464
            Case Else
                Call Parse_
                Call pvSetAdvance
                ParseEXTERN = True
            End Select
        End If
    End With
End Function

Private Function ParseSTATIC() As Boolean
    Dim p469 As Long

    With ctx
        p469 = .BufPos
        If pvMatchString("static") Then             ' "static"
            .BufPos = .BufPos + 6
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p469
            Case Else
                Call Parse_
                Call pvSetAdvance
                ParseSTATIC = True
            End Select
        End If
    End With
End Function

Private Function ParseINLINE() As Boolean
    Dim p474 As Long

    With ctx
        p474 = .BufPos
        If pvMatchString("inline") Then             ' "inline"
            .BufPos = .BufPos + 6
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p474
            Case Else
                Call Parse_
                Call pvSetAdvance
                ParseINLINE = True
            End Select
        End If
    End With
End Function

Private Function ParsePREPRO() As Boolean
    Dim p525 As Long

    With ctx
        p525 = .BufPos
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
                Exit Function
            Else
                .BufPos = p525
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

