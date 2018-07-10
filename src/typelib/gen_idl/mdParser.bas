Attribute VB_Name = "mdParser"
' Auto-generated on 10.7.2018 19:20:19
Option Explicit
DefObj A-Z

'=========================================================================
' API
'=========================================================================

Private Declare Sub CopyMemory Lib "kernel32" Alias "RtlMoveMemory" (Destination As Any, Source As Any, ByVal Length As Long)
Private Declare Function RtlCompareMemory Lib "ntdll" (Source1 As Any, Source2 As Any, ByVal Length As Long) As Long

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
    ucsAct_1_EMPTY
    ucsAct_4_EnumValue
    ucsAct_3_EnumValue
    ucsAct_2_EnumValue
    ucsAct_1_EnumValue
    ucsAct_1_Param
    ucsAct_1_ArraySuffix
    ucsAct_1_EnumValueToken
    ucsActVarAlloc = -1
    ucsActVarSet = -2
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
    LastError           As String
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

Property Get VbPegParserVersion() As String
    VbPegParserVersion = "10.7.2018 19:20:19"
End Property

'=========================================================================
' Methods
'=========================================================================

Public Function VbPegMatch(sSubject As String, Optional ByVal StartPos As Long, Optional UserData As Variant, Optional Result As Variant) As Long
    If VbPegBeginMatch(sSubject, StartPos, UserData) Then
        If VbPegParseStmt() Then
            VbPegMatch = VbPegEndMatch(UserData, Result)
        End If
    End If
End Function

Public Function VbPegBeginMatch(sSubject As String, Optional ByVal StartPos As Long, Optional UserData As Variant) As Boolean
    With ctx
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

Public Function VbPegEndMatch(Optional UserData As Variant, Optional Result As Variant) As Long
    Dim lIdx            As Long
    Dim uEmpty          As UcsParserType
    
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
        If IsObject(.UserData) Then
            Set UserData = .UserData
        Else
            UserData = .UserData
        End If
        VbPegEndMatch = .BufPos + 1
    End With
    uEmpty.LastError = ctx.LastError
    ctx = uEmpty
End Function

Private Sub pvPushAction(ByVal eAction As UcsParserActionsEnum)
    pvPushThunk eAction, ctx.CaptureBegin, ctx.CaptureEnd
End Sub

Private Sub pvPushThunk(ByVal eAction As UcsParserActionsEnum, ByVal lBegin As Long, Optional ByVal lEnd As Long)
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

Private Function pvMatchString(sText As String) As Boolean
    With ctx
        If .BufPos + Len(sText) <= .BufSize Then
            pvMatchString = RtlCompareMemory(.BufData(.BufPos), ByVal StrPtr(sText), LenB(sText)) = LenB(sText)
        End If
    End With
End Function

'= generated functions ===================================================

Public Function VbPegParseStmt() As Boolean
    Dim p6 As Long
    Dim q6 As Long
    Dim p12 As Long
    Dim q12 As Long

    With ctx
        pvPushThunk ucsActVarAlloc, 2
        p6 = .BufPos
        q6 = .ThunkPos
        Call Parse_
        pvPushThunk ucsActVarSet, 1
        pvPushAction ucsAct_1_Stmt
        Do
            p12 = .BufPos
            q12 = .ThunkPos
            If VbPegParseTypedefDecl() Then
                pvPushThunk ucsActVarSet, 2
                GoTo L1
            Else
                .BufPos = p12
                .ThunkPos = q12
                If VbPegParseTypedefCallback() Then
                    pvPushThunk ucsActVarSet, 2
                    GoTo L1
                Else
                    .BufPos = p12
                    .ThunkPos = q12
                    If VbPegParseEnumDecl() Then
                        pvPushThunk ucsActVarSet, 2
                        GoTo L1
                    Else
                        .BufPos = p12
                        .ThunkPos = q12
                        If VbPegParseStructDecl() Then
                            pvPushThunk ucsActVarSet, 2
                            GoTo L1
                        Else
                            .BufPos = p12
                            .ThunkPos = q12
                            If VbPegParseFunDecl() Then
                                pvPushThunk ucsActVarSet, 2
                                GoTo L1
                            Else
                                .BufPos = p12
                                .ThunkPos = q12
                                If VbPegParseSkipStmt() Then
                                    pvPushThunk ucsActVarSet, 2
                                    GoTo L1
                                Else
                                    .BufPos = p12
                                    .ThunkPos = q12
                                    Exit Do
                                End If
                            End If
                        End If
                    End If
                End If
            End If
L1:
            pvPushAction ucsAct_2_Stmt
        Loop
        If ParseEOL() Then
            pvPushAction ucsAct_3_Stmt
            pvPushThunk ucsActVarAlloc, -2
            VbPegParseStmt = True
            Exit Function
        Else
            .BufPos = p6
            .ThunkPos = q6
        End If
    End With
End Function

Private Sub Parse_()
    Do
        If ParseBLOCKCOMMENT() Then
            GoTo L2
        Else
            If ParseLINECOMMENT() Then
                GoTo L2
            Else
                If ParseWS() Then
                    GoTo L2
                Else
                    Exit Do
                End If
            End If
        End If
L2:
    Loop
End Sub

Public Function VbPegParseTypedefDecl() As Boolean
    Dim p32 As Long
    Dim q32 As Long

    With ctx
        pvPushThunk ucsActVarAlloc, 2
        p32 = .BufPos
        q32 = .ThunkPos
        If ParseTYPEDEF() Then
            If VbPegParseType() Then
                pvPushThunk ucsActVarSet, 1
                If ParseID() Then
                    pvPushThunk ucsActVarSet, 2
                    If ParseSEMI() Then
                        pvPushAction ucsAct_1_TypedefDecl
                        pvPushThunk ucsActVarAlloc, -2
                        VbPegParseTypedefDecl = True
                        Exit Function
                    Else
                        .BufPos = p32
                        .ThunkPos = q32
                    End If
                Else
                    .BufPos = p32
                    .ThunkPos = q32
                End If
            Else
                .BufPos = p32
                .ThunkPos = q32
            End If
        End If
    End With
End Function

Public Function VbPegParseTypedefCallback() As Boolean
    Dim p43 As Long
    Dim q43 As Long

    With ctx
        pvPushThunk ucsActVarAlloc, 3
        p43 = .BufPos
        q43 = .ThunkPos
        If ParseTYPEDEF() Then
            If VbPegParseTypeUnlimited() Then
                pvPushThunk ucsActVarSet, 1
                If ParseLPAREN() Then
                    If ParseLLVM_STDCALL() Then
                        If ParseSTAR() Then
                            If ParseID() Then
                                pvPushThunk ucsActVarSet, 2
                                If ParseRPAREN() Then
                                    If ParseLPAREN() Then
                                        If VbPegParseParams() Then
                                            pvPushThunk ucsActVarSet, 3
                                        End If
                                        If ParseRPAREN() Then
                                            If ParseSEMI() Then
                                                pvPushAction ucsAct_1_TypedefCallback
                                                pvPushThunk ucsActVarAlloc, -3
                                                VbPegParseTypedefCallback = True
                                                Exit Function
                                            Else
                                                .BufPos = p43
                                                .ThunkPos = q43
                                            End If
                                        Else
                                            .BufPos = p43
                                            .ThunkPos = q43
                                        End If
                                    Else
                                        .BufPos = p43
                                        .ThunkPos = q43
                                    End If
                                Else
                                    .BufPos = p43
                                    .ThunkPos = q43
                                End If
                            Else
                                .BufPos = p43
                                .ThunkPos = q43
                            End If
                        Else
                            .BufPos = p43
                            .ThunkPos = q43
                        End If
                    Else
                        .BufPos = p43
                        .ThunkPos = q43
                    End If
                Else
                    .BufPos = p43
                    .ThunkPos = q43
                End If
            Else
                .BufPos = p43
                .ThunkPos = q43
            End If
        End If
    End With
End Function

Public Function VbPegParseEnumDecl() As Boolean
    Dim p65 As Long
    Dim q65 As Long
    Dim p74 As Long
    Dim q74 As Long
    Dim i92 As Long
    Dim p86 As Long
    Dim q86 As Long

    With ctx
        pvPushThunk ucsActVarAlloc, 4
        p65 = .BufPos
        q65 = .ThunkPos
        Call Parse_
        pvPushThunk ucsActVarSet, 1
        pvPushAction ucsAct_1_EnumDecl
        Call ParseTYPEDEF
        If ParseENUM() Then
            p74 = .BufPos
            q74 = .ThunkPos
            If ParseID() Then
                pvPushThunk ucsActVarSet, 2
                If ParseLBRACE() Then
                    For i92 = 0 To LNG_MAXINT
                        If ParseID() Then
                            pvPushThunk ucsActVarSet, 3
                        Else
                            Exit For
                        End If
                        p86 = .BufPos
                        q86 = .ThunkPos
                        If ParseEQ() Then
                            If VbPegParseEnumValue() Then
                                pvPushThunk ucsActVarSet, 4
                                GoTo L8
                            Else
                                .BufPos = p86
                                .ThunkPos = q86
                                Call ParseEMPTY
                                pvPushThunk ucsActVarSet, 4
                            End If
                        Else
                            Call ParseEMPTY
                            pvPushThunk ucsActVarSet, 4
                        End If
L8:
                        Call ParseCOMMA
                        pvPushAction ucsAct_2_EnumDecl
                    Next
                    If i92 <> 0 Then
                        If ParseRBRACE() Then
                            If ParseID() Then
                                pvPushThunk ucsActVarSet, 2
                            End If
                            If ParseSEMI() Then
                                pvPushAction ucsAct_3_EnumDecl
                                pvPushThunk ucsActVarAlloc, -4
                                VbPegParseEnumDecl = True
                                Exit Function
                            Else
                                .BufPos = p65
                                .ThunkPos = q65
                            End If
                        Else
                            .BufPos = p65
                            .ThunkPos = q65
                        End If
                    Else
                        .BufPos = p65
                        .ThunkPos = q65
                    End If
                Else
                    .BufPos = p65
                    .ThunkPos = q65
                End If
                GoTo L7
            Else
                .BufPos = p74
                .ThunkPos = q74
            End If
            Call ParseEMPTY
            pvPushThunk ucsActVarSet, 2
            If ParseLBRACE() Then
                For i92 = 0 To LNG_MAXINT
                    If ParseID() Then
                        pvPushThunk ucsActVarSet, 3
                    Else
                        Exit For
                    End If
                    p86 = .BufPos
                    q86 = .ThunkPos
                    If ParseEQ() Then
                        If VbPegParseEnumValue() Then
                            pvPushThunk ucsActVarSet, 4
                            GoTo L11
                        Else
                            .BufPos = p86
                            .ThunkPos = q86
                            Call ParseEMPTY
                            pvPushThunk ucsActVarSet, 4
                        End If
                    Else
                        Call ParseEMPTY
                        pvPushThunk ucsActVarSet, 4
                    End If
L11:
                    Call ParseCOMMA
                    pvPushAction ucsAct_2_EnumDecl
                Next
                If i92 <> 0 Then
                    If ParseRBRACE() Then
                        If ParseID() Then
                            pvPushThunk ucsActVarSet, 2
                        End If
                        If ParseSEMI() Then
                            pvPushAction ucsAct_3_EnumDecl
                            pvPushThunk ucsActVarAlloc, -4
                            VbPegParseEnumDecl = True
                            Exit Function
                        Else
                            .BufPos = p65
                            .ThunkPos = q65
                        End If
                    Else
                        .BufPos = p65
                        .ThunkPos = q65
                    End If
                Else
                    .BufPos = p65
                    .ThunkPos = q65
                End If
            Else
                .BufPos = p65
                .ThunkPos = q65
            End If
L7:
        Else
            .BufPos = p65
            .ThunkPos = q65
        End If
    End With
End Function

Public Function VbPegParseStructDecl() As Boolean
    Dim p102 As Long
    Dim q102 As Long
    Dim p110 As Long
    Dim q110 As Long
    Dim i119 As Long
    Dim p116 As Long
    Dim q116 As Long

    With ctx
        pvPushThunk ucsActVarAlloc, 4
        p102 = .BufPos
        q102 = .ThunkPos
        Call Parse_
        pvPushThunk ucsActVarSet, 1
        pvPushAction ucsAct_1_StructDecl
        Call ParseTYPEDEF
        If ParseSTRUCT() Then
            p110 = .BufPos
            q110 = .ThunkPos
            If ParseID() Then
                pvPushThunk ucsActVarSet, 2
                If ParseLBRACE() Then
                    For i119 = 0 To LNG_MAXINT
                        p116 = .BufPos
                        q116 = .ThunkPos
                        If VbPegParseType() Then
                            pvPushThunk ucsActVarSet, 3
                        Else
                            Exit For
                        End If
                        If ParseID() Then
                            pvPushThunk ucsActVarSet, 4
                        Else
                            .BufPos = p116
                            .ThunkPos = q116
                            Exit For
                        End If
                        If Not ParseSEMI() Then
                            .BufPos = p116
                            .ThunkPos = q116
                            Exit For
                        End If
                        pvPushAction ucsAct_2_StructDecl
                    Next
                    If i119 <> 0 Then
                        If ParseRBRACE() Then
                            If ParseID() Then
                                pvPushThunk ucsActVarSet, 2
                            End If
                            If ParseSEMI() Then
                                pvPushAction ucsAct_3_StructDecl
                                pvPushThunk ucsActVarAlloc, -4
                                VbPegParseStructDecl = True
                                Exit Function
                            Else
                                .BufPos = p102
                                .ThunkPos = q102
                            End If
                        Else
                            .BufPos = p102
                            .ThunkPos = q102
                        End If
                    Else
                        .BufPos = p102
                        .ThunkPos = q102
                    End If
                Else
                    .BufPos = p102
                    .ThunkPos = q102
                End If
                GoTo L16
            Else
                .BufPos = p110
                .ThunkPos = q110
            End If
            Call ParseEMPTY
            pvPushThunk ucsActVarSet, 2
            If ParseLBRACE() Then
                For i119 = 0 To LNG_MAXINT
                    p116 = .BufPos
                    q116 = .ThunkPos
                    If VbPegParseType() Then
                        pvPushThunk ucsActVarSet, 3
                    Else
                        Exit For
                    End If
                    If ParseID() Then
                        pvPushThunk ucsActVarSet, 4
                    Else
                        .BufPos = p116
                        .ThunkPos = q116
                        Exit For
                    End If
                    If Not ParseSEMI() Then
                        .BufPos = p116
                        .ThunkPos = q116
                        Exit For
                    End If
                    pvPushAction ucsAct_2_StructDecl
                Next
                If i119 <> 0 Then
                    If ParseRBRACE() Then
                        If ParseID() Then
                            pvPushThunk ucsActVarSet, 2
                        End If
                        If ParseSEMI() Then
                            pvPushAction ucsAct_3_StructDecl
                            pvPushThunk ucsActVarAlloc, -4
                            VbPegParseStructDecl = True
                            Exit Function
                        Else
                            .BufPos = p102
                            .ThunkPos = q102
                        End If
                    Else
                        .BufPos = p102
                        .ThunkPos = q102
                    End If
                Else
                    .BufPos = p102
                    .ThunkPos = q102
                End If
            Else
                .BufPos = p102
                .ThunkPos = q102
            End If
L16:
        Else
            .BufPos = p102
            .ThunkPos = q102
        End If
    End With
End Function

Public Function VbPegParseFunDecl() As Boolean
    Dim p130 As Long
    Dim q130 As Long

    With ctx
        pvPushThunk ucsActVarAlloc, 3
        p130 = .BufPos
        q130 = .ThunkPos
        Call VbPegParseLinkage
        If VbPegParseType() Then
            pvPushThunk ucsActVarSet, 1
            If ParseLLVM_STDCALL() Then
                If ParseID() Then
                    pvPushThunk ucsActVarSet, 2
                    If ParseLPAREN() Then
                        If VbPegParseParams() Then
                            pvPushThunk ucsActVarSet, 3
                        End If
                        If ParseRPAREN() Then
                            If ParseSEMI() Then
                                pvPushAction ucsAct_1_FunDecl
                                pvPushThunk ucsActVarAlloc, -3
                                VbPegParseFunDecl = True
                                Exit Function
                            End If
                            If ParseLBRACE() Then
                                pvPushAction ucsAct_1_FunDecl
                                pvPushThunk ucsActVarAlloc, -3
                                VbPegParseFunDecl = True
                                Exit Function
                            End If
                            .BufPos = p130
                            .ThunkPos = q130
                        Else
                            .BufPos = p130
                            .ThunkPos = q130
                        End If
                    Else
                        .BufPos = p130
                        .ThunkPos = q130
                    End If
                Else
                    .BufPos = p130
                    .ThunkPos = q130
                End If
            Else
                .BufPos = p130
                .ThunkPos = q130
            End If
        Else
            .BufPos = p130
            .ThunkPos = q130
        End If
    End With
End Function

Public Function VbPegParseSkipStmt() As Boolean
    Dim p159 As Long
    Dim q159 As Long
    Dim lCaptureBegin As Long
    Dim p157 As Long
    Dim lCaptureEnd As Long

    With ctx
        p159 = .BufPos
        q159 = .ThunkPos
        lCaptureBegin = .BufPos
        Do
            p157 = .BufPos
            If ParseNL() Then
                .BufPos = p157
                Exit Do
            End If
            If ParseSEMI() Then
                .BufPos = p157
                Exit Do
            End If
            If ParseBLOCKCOMMENT() Then
                GoTo L22
            Else
                If ParseLINECOMMENT() Then
                    GoTo L22
                Else
                    If ParseWS() Then
                        GoTo L22
                    Else
                        If .BufPos < .BufSize Then
                            .BufPos = .BufPos + 1
                            GoTo L22
                        Else
                            .BufPos = p157
                            Exit Do
                        End If
                    End If
                End If
            End If
L22:
        Loop
        If ParseNL() Then
            Call Parse_
            lCaptureEnd = .BufPos
            .CaptureBegin = lCaptureBegin
            .CaptureEnd = lCaptureEnd
            pvPushAction ucsAct_1_SkipStmt
            VbPegParseSkipStmt = True
            Exit Function
        End If
        If ParseSEMI() Then
            Call Parse_
            lCaptureEnd = .BufPos
            .CaptureBegin = lCaptureBegin
            .CaptureEnd = lCaptureEnd
            pvPushAction ucsAct_1_SkipStmt
            VbPegParseSkipStmt = True
            Exit Function
        End If
        .BufPos = p159
        .ThunkPos = q159
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
    Dim p333 As Long

    With ctx
        p333 = .BufPos
        If pvMatchString("typedef") Then            ' "typedef"
            .BufPos = .BufPos + 7
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p333
            Case Else
                Call Parse_
                ParseTYPEDEF = True
            End Select
        End If
    End With
End Function

Public Function VbPegParseType() As Boolean
    Dim lCaptureBegin As Long
    Dim p171 As Long
    Dim q171 As Long
    Dim lCaptureEnd As Long
    Dim p179 As Long
    Dim q179 As Long

    With ctx
        lCaptureBegin = .BufPos
        p171 = .BufPos
        q171 = .ThunkPos
        If VbPegParseTypePrefix() Then
            If VbPegParseTypeBody() Then
                If ParseLPAREN() Then
                    .BufPos = p171
                    .ThunkPos = q171
                Else
                    If ParseSEMI() Then
                        .BufPos = p171
                        .ThunkPos = q171
                    Else
                        Call VbPegParseTypeSuffix
                        lCaptureEnd = .BufPos
                        .CaptureBegin = lCaptureBegin
                        .CaptureEnd = lCaptureEnd
                        pvPushAction ucsAct_1_Type
                        VbPegParseType = True
                        Exit Function
                    End If
                End If
            Else
                .BufPos = p171
                .ThunkPos = q171
            End If
        End If
        p179 = .BufPos
        q179 = .ThunkPos
        If VbPegParseTypeBody() Then
            If ParseLPAREN() Then
                .BufPos = p179
                .ThunkPos = q179
            Else
                If ParseSEMI() Then
                    .BufPos = p179
                    .ThunkPos = q179
                Else
                    Call VbPegParseTypeSuffix
                    lCaptureEnd = .BufPos
                    .CaptureBegin = lCaptureBegin
                    .CaptureEnd = lCaptureEnd
                    pvPushAction ucsAct_1_Type
                    VbPegParseType = True
                End If
            End If
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
            pvPushAction ucsAct_1_ID
            ParseID = True
        End Select
    End With
End Function

Private Function ParseSEMI() As Boolean
    With ctx
        If .BufData(.BufPos) = 59 Then              ' ";"
            .BufPos = .BufPos + 1
            Call Parse_
            ParseSEMI = True
        End If
    End With
End Function

Public Function VbPegParseTypeUnlimited() As Boolean
    Dim lCaptureBegin As Long
    Dim p194 As Long
    Dim q194 As Long
    Dim lCaptureEnd As Long

    With ctx
        lCaptureBegin = .BufPos
        p194 = .BufPos
        q194 = .ThunkPos
        If VbPegParseTypePrefix() Then
            If VbPegParseTypeBody() Then
                Call VbPegParseTypeSuffix
                lCaptureEnd = .BufPos
                .CaptureBegin = lCaptureBegin
                .CaptureEnd = lCaptureEnd
                pvPushAction ucsAct_1_TypeUnlimited
                VbPegParseTypeUnlimited = True
                Exit Function
            Else
                .BufPos = p194
                .ThunkPos = q194
            End If
        End If
        If VbPegParseTypeBody() Then
            Call VbPegParseTypeSuffix
            lCaptureEnd = .BufPos
            .CaptureBegin = lCaptureBegin
            .CaptureEnd = lCaptureEnd
            pvPushAction ucsAct_1_TypeUnlimited
            VbPegParseTypeUnlimited = True
            Exit Function
        Else
            .BufPos = p194
            .ThunkPos = q194
        End If
    End With
End Function

Private Function ParseLPAREN() As Boolean
    With ctx
        If .BufData(.BufPos) = 40 Then              ' "("
            .BufPos = .BufPos + 1
            Call Parse_
            ParseLPAREN = True
        End If
    End With
End Function

Private Function ParseLLVM_STDCALL() As Boolean
    Dim p403 As Long

    With ctx
        p403 = .BufPos
        If pvMatchString("LLVM_STDCALL") Then       ' "LLVM_STDCALL"
            .BufPos = .BufPos + 12
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p403
            Case Else
                Call Parse_
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
            ParseSTAR = True
        End If
    End With
End Function

Private Function ParseRPAREN() As Boolean
    With ctx
        If .BufData(.BufPos) = 41 Then              ' ")"
            .BufPos = .BufPos + 1
            Call Parse_
            ParseRPAREN = True
        End If
    End With
End Function

Public Function VbPegParseParams() As Boolean
    Dim p251 As Long
    Dim q251 As Long

    With ctx
        pvPushThunk ucsActVarAlloc, 2
        If VbPegParseParam() Then
            pvPushThunk ucsActVarSet, 1
            pvPushAction ucsAct_1_Params
            Do
                p251 = .BufPos
                q251 = .ThunkPos
                If Not ParseCOMMA() Then
                    Exit Do
                End If
                If VbPegParseParam() Then
                    pvPushThunk ucsActVarSet, 2
                Else
                    .BufPos = p251
                    .ThunkPos = q251
                    Exit Do
                End If
                pvPushAction ucsAct_2_Params
            Loop
            pvPushAction ucsAct_3_Params
            pvPushThunk ucsActVarAlloc, -2
            VbPegParseParams = True
        End If
    End With
End Function

Private Function ParseENUM() As Boolean
    Dim p408 As Long

    With ctx
        p408 = .BufPos
        If pvMatchString("enum") Then               ' "enum"
            .BufPos = .BufPos + 4
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p408
            Case Else
                Call Parse_
                ParseENUM = True
            End Select
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
        pvPushAction ucsAct_1_EMPTY
    End With
End Sub

Private Function ParseLBRACE() As Boolean
    With ctx
        If .BufData(.BufPos) = 123 Then             ' "{"
            .BufPos = .BufPos + 1
            Call Parse_
            ParseLBRACE = True
        End If
    End With
End Function

Private Function ParseEQ() As Boolean
    With ctx
        If .BufData(.BufPos) = 61 Then              ' "="
            .BufPos = .BufPos + 1
            Call Parse_
            ParseEQ = True
        End If
    End With
End Function

Public Function VbPegParseEnumValue() As Boolean
    Dim p271 As Long
    Dim q271 As Long

    With ctx
        pvPushThunk ucsActVarAlloc, 2
        p271 = .BufPos
        q271 = .ThunkPos
        Call Parse_
        pvPushThunk ucsActVarSet, 1
        pvPushAction ucsAct_1_EnumValue
        If VbPegParseEnumValueToken() Then
            pvPushThunk ucsActVarSet, 2
            pvPushAction ucsAct_2_EnumValue
            Do
                If VbPegParseEnumValueToken() Then
                    pvPushThunk ucsActVarSet, 2
                Else
                    Exit Do
                End If
                pvPushAction ucsAct_3_EnumValue
            Loop
            pvPushAction ucsAct_4_EnumValue
            pvPushThunk ucsActVarAlloc, -2
            VbPegParseEnumValue = True
            Exit Function
        Else
            .BufPos = p271
            .ThunkPos = q271
        End If
    End With
End Function

Private Function ParseCOMMA() As Boolean
    With ctx
        If .BufData(.BufPos) = 44 Then              ' ","
            .BufPos = .BufPos + 1
            Call Parse_
            ParseCOMMA = True
        End If
    End With
End Function

Private Function ParseRBRACE() As Boolean
    With ctx
        If .BufData(.BufPos) = 125 Then             ' "}"
            .BufPos = .BufPos + 1
            Call Parse_
            ParseRBRACE = True
        End If
    End With
End Function

Private Function ParseSTRUCT() As Boolean
    Dim p413 As Long

    With ctx
        p413 = .BufPos
        If pvMatchString("struct") Then             ' "struct"
            .BufPos = .BufPos + 6
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p413
            Case Else
                Call Parse_
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
    Dim p472 As Long

    With ctx
        p472 = .BufPos
        If .BufData(.BufPos) = 13 Then              ' "\r"
            .BufPos = .BufPos + 1
        End If
        If .BufData(.BufPos) = 10 Then              ' "\n"
            .BufPos = .BufPos + 1
            Call ParsePREPRO
            ParseNL = True
            Exit Function
        Else
            .BufPos = p472
        End If
    End With
End Function

Private Function ParseBLOCKCOMMENT() As Boolean
    Dim p492 As Long

    With ctx
        p492 = .BufPos
        If .BufData(.BufPos) = 47 And .BufData(.BufPos + 1) = 42 Then ' "/*"
            .BufPos = .BufPos + 2
            Do
                If .BufData(.BufPos) = 42 And .BufData(.BufPos + 1) = 47 Then ' "*/"
                    Exit Do
                End If
                If .BufData(.BufPos) = 47 And .BufData(.BufPos + 1) = 42 Then ' "/*"
                    If ParseBLOCKCOMMENT() Then
                        GoTo L35
                    Else
                        If .BufPos < .BufSize Then
                            .BufPos = .BufPos + 1
                            GoTo L35
                        Else
                            Exit Do
                        End If
                    End If
                Else
                    If .BufPos < .BufSize Then
                        .BufPos = .BufPos + 1
                        GoTo L35
                    Else
                        Exit Do
                    End If
                End If
L35:
            Loop
            If .BufData(.BufPos) = 42 And .BufData(.BufPos + 1) = 47 Then ' "*/"
                .BufPos = .BufPos + 2
                ParseBLOCKCOMMENT = True
                Exit Function
            Else
                .BufPos = p492
            End If
        End If
    End With
End Function

Private Function ParseLINECOMMENT() As Boolean
    Dim p497 As Long

    With ctx
        p497 = .BufPos
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
                ParseLINECOMMENT = True
                Exit Function
            Else
                .BufPos = p497
            End If
        End If
    End With
End Function

Private Function ParseWS() As Boolean
    Dim i468 As Long

    With ctx
        For i468 = 0 To LNG_MAXINT
            Select Case .BufData(.BufPos)
            Case 32, 9                              ' [ \t]
                .BufPos = .BufPos + 1
                GoTo L36
            Case Else
                If ParseNL() Then
                    GoTo L36
                Else
                    Exit For
                End If
            End Select
L36:
        Next
        If i468 <> 0 Then
            ParseWS = True
        End If
    End With
End Function

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
    Dim p211 As Long
    Dim q211 As Long

    With ctx
        p211 = .BufPos
        q211 = .ThunkPos
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
            .BufPos = p211
            .ThunkPos = q211
        End If
    End With
End Function

Public Sub VbPegParseTypeSuffix()
    Dim p236 As Long

    With ctx
        Do
            p236 = .BufPos
            Call ParseCONST
            If Not ParseSTAR() Then
                .BufPos = p236
                Exit Do
            End If
        Loop
    End With
End Sub

Private Function ParseCONST() As Boolean
    Dim p353 As Long

    With ctx
        p353 = .BufPos
        If pvMatchString("const") Then              ' "const"
            .BufPos = .BufPos + 5
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p353
            Case Else
                Call Parse_
                ParseCONST = True
            End Select
        End If
    End With
End Function

Private Function ParseUNSIGNED() As Boolean
    Dim p348 As Long

    With ctx
        p348 = .BufPos
        If pvMatchString("unsigned") Then           ' "unsigned"
            .BufPos = .BufPos + 8
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p348
            Case Else
                Call Parse_
                ParseUNSIGNED = True
            End Select
        End If
    End With
End Function

Private Function ParseINT() As Boolean
    Dim p338 As Long

    With ctx
        p338 = .BufPos
        If pvMatchString("int") Then                ' "int"
            .BufPos = .BufPos + 3
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p338
            Case Else
                Call Parse_
                ParseINT = True
            End Select
        End If
    End With
End Function

Private Function ParseCHAR() As Boolean
    Dim p343 As Long

    With ctx
        p343 = .BufPos
        If pvMatchString("char") Then               ' "char"
            .BufPos = .BufPos + 4
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p343
            Case Else
                Call Parse_
                ParseCHAR = True
            End Select
        End If
    End With
End Function

Private Function ParseVOID() As Boolean
    Dim p358 As Long

    With ctx
        p358 = .BufPos
        If pvMatchString("void") Then               ' "void"
            .BufPos = .BufPos + 4
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p358
            Case Else
                Call Parse_
                ParseVOID = True
            End Select
        End If
    End With
End Function

Private Function ParseUINT_A_T() As Boolean
    Dim p363 As Long

    With ctx
        p363 = .BufPos
        If pvMatchString("uint8_t") Then            ' "uint8_t"
            .BufPos = .BufPos + 7
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p363
            Case Else
                Call Parse_
                ParseUINT_A_T = True
            End Select
        End If
    End With
End Function

Private Function ParseUINT_B_T() As Boolean
    Dim p368 As Long

    With ctx
        p368 = .BufPos
        If pvMatchString("uint32_t") Then           ' "uint32_t"
            .BufPos = .BufPos + 8
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p368
            Case Else
                Call Parse_
                ParseUINT_B_T = True
            End Select
        End If
    End With
End Function

Private Function ParseUINT_C_T() As Boolean
    Dim p373 As Long

    With ctx
        p373 = .BufPos
        If pvMatchString("uint64_t") Then           ' "uint64_t"
            .BufPos = .BufPos + 8
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p373
            Case Else
                Call Parse_
                ParseUINT_C_T = True
            End Select
        End If
    End With
End Function

Private Function ParseUINTPTR_T() As Boolean
    Dim p378 As Long

    With ctx
        p378 = .BufPos
        If pvMatchString("uintptr_t") Then          ' "uintptr_t"
            .BufPos = .BufPos + 9
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p378
            Case Else
                Call Parse_
                ParseUINTPTR_T = True
            End Select
        End If
    End With
End Function

Private Function ParseSIZE_T() As Boolean
    Dim p383 As Long

    With ctx
        p383 = .BufPos
        If pvMatchString("size_t") Then             ' "size_t"
            .BufPos = .BufPos + 6
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p383
            Case Else
                Call Parse_
                ParseSIZE_T = True
            End Select
        End If
    End With
End Function

Private Function ParseDOUBLE() As Boolean
    Dim p388 As Long

    With ctx
        p388 = .BufPos
        If pvMatchString("double") Then             ' "double"
            .BufPos = .BufPos + 6
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p388
            Case Else
                Call Parse_
                ParseDOUBLE = True
            End Select
        End If
    End With
End Function

Private Function ParseLONG_LONG() As Boolean
    Dim p393 As Long

    With ctx
        p393 = .BufPos
        If pvMatchString("long long") Then          ' "long long"
            .BufPos = .BufPos + 9
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p393
            Case Else
                Call Parse_
                ParseLONG_LONG = True
            End Select
        End If
    End With
End Function

Private Function ParseBOOL() As Boolean
    Dim p398 As Long

    With ctx
        p398 = .BufPos
        If pvMatchString("bool") Then               ' "bool"
            .BufPos = .BufPos + 4
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p398
            Case Else
                Call Parse_
                ParseBOOL = True
            End Select
        End If
    End With
End Function

Public Function VbPegParseRefType() As Boolean
    Dim p241 As Long
    Dim q241 As Long

    With ctx
        p241 = .BufPos
        q241 = .ThunkPos
        If ParseLLVM_STDCALL() Then
            .BufPos = p241
            .ThunkPos = q241
        Else
            If ParseID() Then
                If  IsRefType(Mid$(.Contents, .CaptureBegin + 1, .CaptureEnd - .CaptureBegin))  Then
                    VbPegParseRefType = True
                    Exit Function
                Else
                    .BufPos = p241
                    .ThunkPos = q241
                End If
            Else
                .BufPos = p241
                .ThunkPos = q241
            End If
        End If
    End With
End Function

Public Function VbPegParseParam() As Boolean
    Dim p260 As Long
    Dim q260 As Long
    Dim p266 As Long
    Dim q266 As Long

    With ctx
        pvPushThunk ucsActVarAlloc, 3
        If VbPegParseType() Then
            pvPushThunk ucsActVarSet, 1
            p260 = .BufPos
            q260 = .ThunkPos
            If ParseID() Then
                pvPushThunk ucsActVarSet, 2
                p266 = .BufPos
                q266 = .ThunkPos
                If VbPegParseArraySuffix() Then
                    pvPushThunk ucsActVarSet, 3
                    pvPushAction ucsAct_1_Param
                    pvPushThunk ucsActVarAlloc, -3
                    VbPegParseParam = True
                    Exit Function
                Else
                    .BufPos = p266
                    .ThunkPos = q266
                End If
                Call ParseEMPTY
                pvPushThunk ucsActVarSet, 3
                pvPushAction ucsAct_1_Param
                pvPushThunk ucsActVarAlloc, -3
                VbPegParseParam = True
                Exit Function
            Else
                .BufPos = p260
                .ThunkPos = q260
            End If
            Call ParseEMPTY
            pvPushThunk ucsActVarSet, 2
            p266 = .BufPos
            q266 = .ThunkPos
            If VbPegParseArraySuffix() Then
                pvPushThunk ucsActVarSet, 3
                pvPushAction ucsAct_1_Param
                pvPushThunk ucsActVarAlloc, -3
                VbPegParseParam = True
                Exit Function
            Else
                .BufPos = p266
                .ThunkPos = q266
            End If
            Call ParseEMPTY
            pvPushThunk ucsActVarSet, 3
            pvPushAction ucsAct_1_Param
            pvPushThunk ucsActVarAlloc, -3
            VbPegParseParam = True
        End If
    End With
End Function

Public Function VbPegParseArraySuffix() As Boolean
    Dim p298 As Long
    Dim q298 As Long
    Dim lCaptureBegin As Long
    Dim p303 As Long
    Dim lCaptureEnd As Long

    With ctx
        p298 = .BufPos
        q298 = .ThunkPos
        lCaptureBegin = .BufPos
        If ParseLBRACKET() Then
            Do
                p303 = .BufPos
                If ParseRBRACKET() Then
                    .BufPos = p303
                    Exit Do
                End If
                If .BufPos < .BufSize Then
                    .BufPos = .BufPos + 1
                Else
                    .BufPos = p303
                    Exit Do
                End If
            Loop
            If ParseRBRACKET() Then
                lCaptureEnd = .BufPos
                Call Parse_
                .CaptureBegin = lCaptureBegin
                .CaptureEnd = lCaptureEnd
                pvPushAction ucsAct_1_ArraySuffix
                VbPegParseArraySuffix = True
                Exit Function
            Else
                .BufPos = p298
                .ThunkPos = q298
            End If
        End If
    End With
End Function

Public Function VbPegParseEnumValueToken() As Boolean
    Dim lCaptureBegin As Long
    Dim i290 As Long
    Dim p289 As Long
    Dim lCaptureEnd As Long

    With ctx
        lCaptureBegin = .BufPos
        For i290 = 0 To LNG_MAXINT
            p289 = .BufPos
            If ParseBLOCKCOMMENT() Then
                .BufPos = p289
                Exit For
            End If
            If ParseLINECOMMENT() Then
                .BufPos = p289
                Exit For
            End If
            If ParseWS() Then
                .BufPos = p289
                Exit For
            End If
            Select Case .BufData(.BufPos)
            Case 44, 125                            ' [,}]
                .BufPos = .BufPos + 1
                .BufPos = p289
                Exit For
            End Select
            If .BufPos < .BufSize Then
                .BufPos = .BufPos + 1
            Else
                .BufPos = p289
                Exit For
            End If
        Next
        If i290 <> 0 Then
            lCaptureEnd = .BufPos
            Call Parse_
            .CaptureBegin = lCaptureBegin
            .CaptureEnd = lCaptureEnd
            pvPushAction ucsAct_1_EnumValueToken
            VbPegParseEnumValueToken = True
        End If
    End With
End Function

Private Function ParseLBRACKET() As Boolean
    With ctx
        If .BufData(.BufPos) = 91 Then              ' "["
            .BufPos = .BufPos + 1
            Call Parse_
            ParseLBRACKET = True
        End If
    End With
End Function

Private Function ParseRBRACKET() As Boolean
    With ctx
        If .BufData(.BufPos) = 93 Then              ' "]"
            .BufPos = .BufPos + 1
            Call Parse_
            ParseRBRACKET = True
        End If
    End With
End Function

Private Function ParseEXTERN() As Boolean
    Dim p418 As Long

    With ctx
        p418 = .BufPos
        If pvMatchString("extern") Then             ' "extern"
            .BufPos = .BufPos + 6
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p418
            Case Else
                Call Parse_
                ParseEXTERN = True
            End Select
        End If
    End With
End Function

Private Function ParseSTATIC() As Boolean
    Dim p423 As Long

    With ctx
        p423 = .BufPos
        If pvMatchString("static") Then             ' "static"
            .BufPos = .BufPos + 6
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p423
            Case Else
                Call Parse_
                ParseSTATIC = True
            End Select
        End If
    End With
End Function

Private Function ParseINLINE() As Boolean
    Dim p428 As Long

    With ctx
        p428 = .BufPos
        If pvMatchString("inline") Then             ' "inline"
            .BufPos = .BufPos + 6
            Select Case .BufData(.BufPos)
            Case 97 To 122, 65 To 90, 95, 48 To 57, 35 ' [a-zA-Z_0-9#]
                .BufPos = p428
            Case Else
                Call Parse_
                ParseINLINE = True
            End Select
        End If
    End With
End Function

Private Function ParsePREPRO() As Boolean
    Dim p479 As Long

    With ctx
        p479 = .BufPos
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
                ParsePREPRO = True
                Exit Function
            Else
                .BufPos = p479
            End If
        End If
    End With
End Function

Private Sub pvImplAction(ByVal eAction As UcsParserActionsEnum, ByVal lOffset As Long, ByVal lSize As Long)
    With ctx
        Select Case eAction
        Case ucsAct_3_Stmt
               Dim oJson As Object, oEl As Object : Set .VarResult = .VarStack(.VarPos - 1)
        Case ucsAct_2_Stmt
               Set oJson = .VarStack(.VarPos - 1) : JsonItem(oJson, -1) = .VarStack(.VarPos - 2)
        Case ucsAct_1_Stmt
               JsonItem(oJson, -1) = Empty
                                                                            Set .VarStack(.VarPos - 1) = oJson
        Case ucsAct_1_TypedefDecl
               JsonItem(oJson, "Tag") = "TypedefDecl"
                                                                            JsonItem(oJson, "Name") = .VarStack(.VarPos - 2)
                                                                            JsonItem(oJson, "Type") = .VarStack(.VarPos - 1)
                                                                            Set .VarResult = oJson
        Case ucsAct_1_TypedefCallback
               JsonItem(oJson, "Tag") = "TypedefCallback"
                                                                            JsonItem(oJson, "Name") = .VarStack(.VarPos - 2)
                                                                            JsonItem(oJson, "Type") = .VarStack(.VarPos - 1)
                                                                            JsonItem(oJson, "Params") = .VarStack(.VarPos - 3)
                                                                            Set .VarResult = oJson
        Case ucsAct_3_EnumDecl
               Set oJson = .VarStack(.VarPos - 1)
                                                                            JsonItem(oJson, "Name") = .VarStack(.VarPos - 2)
                                                                            Set .VarResult = oJson
        Case ucsAct_2_EnumDecl
               JsonItem(oEl, "Name") = .VarStack(.VarPos - 3)
                                                                            JsonItem(oEl, "Value") = zn(CStr(.VarStack(.VarPos - 4)), Empty)
                                                                            Set oJson = .VarStack(.VarPos - 1)
                                                                            JsonItem(oJson, "Items/-1") = oEl
        Case ucsAct_1_EnumDecl
               JsonItem(oJson, "Tag") = "EnumDecl" 
                                                                            JsonItem(oJson, "Items/-1") = Empty
                                                                            Set .VarStack(.VarPos - 1) = oJson
        Case ucsAct_3_StructDecl
               Set oJson = .VarStack(.VarPos - 1)
                                                                            JsonItem(oJson, "Name") = .VarStack(.VarPos - 2)
                                                                            Set .VarResult = oJson
        Case ucsAct_2_StructDecl
               JsonItem(oEl, "Name") = .VarStack(.VarPos - 4)
                                                                            JsonItem(oEl, "Type") = .VarStack(.VarPos - 3)
                                                                            Set oJson = .VarStack(.VarPos - 1)
                                                                            JsonItem(oJson, "Items/-1") = oEl
        Case ucsAct_1_StructDecl
               JsonItem(oJson, "Tag") = "StructDecl" 
                                                                            JsonItem(oJson, "Items/-1") = Empty
                                                                            Set .VarStack(.VarPos - 1) = oJson
        Case ucsAct_1_FunDecl
               JsonItem(oJson, "Tag") = "FunDecl"
                                                                            JsonItem(oJson, "Name") = .VarStack(.VarPos - 2)
                                                                            JsonItem(oJson, "Type") = .VarStack(.VarPos - 1)
                                                                            JsonItem(oJson, "Params") = .VarStack(.VarPos - 3)
                                                                            Set .VarResult = oJson
        Case ucsAct_1_SkipStmt
               JsonItem(oJson, "Tag") = "SkipStmt"
                                                                            JsonItem(oJson, "Text") = Mid$(.Contents, lOffset, lSize)
                                                                            Set .VarResult = oJson
        Case ucsAct_1_Type
             .VarResult = Mid$(.Contents, lOffset, lSize)
        Case ucsAct_1_ID
               .VarResult = Mid$(.Contents, lOffset, lSize)
        Case ucsAct_1_TypeUnlimited
             .VarResult = Mid$(.Contents, lOffset, lSize)
        Case ucsAct_3_Params
               Set .VarResult = .VarStack(.VarPos - 1)
        Case ucsAct_2_Params
               Set oJson = .VarStack(.VarPos - 1) : JsonItem(oJson, -1) = .VarStack(.VarPos - 2)
        Case ucsAct_1_Params
               JsonItem(oJson, -1) = .VarStack(.VarPos - 1) : Set .VarStack(.VarPos - 1) = oJson
        Case ucsAct_1_EMPTY
               .VarResult = Mid$(.Contents, lOffset, lSize)
        Case ucsAct_4_EnumValue
               Set oJson = .VarStack(.VarPos - 1) : .VarResult = ConcatCollection(oJson, " ")
        Case ucsAct_3_EnumValue
               .VarStack(.VarPos - 1).Add .VarStack(.VarPos - 2)
        Case ucsAct_2_EnumValue
               .VarStack(.VarPos - 1).Add .VarStack(.VarPos - 2)
        Case ucsAct_1_EnumValue
               Set .VarStack(.VarPos - 1) = New Collection
        Case ucsAct_1_Param
               JsonItem(oJson, "Type") = .VarStack(.VarPos - 1)
                                                                            JsonItem(oJson, "Name") = .VarStack(.VarPos - 2)
                                                                            JsonItem(oJson, "ArraySuffix") = zn(CStr(.VarStack(.VarPos - 3)), Empty)
                                                                            Set .VarResult = oJson
        Case ucsAct_1_ArraySuffix
               .VarResult = Mid$(.Contents, lOffset, lSize)
        Case ucsAct_1_EnumValueToken
               .VarResult = Mid$(.Contents, lOffset, lSize)
        End Select
    End With
End Sub
