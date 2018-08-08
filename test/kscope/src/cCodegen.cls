VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cCodegen"
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
' cCodegen.cls - Codegen functions
'
'=========================================================================
Option Explicit
DefObj A-Z

'=========================================================================
' Constants and member variables
'=========================================================================

Private Const ERR_NO_METHOD     As Long = 438  ' Object doesn't support this property or method

Private m_sLastError        As String
Private m_oTree             As Object
Private m_oMachine          As cTargetMachine
Private m_sModuleName       As String
Private m_hModule           As LLVMModuleRef
Private m_hBuilder          As LLVMBuilderRef
Private m_hFuncPassManager  As LLVMPassManagerRef
Private m_oModuleScope      As Scripting.Dictionary
Private m_oLocalScope       As Scripting.Dictionary

'=========================================================================
' Properties
'=========================================================================

Property Get LastError() As String
    LastError = m_sLastError
End Property

Property Get ModuleName() As String
    ModuleName = m_sModuleName
End Property

'=========================================================================
' Methods
'=========================================================================

Public Function Init(oTree As Object, oMachine As cTargetMachine, sModuleName As String) As Boolean
    '--- init member vars
    Set m_oTree = oTree
    Set m_oMachine = oMachine
    m_sModuleName = sModuleName
    '--- setup LLVM module
    m_hModule = LLVMModuleCreateWithName(m_sModuleName & ".ks")
    If m_hModule = 0 Then
        GoTo QH
    End If
    Call LLVMSetTarget(m_hModule, m_oMachine.Triple)
    Call LLVMSetModuleDataLayout(m_hModule, LLVMCreateTargetDataLayout(m_oMachine.GetMachine()))
    m_hBuilder = LLVMCreateBuilder()
    If m_hBuilder = 0 Then
        GoTo QH
    End If
    '--- setup current scope
    Set m_oModuleScope = New Scripting.Dictionary
    Set m_oLocalScope = m_oLocalScope
    '--- success
    Init = True
QH:
End Function

Public Function SetOptimize(ByVal lLevel As Long) As Boolean
    If lLevel = 0 Then
        If m_hFuncPassManager <> 0 Then
            Call LLVMDisposePassManager(m_hFuncPassManager)
            m_hFuncPassManager = 0
        End If
    Else
        m_hFuncPassManager = LLVMCreateFunctionPassManagerForModule(m_hModule)
        Call LLVMInitializeFunctionPassManager(m_hFuncPassManager)
        Call LLVMAddConstantPropagationPass(m_hFuncPassManager)
        Call LLVMAddReassociatePass(m_hFuncPassManager)
        Call LLVMAddCFGSimplificationPass(m_hFuncPassManager)
    End If
    '--- success
    SetOptimize = True
End Function

Public Function CodeGenTop(vNode As Variant) As Boolean
    Dim oFunc           As Object
    Dim oProto          As Object
    
    Select Case JsonItem(C_Obj(vNode), "Tag")
    Case "Def", "Extern"
        CodeGenTop = CodeGenNode(vNode, 0)
    Case Else
        JsonItem(oProto, "Tag") = "Prototype"
        JsonItem(oProto, "Name") = "__anon_expr"
        JsonItem(oFunc, "Tag") = "Def"
        JsonItem(oFunc, "Proto") = oProto
        JsonItem(oFunc, "Body") = vNode
        CodeGenTop = CodeGenNode(oFunc, 0)
    End Select
End Function

Public Function CodeGenNode(vNode As Variant, hResult As LLVMValueRef) As Boolean
    Dim oNode           As Scripting.Dictionary
    
    Select Case VarType(vNode)
    Case vbDouble
        hResult = LLVMConstReal(LLVMDoubleType(), vNode)
    Case vbString
        hResult = pvScopeResolve(m_oLocalScope, C_Str(vNode))
        If hResult = 0 Then
            m_sLastError = "Undefined '" & vNode & "'"
            GoTo QH
        End If
        hResult = LLVMBuildLoad(m_hBuilder, hResult, C_Str(vNode))
    Case vbObject
        Set oNode = C_Obj(vNode)
        On Error GoTo EH_NoMethod
        If Not CallByName(Me, "Visit" & JsonItem(oNode, "Tag"), VbMethod, oNode, hResult) Then
            GoTo QH
        End If
        On Error GoTo 0
    Case Else
        Err.Raise vbObjectError, , "Unknown node vartype: " & VarType(vNode)
    End Select
    '--- success
    CodeGenNode = True
QH:
    Exit Function
EH_NoMethod:
    If Err.Number = ERR_NO_METHOD Then
        Err.Raise vbObjectError, , "Unknown node type: " & JsonItem(oNode, "Tag")
    End If
    Err.Raise Err.Number, Err.Source, Err.Description
End Function

Public Function GetModule() As LLVMModuleRef
    Dim lMsgPtr         As Long
    
    If LLVMVerifyModule(m_hModule, LLVMAbortProcessAction, lMsgPtr) <> 0 Then
        m_sLastError = ToString(lMsgPtr)
        Call LLVMDisposeMessage(lMsgPtr)
        GoTo QH
    End If
    GetModule = m_hModule
QH:
End Function

Public Function GetFunction(sName As String) As LLVMValueRef
    GetFunction = pvScopeResolve(m_oModuleScope, sName)
    If GetFunction = 0 Then
        GetFunction = LLVMGetNamedFunction(m_hModule, sName)
    End If
End Function

Public Function GetString() As String
    Dim lMsgPtr         As Long
    
    If LLVMVerifyModule(m_hModule, LLVMAbortProcessAction, lMsgPtr) <> 0 Then
        m_sLastError = ToString(lMsgPtr)
        Call LLVMDisposeMessage(lMsgPtr)
        GoTo QH
    End If
    lMsgPtr = LLVMPrintModuleToString(m_hModule)
    If lMsgPtr <> 0 Then
        GetString = ToString(lMsgPtr)
        Call LLVMDisposeMessage(lMsgPtr)
    End If
QH:
End Function

Public Function EmitToFile(sFile As String) As Boolean
    Dim lMsgPtr         As Long
    
    If LenB(sFile) = 0 Then
        sFile = m_sModuleName & ".o"
    End If
    If LLVMTargetMachineEmitToFile(m_oMachine.GetMachine(), m_hModule, sFile, LLVMObjectFile, lMsgPtr) <> 0 Then
        m_sLastError = ToString(lMsgPtr)
        Call LLVMDisposeMessage(lMsgPtr)
        GoTo QH
    End If
    '--- success
    EmitToFile = True
QH:
End Function

'= visitor impl ==========================================================

Public Function VisitDef(oNode As Scripting.Dictionary, hResult As LLVMValueRef) As Boolean
    Dim oScope          As Scripting.Dictionary
    Dim hFunc           As LLVMValueRef
    Dim hBlock          As LLVMBasicBlockRef
    Dim hBody           As LLVMValueRef
    Dim hParam          As LLVMValueRef
    Dim hAlloca         As LLVMValueRef
    Dim sName           As String
    Dim lIdx            As Long
    
    If Not CodeGenNode(JsonItem(oNode, "Proto"), hResult) Then
        GoTo QH
    End If
    hFunc = hResult
    hBlock = LLVMAppendBasicBlock(hFunc, "entry")
    Call LLVMPositionBuilderAtEnd(m_hBuilder, hBlock)
    Set oScope = New Scripting.Dictionary
    pvScopePush oScope
    For lIdx = 0 To LLVMCountParams(hFunc) - 1
        hParam = LLVMGetParam(hFunc, lIdx)
        Debug.Assert hParam <> 0
        sName = ToString(LLVMGetValueName(hParam))
        hAlloca = pvCreateEntryBlockAlloca(hFunc, sName)
        Call LLVMBuildStore(m_hBuilder, hParam, hAlloca)
        m_oLocalScope.Item(sName) = hAlloca
    Next
    If Not CodeGenNode(JsonItem(oNode, "Body"), hBody) Then
        Call LLVMDeleteFunction(hFunc)
        GoTo QH
    End If
    Call LLVMBuildRet(m_hBuilder, hBody)
    If LLVMVerifyFunction(hFunc, LLVMPrintMessageAction) = 1 Then
        Call LLVMDeleteFunction(hFunc)
        GoTo QH
    End If
    If m_hFuncPassManager <> 0 Then
        If LLVMRunFunctionPassManager(m_hFuncPassManager, hFunc) = 1 Then
            Call LLVMDeleteFunction(hFunc)
            GoTo QH
        End If
    End If
    '--- success
    VisitDef = True
QH:
    If Not oScope Is Nothing Then
        pvScopePop
    End If
End Function

Public Function VisitExtern(oNode As Scripting.Dictionary, hResult As LLVMValueRef) As Boolean
    If Not CodeGenNode(JsonItem(oNode, "Proto"), hResult) Then
        GoTo QH
    End If
    Call LLVMSetLinkage(hResult, LLVMExternalLinkage)
'    pvAddLLVMAttrStr hResult, "less-precise-fpmad", "false"
'    pvAddLLVMAttrStr hResult, "no-frame-pointer-elim", "true"
'    pvAddLLVMAttrStr hResult, "no-frame-pointer-elim-non-leaf", vbNullString
'    pvAddLLVMAttrStr hResult, "no-infs-fp-math", "false"
'    pvAddLLVMAttrStr hResult, "no-nans-fp-math", "false"
'    pvAddLLVMAttrStr hResult, "stack-protector-buffer-size", "8"
'    pvAddLLVMAttrStr hResult, "unsafe-fp-math", "false"
'    pvAddLLVMAttrStr hResult, "use-soft-float", "false"
    '--- success
    VisitExtern = True
QH:
End Function

Public Function VisitPrototype(oNode As Scripting.Dictionary, hResult As LLVMValueRef) As Boolean
    Dim sName           As String
    Dim lCount          As Long
    Dim aParams()       As LLVMTypeRef
    Dim lIdx            As Long
    Dim hType           As LLVMTypeRef
    Dim hParam          As LLVMValueRef
    
    sName = JsonItem(oNode, "Name")
    If IsObject(JsonItem(oNode, "Params")) Then
        lCount = UBound(JsonKeys(oNode, "Params")) + 1
    End If
    hResult = LLVMGetNamedFunction(m_hModule, sName)
    If hResult <> 0 Then
        If LLVMCountParams(hResult) <> lCount Then
            m_sLastError = sName & " declared with different param count"
            GoTo QH
        End If
        If LLVMCountBasicBlocks(hResult) <> 0 Then
            m_sLastError = sName & " already defined"
            GoTo QH
        End If
    Else
        ReDim aParams(0 To lCount) As LLVMTypeRef '--- one more
        For lIdx = 0 To lCount - 1
            aParams(lIdx) = LLVMDoubleType()
        Next
        hType = LLVMFunctionType(LLVMDoubleType(), aParams(0), lCount, 0)
        Debug.Assert hType <> 0
        hResult = LLVMAddFunction(m_hModule, sName, hType)
        m_oModuleScope.Item(sName) = hResult
    End If
    For lIdx = 0 To lCount - 1
        hParam = LLVMGetParam(hResult, lIdx)
        Debug.Assert hParam <> 0
        Call LLVMSetValueName(hParam, JsonItem(oNode, "Params/" & lIdx))
    Next
    '--- success
    VisitPrototype = True
QH:
End Function

Public Function VisitBinExpr(oNode As Scripting.Dictionary, hResult As LLVMValueRef) As Boolean
    Dim hAlloca         As LLVMValueRef
    Dim hLeft           As LLVMValueRef
    Dim hRight          As LLVMValueRef
    Dim hFunc           As LLVMValueRef
    Dim aArgs()         As LLVMValueRef
    
    If JsonItem(oNode, "Oper") = "=" Then
        If VarType(JsonItem(oNode, "Left")) <> vbString Then
            m_sLastError = "Destination of '=' must be a variable"
            GoTo QH
        End If
        hAlloca = pvScopeResolve(m_oLocalScope, JsonItem(oNode, "Left"))
        If hAlloca = 0 Then
            m_sLastError = "Unknown variable '" & JsonItem(oNode, "Left") & "'"
            GoTo QH
        End If
        If Not CodeGenNode(JsonItem(oNode, "Right"), hRight) Then
            GoTo QH
        End If
        Call LLVMBuildStore(m_hBuilder, hRight, hAlloca)
    Else
        If Not CodeGenNode(JsonItem(oNode, "Left"), hLeft) Then
            GoTo QH
        End If
        If Not CodeGenNode(JsonItem(oNode, "Right"), hRight) Then
            GoTo QH
        End If
        Select Case JsonItem(oNode, "Oper")
        Case "+"
            hResult = LLVMBuildFAdd(m_hBuilder, hLeft, hRight, "addtmp")
        Case "-"
            hResult = LLVMBuildFSub(m_hBuilder, hLeft, hRight, "subtmp")
        Case "*"
            hResult = LLVMBuildFMul(m_hBuilder, hLeft, hRight, "multmp")
        Case "/"
            hResult = LLVMBuildFDiv(m_hBuilder, hLeft, hRight, "divtmp")
        Case "<"
            hResult = LLVMBuildFCmp(m_hBuilder, LLVMRealULT, hLeft, hRight, "cmptmp")
            hResult = LLVMBuildCast(m_hBuilder, LLVMUIToFP, hResult, LLVMDoubleType(), "booltmp")
        Case Else
            hFunc = GetFunction("binary" & JsonItem(oNode, "Oper"))
            If hFunc = 0 Then
                m_sLastError = "Unknown binop '" & JsonItem(oNode, "Oper") & "'"
                GoTo QH
            End If
            ReDim aArgs(0 To 1) As LLVMValueRef
            aArgs(0) = hLeft
            aArgs(1) = hRight
            hResult = LLVMBuildCall(m_hBuilder, hFunc, aArgs(0), 2, "binop")
        End Select
    End If
    '--- success
    VisitBinExpr = True
QH:
End Function

Public Function VisitUnExpr(oNode As Scripting.Dictionary, hResult As LLVMValueRef) As Boolean
    Dim hFunc           As LLVMValueRef
    Dim aArgs()         As LLVMValueRef
    
    hFunc = GetFunction("unary" & JsonItem(oNode, "Oper"))
    If hFunc = 0 Then
        m_sLastError = "Unknown unary '" & JsonItem(oNode, "Oper") & "'"
        GoTo QH
    End If
    ReDim aArgs(0 To 0) As LLVMValueRef
    If Not CodeGenNode(JsonItem(oNode, "Expr"), aArgs(0)) Then
        GoTo QH
    End If
    hResult = LLVMBuildCall(m_hBuilder, hFunc, aArgs(0), 1, "unop")
    '--- success
    VisitUnExpr = True
QH:
End Function

Public Function VisitCallExpr(oNode As Scripting.Dictionary, hResult As LLVMValueRef) As Boolean
    Dim sName           As String
    Dim hFunc           As LLVMValueRef
    Dim lCount          As Long
    Dim lIdx            As Long
    Dim aArgs()         As LLVMValueRef
    
    sName = JsonItem(oNode, "Name")
    hFunc = GetFunction(sName)
    If hFunc = 0 Then
        m_sLastError = "Function '" & sName & "' not defined"
        GoTo QH
    End If
    If IsObject(JsonItem(oNode, "Args")) Then
        lCount = UBound(JsonKeys(oNode, "Args")) + 1
    End If
    If LLVMCountParams(hFunc) <> lCount Then
        m_sLastError = "Wrong number of arguments to '" & sName & "' function"
        GoTo QH
    End If
    ReDim aArgs(0 To LLVMCountParams(hFunc)) As LLVMValueRef
    For lIdx = 0 To lCount - 1
        If Not CodeGenNode(JsonItem(oNode, "Args/" & lIdx), aArgs(lIdx)) Then
            GoTo QH
        End If
    Next
    hResult = LLVMBuildCall(m_hBuilder, hFunc, aArgs(0), lCount, "calltmp")
    '--- success
    VisitCallExpr = True
QH:
End Function

Public Function VisitIfExpr(oNode As Scripting.Dictionary, hResult As LLVMValueRef) As Boolean
    Dim hCond           As LLVMValueRef
    Dim hThen           As LLVMValueRef
    Dim hElse           As LLVMValueRef
    Dim hFunc           As LLVMValueRef
    Dim hBlockThen      As LLVMBasicBlockRef
    Dim hBlockElse      As LLVMBasicBlockRef
    Dim hBlockMerge     As LLVMBasicBlockRef
    
    If Not CodeGenNode(JsonItem(oNode, "Cond"), hCond) Then
        GoTo QH
    End If
    hFunc = LLVMGetBasicBlockParent(LLVMGetInsertBlock(m_hBuilder))
    hBlockThen = LLVMAppendBasicBlock(hFunc, "then")
    hBlockElse = LLVMAppendBasicBlock(hFunc, "else")
    hBlockMerge = LLVMAppendBasicBlock(hFunc, "ifcont")
    hCond = LLVMBuildFCmp(m_hBuilder, LLVMRealONE, hCond, LLVMConstReal(LLVMDoubleType(), 0), "ifcond")
    Call LLVMBuildCondBr(m_hBuilder, hCond, hBlockThen, hBlockElse)
    Call LLVMPositionBuilderAtEnd(m_hBuilder, hBlockThen)
    If Not CodeGenNode(JsonItem(oNode, "Then"), hThen) Then
        GoTo QH
    End If
    Call LLVMBuildBr(m_hBuilder, hBlockMerge)
    hBlockThen = LLVMGetInsertBlock(m_hBuilder)
    Call LLVMPositionBuilderAtEnd(m_hBuilder, hBlockElse)
    If Not CodeGenNode(JsonItem(oNode, "Else"), hElse) Then
        GoTo QH
    End If
    Call LLVMBuildBr(m_hBuilder, hBlockMerge)
    hBlockElse = LLVMGetInsertBlock(m_hBuilder)
    Call LLVMPositionBuilderAtEnd(m_hBuilder, hBlockMerge)
    hResult = LLVMBuildPhi(m_hBuilder, LLVMDoubleType(), "iftmp")
    Call LLVMAddIncoming(hResult, hThen, hBlockThen, 1)
    Call LLVMAddIncoming(hResult, hElse, hBlockElse, 1)
    '--- success
    VisitIfExpr = True
QH:
End Function

Public Function VisitForExpr(oNode As Scripting.Dictionary, hResult As LLVMValueRef) As Boolean
    Dim hInit           As LLVMValueRef
    Dim hFunc           As LLVMValueRef
    Dim hAlloca         As LLVMValueRef
    Dim hBlockLoop      As LLVMBasicBlockRef
    Dim hStepVal        As LLVMValueRef
    Dim hEnd            As LLVMValueRef
    Dim hCurVar         As LLVMValueRef
    Dim hNextVar        As LLVMValueRef
    Dim hBlockAfter     As LLVMBasicBlockRef
    Dim oScope          As Scripting.Dictionary
    Dim hBody           As LLVMValueRef
    
    If Not CodeGenNode(JsonItem(oNode, "Init"), hInit) Then
        GoTo QH
    End If
    hFunc = LLVMGetBasicBlockParent(LLVMGetInsertBlock(m_hBuilder))
    hAlloca = pvCreateEntryBlockAlloca(hFunc, JsonItem(oNode, "Var"))
    Call LLVMBuildStore(m_hBuilder, hInit, hAlloca)
    hBlockLoop = LLVMAppendBasicBlock(hFunc, "loop")
    Call LLVMBuildBr(m_hBuilder, hBlockLoop)
    Call LLVMPositionBuilderAtEnd(m_hBuilder, hBlockLoop)
    Set oScope = New Scripting.Dictionary
    oScope.Item(JsonItem(oNode, "Var")) = hAlloca
    pvScopePush oScope
    If Not CodeGenNode(JsonItem(oNode, "Body"), hBody) Then
        GoTo QH
    End If
    If Not IsEmpty(JsonItem(oNode, "Step")) Then
        If Not CodeGenNode(JsonItem(oNode, "Step"), hStepVal) Then
            GoTo QH
        End If
    Else
        hStepVal = LLVMConstReal(LLVMDoubleType(), 1)
    End If
    If Not CodeGenNode(JsonItem(oNode, "End"), hEnd) Then
        GoTo QH
    End If
    hCurVar = LLVMBuildLoad(m_hBuilder, hAlloca, JsonItem(oNode, "Var"))
    hNextVar = LLVMBuildFAdd(m_hBuilder, hCurVar, hStepVal, "nextvar")
    Call LLVMBuildStore(m_hBuilder, hNextVar, hAlloca)
    hEnd = LLVMBuildFCmp(m_hBuilder, LLVMRealONE, hEnd, LLVMConstReal(LLVMDoubleType(), 0), "loopcond")
    hBlockAfter = LLVMAppendBasicBlock(hFunc, "afterloop")
    Call LLVMBuildCondBr(m_hBuilder, hEnd, hBlockLoop, hBlockAfter)
    Call LLVMPositionBuilderAtEnd(m_hBuilder, hBlockAfter)
    hResult = LLVMConstReal(LLVMDoubleType(), 0)
    '--- success
    VisitForExpr = True
QH:
    If Not oScope Is Nothing Then
        pvScopePop
    End If
End Function

Public Function VisitVarExpr(oNode As Scripting.Dictionary, hResult As LLVMValueRef) As Boolean
    Dim oScope          As Scripting.Dictionary
    Dim hFunc           As LLVMValueRef
    Dim lIdx            As Long
    Dim hAlloca         As LLVMValueRef
    Dim hInit           As LLVMValueRef
    
    Set oScope = New Scripting.Dictionary
    hFunc = LLVMGetBasicBlockParent(LLVMGetInsertBlock(m_hBuilder))
    For lIdx = 0 To UBound(JsonKeys(oNode, "Vars"))
        hAlloca = pvCreateEntryBlockAlloca(hFunc, JsonItem(oNode, "Vars/" & lIdx & "/Name"))
        If Not IsEmpty(JsonItem(oNode, "Vars/" & lIdx & "/Expr")) Then
            If Not CodeGenNode(JsonItem(oNode, "Vars/" & lIdx & "/Expr"), hInit) Then
                GoTo QH
            End If
            Call LLVMBuildStore(m_hBuilder, hInit, hAlloca)
        End If
        oScope.Item(JsonItem(oNode, "Vars/" & lIdx & "/Name")) = hAlloca
    Next
    pvScopePush oScope
    If Not CodeGenNode(JsonItem(oNode, "Body"), hResult) Then
        GoTo QH
    End If
    '--- success
    VisitVarExpr = True
QH:
    If Not oScope Is Nothing Then
        pvScopePop
    End If
End Function

'= private ===============================================================

Private Function pvCreateEntryBlockAlloca(ByVal hFunc As LLVMValueRef, sName As String) As LLVMValueRef
    Dim hBuilder        As LLVMBuilderRef
    Dim hBlock          As LLVMBasicBlockRef
    
    hBuilder = LLVMCreateBuilder()
    hBlock = LLVMGetEntryBasicBlock(hFunc)
    Call LLVMPositionBuilder(hBuilder, hBlock, LLVMGetFirstInstruction(hBlock))
    pvCreateEntryBlockAlloca = LLVMBuildAlloca(hBuilder, LLVMDoubleType(), sName)
End Function

Private Sub pvScopePush(oScope As Scripting.Dictionary)
    Set oScope.Item("#Parent") = m_oLocalScope
    Set m_oLocalScope = oScope
End Sub

Private Sub pvScopePop()
    Set m_oLocalScope = m_oLocalScope.Item("#Parent")
End Sub

Private Function pvScopeResolve(ByVal oScope As Scripting.Dictionary, sName As String) As LLVMValueRef
    Do While Not oScope Is Nothing
        If oScope.Exists(sName) Then
            pvScopeResolve = oScope.Item(sName)
            Exit Do
        End If
        Set oScope = C_Obj(oScope.Item("#Parent"))
    Loop
End Function

'Private Sub pvAddLLVMAttrStr(ByVal hFunc As LLVMValueRef, sName As String, sValue As String)
'    Dim hAttr           As LLVMAttributeRef
'
'    hAttr = LLVMCreateStringAttribute(LLVMGetGlobalContext(), sName, Len(sName), sValue, Len(sValue))
'    If hAttr <> 0 Then
'        Call LLVMAddAttributeAtIndex(hFunc, -1, hAttr)
'    End If
'End Sub

'Private Sub pvAddLLVMAttrInt(ByVal hFunc As LLVMValueRef, sName As String, Optional ByVal lValue As Long)
'    Dim lKind           As Long
'    Dim hAttr           As LLVMAttributeRef
'
'    lKind = LLVMGetEnumAttributeKindForName(sName, Len(sName))
'    Debug.Assert lKind <> 0
'    hAttr = LLVMCreateEnumAttribute(LLVMGetGlobalContext(), lKind, lValue * 10000)
'    If hAttr <> 0 Then
'        Call LLVMAddAttributeAtIndex(hFunc, -1, hAttr)
'    End If
'End Sub

'=========================================================================
' Base class events
'=========================================================================

Private Sub Class_Terminate()
    If m_hFuncPassManager <> 0 Then
        Call LLVMDisposePassManager(m_hFuncPassManager)
    End If
    If m_hBuilder <> 0 Then
        Call LLVMDisposeBuilder(m_hBuilder)
    End If
    If m_hModule <> 0 Then
        Call LLVMDisposeModule(m_hModule)
    End If
End Sub
