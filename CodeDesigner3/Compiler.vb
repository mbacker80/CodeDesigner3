Public Class Compiler

    Private mpAsm As MIPSAssembly, Flt32 As RegisterFloat
    Private ErrDetail As ErrorDetails
    Private Lbls() As Labels, LBCount As Integer
    Private FuncsAndSubs() As AsmFunction, FnScount As Integer
    Private BraceScopes() As BraceScope, ScopeCount As Integer
    Private Footer() As FooterCode, FooterCount As Integer
    Private FThreads() As FuncThread, ThreadCount As Integer

    Private CodeFormat As String
    Private ifsCount As Integer, switchesCount As Integer

    Public ElfCfg As ELF_Config

    '----------------------------------------------- General Errors
    Private Const SyntaxError_UnknownInstruction = -1
    Private Const SyntaxError_EmptyString = -2
    Private Const SyntaxError_NoSignifcantData = -3

    '----------------------------------------------- Argument Errors
    Private Const SyntaxError_DataTypeUnknown = -4
    Private Const SyntaxError_BadArgumentCount = -5
    Private Const SyntaxError_BadArgumentType = -6

    '----------------------------------------------- Label Errors
    Private Const SyntaxError_LabelNotFound = -7
    Private Const SyntaxError_DuplicateLabel = -8
    Private Const SyntaxError_BadLabelDefinition = -9
    Private Const SyntaxError_BadStringDefinition = -10

    '----------------------------------------------- IF Statement Errors
    Private Const SyntaxError_BadIFStatement = -11

    '----------------------------------------------- SWITCH Statement Errors
    Private Const SyntaxError_BadSwitchDeclaration = -12
    Private Const SyntaxError_SwitchEmptyCase = -13
    Private Const SyntaxError_BadCaseDeclaration = -14

    '----------------------------------------------- General Bracing Errors
    Private Const SyntaxError_BracingError = -15

    '----------------------------------------------- For Loop Statement Errors
    Private Const SyntaxError_ForLoopInit = -16
    Private Const SyntaxError_ForLoopCond = -17
    Private Const SyntaxError_ForLoopIncr = -18

    '----------------------------------------------- While Loop Statement Errors
    Private Const SyntaxError_BadWhileCondition = -19



    Private Structure FuncThread
        Dim ThreadName As String
        Dim ThreadID As Integer
    End Structure
    Private Structure FooterCode
        Dim DeclarationLine As Integer
        Dim Code As String
        Dim LabelName As String
        Dim PageName As String
    End Structure

    Private Structure BraceScope
        Dim First As Integer
        Dim Last As Integer
        Dim Leave As String
    End Structure

    Private Structure Labels
        Dim LName As String
        Dim LValue As Int32
    End Structure

    Private Structure AsmFunction
        Dim FncName As String
        Dim FncAddr As Int32
        Dim ArgTypes() As String
        Dim ArgRegs() As String

        Dim Preserves() As String
        Dim Restores() As String

        Dim FncType As Integer
        Dim Updated As Boolean
    End Structure

    Private Structure ErrorDetails
        Dim PageName As String
        Dim ErrorNumber As Integer
        Dim LineNumber As Integer
        Dim LineData As String
        Dim ExtraDetails As String
    End Structure

    Public Structure ELF_Config
        Dim Entry As UInt32
        Dim Main As UInt32
        Dim StackSize As UInt32
        Dim ResourceCount As Int32
        Dim Resources() As ELF_Resource
        Dim ResPath As String
    End Structure
    Public Structure ELF_Resource
        Dim Offset As UInt32
        Dim Data() As Byte
    End Structure

    Private Function CreateError(PGName As String, ErrNum As Integer, LineNum As Integer,
                                 LineData As String, ExtraDets As String) As Integer

        With ErrDetail
            .PageName = PGName
            .ErrorNumber = ErrNum
            .LineNumber = LineNum
            .LineData = LineData
            .ExtraDetails = ExtraDets
        End With

        Return ErrNum
    End Function

    Public Function RetreiveError(ByRef PGName As String, ByRef ErrNum As Integer,
                                  ByRef LineNum As Integer, ByRef LineData As String,
                                  ByRef ExtraDets As String) As String

        PGName = ErrDetail.PageName
        ErrNum = ErrDetail.ErrorNumber
        LineNum = ErrDetail.LineNumber
        LineData = ErrDetail.LineData
        ExtraDets = ErrDetail.ExtraDetails

        Select Case ErrDetail.ErrorNumber
            Case SyntaxError_UnknownInstruction
                Return "Unknown Instruction"
            Case SyntaxError_EmptyString
                Return "Empty String"
            Case SyntaxError_NoSignifcantData
                Return "No Significant Data"
            Case SyntaxError_DataTypeUnknown
                Return "Data Type Unknown"
            Case SyntaxError_BadArgumentCount
                Return "Bad Argument Count"
            Case SyntaxError_BadArgumentType
                Return "Wrong Argument Type"
            Case SyntaxError_LabelNotFound
                Return "Label Not Found"
            Case SyntaxError_DuplicateLabel
                Return "Duplicate Label Declaration"
            Case SyntaxError_BadLabelDefinition
                Return "Bad Label Definition"
            Case SyntaxError_BadIFStatement
                Return "Bad IF statement declaration"
            Case SyntaxError_BadSwitchDeclaration
                Return "Bad SWITCH statement declaration"
            Case SyntaxError_SwitchEmptyCase
                Return "Case statement is empty"
            Case SyntaxError_BadCaseDeclaration
                Return "Bad case declaration"
            Case SyntaxError_BracingError
                Return "Bracing issue"
            Case SyntaxError_ForLoopInit
                Return "Bad FOR loop initial statement"
            Case SyntaxError_ForLoopCond
                Return "Bad FOR loop conditional statement"
            Case SyntaxError_ForLoopIncr
                Return "Bad FOR loop increment statement"
            Case SyntaxError_BadWhileCondition
                Return "Bad WHILE loop conditional statement"
        End Select

        Return "Unkown Error"
    End Function

    Public Delegate Sub DebugOutput(Str As String)
    Private debugOut As DebugOutput

    Public Sub Init(ByRef Asm As MIPSAssembly, ByRef dbgFunc As DebugOutput)
        mpAsm = Asm

        FooterCount = -1
        FnScount = -1
        LBCount = -1
        ThreadCount = -1
        CodeFormat = "2"
        debugOut = dbgFunc

    End Sub

    Private Function GetLabel(LName As String, ByRef LValue As Int32) As Integer
        On Error GoTo E

        Dim I As Integer
        For I = 0 To Lbls.Count - 1
            If Lbls(I).LName = LName Then
                LValue = Lbls(I).LValue
                Return 0
            End If
        Next
        Return -1
        Exit Function
E:
        Err.Clear()
        Return -2
    End Function

    Private Function FindLabel(LName As String) As Int32
        On Error GoTo E

        Dim I As Integer
        For I = 0 To Lbls.Count - 1
            If Lbls(I).LName = LName Then
                Return I
            End If
        Next
        Return -1
        Exit Function
E:
        Err.Clear()
        Return -2
    End Function

    Private Function AddLabel(LName As String, LValue As Int32) As Integer
        Dim rt As Integer, tmp As Int32

        If LName = "" Then Return -2 'Empty Name

        rt = GetLabel(LName, tmp)
        If rt < 0 Then
            LBCount += 1
            ReDim Preserve Lbls(LBCount)
            Lbls(LBCount).LName = LName
            Lbls(LBCount).LValue = LValue
            Return 0

        Else
            Return -1 'Duplicate Label
        End If
    End Function

    Private Sub ClearLabels()
        ReDim Lbls(0)
        LBCount = -1
    End Sub

    Private Sub AddThread(TName As String)
        ThreadCount += 1
        ReDim Preserve FThreads(ThreadCount)
        With FThreads(ThreadCount)
            .ThreadName = TName
            .ThreadID = ThreadCount
        End With
    End Sub
    Private Function GetThreadID(TName As String) As Integer
        Dim i As Integer
        For i = 0 To ThreadCount
            With FThreads(i)
                If .ThreadName = TName Then Return .ThreadID
            End With
        Next

        Return -1
    End Function
    Private Sub ClearThreads()
        ThreadCount = -1
        ReDim FThreads(0)
    End Sub

    Private Sub ClearFuncs()
        ReDim FuncsAndSubs(0)
        FnScount = -1
    End Sub

    Private Function FindFuncOrSub(FncName As String) As Integer
        On Error GoTo E

        Dim i As Integer

        For i = 0 To FuncsAndSubs.Count - 1
            If FuncsAndSubs(i).FncName = FncName Then Return i
        Next

        Return -1
E:
        Return -2
    End Function

    Private Function AddFuncOrSub(FncName As String, FncAddr As Int32, ArgTypes() As String,
                                  ArgRegs() As String) As Integer
        Dim rt As Integer, i As Integer, fx As Integer

        If FncName = "" Then Return -2 'Empty Name
        rt = FindFuncOrSub(FncName)
        If rt < 0 Then
            rt = AddLabel(FncName, FncAddr)
            If rt < 0 Then Return rt

            FnScount += 1
            fx = FnScount

            ReDim Preserve FuncsAndSubs(fx)
            FuncsAndSubs(fx).Updated = False
updateFunc:
            rt = FindLabel(FncName)
            Lbls(rt).LValue = FncAddr

            With FuncsAndSubs(fx)
                .FncName = FncName
                .FncAddr = FncAddr
                ReDim .ArgTypes(ArgTypes.Count - 1)
                ReDim .ArgRegs(ArgRegs.Count - 1)
                For i = 0 To ArgTypes.Count - 1
                    .ArgTypes(i) = ArgTypes(i)
                    .ArgRegs(i) = ArgRegs(i)
                Next
                Return fx
            End With
        Else
            If FuncsAndSubs(rt).Updated = False Then
                FuncsAndSubs(rt).Updated = True
                fx = rt
                GoTo updateFunc
            End If
        End If

        Return -1 'Duplicate
    End Function

    Private Function MaxScope(Index As Integer) As Integer
        Dim I As Integer, Cnt As Integer
        Cnt = -1
        For I = 0 To ScopeCount
            With BraceScopes(I)
                If Index > .First And Index < .Last Then Cnt += 1
            End With
        Next
        Return Cnt
    End Function
    Private Function GetScope(Index As Integer, ScopeLevel As Integer) As Integer
        Dim Lowest As Integer, Highest As Integer, LevelCount As Integer
        Dim Scopes() As Integer

        Lowest = -1
        Highest = -1
        LevelCount = -1
        For i = 0 To ScopeCount
            With BraceScopes(i)
                If Index > .First And Index < .Last Then
                    LevelCount += 1
                    ReDim Preserve Scopes(LevelCount)
                    Scopes(LevelCount) = i
                End If
            End With
        Next
        If LevelCount < 0 Then Return -1
        If ScopeLevel > LevelCount Then Return -2

        Return Scopes(LevelCount - ScopeLevel)

    End Function
    Private Function AddScope(First As Integer, Last As Integer, Leave As String) As Integer
        ScopeCount += 1
        ReDim Preserve BraceScopes(ScopeCount)
        With BraceScopes(ScopeCount)
            .First = First
            .Last = Last
            .Leave = Leave
        End With
        Return ScopeCount
    End Function
    Private Sub ClearScopes()
        ReDim BraceScopes(0)
        ScopeCount = -1
    End Sub

    Private Sub ClearFooter()
        ReDim Footer(0)
        FooterCount = -1
    End Sub
    Private Sub AddFooter(PgName As String, LnNumber As Integer, FtData As String, UseLabel As String)
        FooterCount += 1
        ReDim Preserve Footer(FooterCount)

        With Footer(FooterCount)
            .Code = FtData
            .DeclarationLine = LnNumber
            .LabelName = UseLabel
            .PageName = PgName
        End With
    End Sub
    Private Function GatherFooterData() As String
        Dim i As Integer, ret As String

        ret = ""
        For i = 0 To FooterCount
            With Footer(i)
                If .LabelName <> "" Then ret += .LabelName + ":" + vbCrLf
                ret += .Code + vbCrLf
            End With
        Next

        Return ret
    End Function

    Public Function CompileProject(PrjName() As String, PrjData() As String, ByRef cOut As String) As Integer
        Dim MemAddr As Int32, i As Integer, Output() As String, CodeOutput() As String, rt As Integer
        Dim FootData As String, FootOutput As String, FootCode() As String, ImportsAndExtra As String

        MemAddr = 0

        ElfCfg.ResPath = ""
        ElfCfg.ResourceCount = -1
        ReDim ElfCfg.Resources(0)
        ElfCfg.Entry = 0
        ElfCfg.StackSize = 0

        ClearFooter()
        ClearLabels()
        ClearFuncs()
        ClearThreads()
        ReDim Output(PrjName.Count - 1)

        ImportsAndExtra = ProjectImportScan(PrjData)

        For i = 0 To PrjName.Count - 1
            If i = PrjName.Count - 1 Then PrjData(i) += vbCrLf + ImportsAndExtra + vbCrLf
            rt = CompileSingle(PrjData(i), Output(i), True, MemAddr, "", True)
            If rt < 0 Then
                ErrDetail.PageName = PrjName(i)
                Return rt
            End If
        Next

        MemAddr = 0

        For i = 0 To PrjName.Count - 1
            rt = CompileSingle(PrjData(i), Output(i), True, MemAddr)
            If rt < 0 Then
                ErrDetail.PageName = PrjName(i)
                Return rt
            End If
        Next

        FootData = GatherFooterData()
        If FootData <> "" Then
            ClearFooter()
            rt = CompileSingle(FootData, FootOutput, True, MemAddr, "Footer")
            If rt < 0 Then
                ErrDetail.PageName = "Footer"
                Return rt
            End If
            FootCode = Split(FootOutput + "})|%%{[NEW]%[LINE]%}%%|({", "})|%%{[NEW]%[LINE]%}%%|({")
        End If

        For i = 0 To PrjName.Count - 1
            CodeOutput = Split(Output(i) + "})|%%{[NEW]%[LINE]%}%%|({", "})|%%{[NEW]%[LINE]%}%%|({")
            rt = PatchLabels(CodeOutput)
            If rt < 0 Then Return rt
            Output(i) = Join(CodeOutput, vbCrLf)
            Do Until Strings.Right(Output(i), 2) <> vbCrLf
                Output(i) = Strings.Left(Output(i), Len(Output(i)) - 2)
            Loop
        Next
        If FootData <> "" Then
            rt = PatchLabels(FootCode)
            If rt < 0 Then Return rt
            FootOutput = Join(FootCode, vbCrLf)
            Do Until Strings.Right(FootOutput, 2) <> vbCrLf
                FootOutput = Strings.Left(FootOutput, Len(FootOutput) - 2)
            Loop
        End If

        cOut = Join(Output, vbCrLf) + vbCrLf + FootOutput
        cOut = Replace(cOut, vbCrLf + vbCrLf, vbCrLf)

        Return 0
    End Function

    Public Function CompileSingle(Src As String, ByRef cOut As String, isProj As Boolean, Optional ByRef projAddr As Integer = vbNull, Optional projPage As String = "", Optional projFncScan As Boolean = False) As Integer
        Dim Lines() As String, I As Integer, i2 As Integer, i3 As Integer, isInComments As Boolean
        Dim LineData As String, sp() As String, sp2() As String, rt As Integer, tStr As String
        Dim MemAddr As Int32, CodeRet As UInt32, tStr2 As String
        Dim CodeOutput() As String, rt2 As Integer, sp3() As String
        Dim CurrentFunc As Integer, fncScan As Boolean, myScope As Integer
        Dim PageData As String, SILData As String

        If isProj = False Then
            ElfCfg.ResPath = ""
            ElfCfg.ResourceCount = -1
            ReDim ElfCfg.Resources(0)
            ElfCfg.Entry = 0
            ElfCfg.StackSize = 0
            ClearThreads()
            SILData = SingleImportLibrary(Src)
            PageData = Src + SILData
            If SILData = "{%ERROR%}" Then
                Return ErrDetail.ErrorNumber
            End If
        Else
            PageData = Src
        End If

        fncScan = True
fncScanCompleteRestart:

        If isProj = False Then
            If fncScan = True Then ClearLabels()
            If fncScan = True Then ClearFuncs()
            MemAddr = 0
            ifsCount = 0
            switchesCount = 0
        Else
            If projFncScan = False Then fncScan = False
            MemAddr = projAddr
        End If

        Lines = Split(PageData + vbCrLf, vbCrLf)
        For I = 0 To Lines.Count - 2
            Lines(I) = stripWhiteSpace(Lines(I))
        Next

        isInComments = False
        CurrentFunc = -1
        ReDim CodeOutput(0)
        ClearScopes()

compileFooter:
        For I = 0 To Lines.Count - 2
            For i2 = 0 To ElfCfg.ResourceCount
                If MemAddr = ElfCfg.Resources(i2).Offset Then
                    MemAddr += ElfCfg.Resources(i2).Data.Length + 4
                    While ((MemAddr And 3) <> 0)
                        MemAddr += 1
                    End While
                End If
            Next

            If Strings.Left(Lines(I), 2) = "/*" Then isInComments = True
            If isInComments = True Then GoTo skipBlank
            If Lines(I) = "" Then GoTo skipBlank
            If Strings.Left(Lines(I), 2) = "//" Then GoTo skipBlank

            LineData = parseSyntax(Lines(I))
            If LineData = "" Then Return CreateError("", SyntaxError_NoSignifcantData, I, Lines(I), "")

            sp = Split(LineData + "       ", " ")
            If fncScan Then
                If LCase(sp(0)) = "extern" Then GoTo fScanSub
                If LCase(sp(0)) = "fnc" Then GoTo fScanFunc
                GoTo skipBlank
            End If

            Select Case LCase(sp(0))
                Case "elf.entry"
                    If Strings.Left(sp(1), 1) = ":" Then
                        LabeledToCodeArray(CodeOutput, MemAddr, "elf.entry " + sp(1) + "   " + I.ToString, 0)
                    Else
                        ElfCfg.Entry = GetVal(sp(1))
                    End If
                Case "resource.path"
                    tStr = Lines(I) + """"""
                    sp2 = Split(tStr, """")
                    sp3 = Split(sp2(1), """")
                    If Strings.Right(sp3(0), 1) <> "\" Then sp3(0) += "\"
                    If sp3(0) = "\" Then sp3(0) = ""
                    ElfCfg.ResPath = sp3(0)
                    debugOut("Changing resource path to '" + sp3(0) + "'")
                Case "resource"
                    tStr = Lines(I) + """"""
                    sp2 = Split(tStr, """")
                    sp3 = Split(sp2(1), """")
                    If sp3(0) = "" Then Return CreateError("", SyntaxError_BadStringDefinition, I, Lines(I), "Resources must have a valid file path")
                    If sp(2) = "" Then Return CreateError("", SyntaxError_BadLabelDefinition, I, Lines(I), "Resource must be given a name for usage")
                    tStr = sp3(0)

                    While ((MemAddr And 15) <> 0) 'Align to quad word
                        MemAddr += 4
                    End While


                    If sp(1) = "%here" Then
                        ElfCfg.ResourceCount += 1
                        ReDim Preserve ElfCfg.Resources(ElfCfg.ResourceCount)
                        While ((MemAddr And 15) <> 0)
                            MemAddr += 4
                        End While
                        With ElfCfg.Resources(ElfCfg.ResourceCount)
                            Try
                                .Data = System.IO.File.ReadAllBytes(ElfCfg.ResPath + tStr)
                            Catch
                                Return CreateError("", SyntaxError_BadStringDefinition, I, Lines(I), "File I/O Error with '" + tStr + "'")
                            End Try
                            .Offset = MemAddr
                            debugOut("Resource '" + tStr + "' loaded to " + .Offset.ToString("X8"))
                        End With
                        MemAddr += ElfCfg.Resources(ElfCfg.ResourceCount).Data.Length + 4
                        While ((MemAddr And 3) <> 0)
                            MemAddr += 1
                        End While
                        AddLabel(sp(2), ElfCfg.Resources(ElfCfg.ResourceCount).Offset)
                        debugOut("Notice: Resources will only be exported in .ELF and .RAW formats, will not display as a code due to potential excess size")
                    ElseIf sp(1) = "%append" Then
                        tStr2 = "resource %here " + sp(2) + """" + tStr + """"
                        AddFooter("", I, "memalign quad", "")
                        AddFooter("", I, tStr2, sp(2))
                    Else
                        If Strings.Left(sp(1), 1) = ":" Then
                            Return CreateError("", SyntaxError_BadLabelDefinition, I, Lines(I), "Dynamic resource placement not supported yet")
                        Else
                            ElfCfg.ResourceCount += 1
                            ReDim Preserve ElfCfg.Resources(ElfCfg.ResourceCount)
                            With ElfCfg.Resources(ElfCfg.ResourceCount)
                                Try
                                    .Data = System.IO.File.ReadAllBytes(ElfCfg.ResPath + tStr)
                                Catch
                                    Return CreateError("", SyntaxError_BadStringDefinition, I, Lines(I), "File I/O Error with '" + tStr + "'")
                                End Try
                                .Offset = GetVal(sp(1))
                                debugOut("Resource '" + tStr + "' loaded to " + .Offset.ToString("X8"))
                            End With
                            AddLabel(sp(2), ElfCfg.Resources(ElfCfg.ResourceCount).Offset)
                        End If
                        debugOut("Notice: Resources will only be exported in .ELF and .RAW formats, will not display as a code due to potential excess size")
                    End If
                Case "alloc"
                    If sp(1) = "" Then Return CreateError("", SyntaxError_BadLabelDefinition, I, Lines(I), "Must have a name for the allocated space")
                    If sp(2) = "" Then Return CreateError("", SyntaxError_BadLabelDefinition, I, Lines(I), "Memory allocation requires a size")

                    AddFooter("", I, "addradd " + sp(2), sp(1))
                Case "malloc"
                    If GetEERegVal(sp(1)) < 0 Then Return CreateError("", SyntaxError_BadLabelDefinition, I, Lines(I), "Must have a register for the allocated space")
                    If sp(2) = "" Then Return CreateError("", SyntaxError_BadLabelDefinition, I, Lines(I), "Memory allocation requires a size")

                    AddFooter("", I, "memalign quad", "")
                    AddFooter("", I, "addradd " + sp(2), "_malloc_%" + MemAddr.ToString + "%" + I.ToString + "%" + sp(1) + "%")

                    tStr = "setreg " + sp(1) + ", :" + "_malloc_%" + MemAddr.ToString + "%" + I.ToString + "%" + sp(1) + "%  " + I.ToString
                    LabeledToCodeArray(CodeOutput, MemAddr, tStr, 8)
                Case "memalign"
                    sp(1) = LCase(sp(1))
                    If sp(1) = "double" Then
                        While ((MemAddr And 7) <> 0)
                            'FormatToCodeArray(CodeOutput, MemAddr, 0)
                            MemAddr += 4
                        End While
                        'If (MemAddr And 7) <> 0 Then
                        '    MemAddr = ((MemAddr \ 8) * 8) + 8
                        'End If
                    ElseIf sp(1) = "quad" Then
                        While ((MemAddr And 15) <> 0)
                            'FormatToCodeArray(CodeOutput, MemAddr, 0)
                            MemAddr += 4
                        End While
                        'If (MemAddr And 15) <> 0 Then
                        'MemAddr = ((MemAddr \ 16) * 16) + 16
                        'End If

                    End If
                Case "thread"
                Case "prochook"
                Case "hook"
                    'hook %me $00100000 -j
                    'hook labelname $00100000 -jal
                    'hook labelname $00100000 -pointer

                    If LCase(Strings.Right("00000000" + Hex(Val(Replace(sp(1), "$", "&h0"))), 8)) = LCase(Strings.Right("00000000" + Replace(sp(1), "$", ""), 8)) Then
                        i2 = Val(Replace(sp(1), "$", "&h0"))
                        LabeledToCodeArray(CodeOutput, i2, "hook " + sp(1) + " " + sp(2) + " " + sp(3) + " " + I.ToString, 0)
                    ElseIf LCase(sp(1)) = "%me" Then
                        tStr = CodeFormat + Strings.Right("00000000" + Hex(Val(Replace(sp(2), "$", "&h0"))), 7)
                        If LCase(sp(2)) = "-j" Then
                            rt = mpAsm.AssembleInstruction("j " + MemAddr.ToString, CodeRet)
                            tStr2 = Strings.Right("00000000" + Hex(CodeRet), 8)
                        ElseIf LCase(sp(2)) = "-jal" Then
                            rt = mpAsm.AssembleInstruction("jal " + MemAddr.ToString, CodeRet)
                            tStr2 = Strings.Right("00000000" + Hex(CodeRet), 8)
                        ElseIf LCase(sp(2)) = "-pointer" Then
                            tStr2 = Strings.Right("00000000" + Hex(MemAddr), 8)
                        Else
                            rt = mpAsm.AssembleInstruction("j " + MemAddr.ToString, CodeRet)
                            tStr2 = Strings.Right("00000000" + Hex(CodeRet), 8)
                        End If
                        AppendToCodeArray(CodeOutput, tStr, tStr2)
                    Else
                        LabeledToCodeArray(CodeOutput, MemAddr, "hook " + sp(1) + " " + sp(2) + " " + sp(3) + " " + I.ToString, 0)
                    End If

                Case "code"
                    If Len(sp(1)) <> 8 Then Return CreateError("", SyntaxError_DataTypeUnknown, I, Lines(I), "Unkown data")
                    If Len(sp(2)) <> 8 Then Return CreateError("", SyntaxError_DataTypeUnknown, I, Lines(I), "Unkown data")
                    If LCase(Strings.Right("00000000" + Hex(Val("&H" + sp(1))), 8)) <> LCase(sp(1)) Then Return CreateError("", SyntaxError_DataTypeUnknown, I, Lines(I), "Unkown data")
                    If LCase(Strings.Right("00000000" + Hex(Val("&H" + sp(2))), 8)) <> LCase(sp(2)) Then Return CreateError("", SyntaxError_DataTypeUnknown, I, Lines(I), "Unkown data")
                    AppendToCodeArray(CodeOutput, sp(1), sp(2))
                Case "include"
                Case "import"
                Case "event"
                Case "thread.sleep"
                    'thread.sleep(me, 1000)
                    i2 = -1
                    If LCase(sp(1)) = "me" Then
                        If CurrentFunc < 0 Then Return CreateError("", SyntaxError_BadLabelDefinition, I, Lines(I), "Not inside a thread")
                        i2 = GetThreadID(FuncsAndSubs(CurrentFunc).FncName)
                    Else
                        i2 = GetThreadID(sp(1))
                    End If
                    If i2 < 0 Then Return CreateError("", SyntaxError_BadLabelDefinition, I, Lines(I), "Thread '" + sp(1) + "' could not be found")

                    tStr = "addiu sp, sp, $ffe0" + vbCrLf +
                            "sq t8, $0000(sp)" + vbCrLf +
                            "sq t9, $0010(sp)" + vbCrLf

                    tStr += GenerateSetRegCode(Val(Replace(sp(2), "$", "&h0")), "t8") + vbCrLf
                    tStr += "ori at, zero, " + i2.ToString + vbCrLf
                    tStr += "sll at, at, 4"
                    AddGeneratedToOutput(tStr, MemAddr, CodeOutput, I)

                    tStr = "setreg t9, :_Main_Process_Thread_Table  " + I.ToString
                    LabeledToCodeArray(CodeOutput, MemAddr, tStr, 8)

                    tStr = "addu t9, at, t9" + vbCrLf
                    tStr += "sw t8, $0004(t9)" + vbCrLf
                    tStr += "sw zero, $0008(t9)" + vbCrLf

                    tStr += "lq t8, $0000(sp)" + vbCrLf +
                            "lq t9, $0010(sp)" + vbCrLf +
                            "addiu sp, sp, $0020"
                    AddGeneratedToOutput(tStr, MemAddr, CodeOutput, I)

                Case "thread.wakeup"
                    i2 = -1
                    If LCase(sp(1)) = "me" Then
                        If CurrentFunc < 0 Then Return CreateError("", SyntaxError_BadLabelDefinition, I, Lines(I), "Not inside a thread")
                        i2 = GetThreadID(FuncsAndSubs(CurrentFunc).FncName)
                    Else
                        i2 = GetThreadID(sp(1))
                    End If
                    If i2 < 0 Then Return CreateError("", SyntaxError_BadLabelDefinition, I, Lines(I), "Thread '" + sp(1) + "' could not be found")

                    tStr = "addiu sp, sp, $fff0" + vbCrLf +
                            "sq t9, $0000(sp)" + vbCrLf

                    'tStr += GenerateSetRegCode(Val(Replace(sp(2), "$", "&h0")), "t8") + vbCrLf
                    tStr += "ori at, zero, " + i2.ToString + vbCrLf
                    tStr += "sll at, at, 4"
                    AddGeneratedToOutput(tStr, MemAddr, CodeOutput, I)

                    tStr = "setreg t9, :_Main_Process_Thread_Table  " + I.ToString
                    LabeledToCodeArray(CodeOutput, MemAddr, tStr, 8)

                    tStr = "addu t9, at, t9" + vbCrLf
                    tStr += "sw zero, $0004(t9)" + vbCrLf
                    tStr += "sw zero, $0008(t9)" + vbCrLf

                    tStr += "lq t9, $0000(sp)" + vbCrLf +
                            "addiu sp, sp, $0010"
                    AddGeneratedToOutput(tStr, MemAddr, CodeOutput, I)

                Case "thread.stop"
                    i2 = -1
                    If LCase(sp(1)) = "me" Then
                        If CurrentFunc < 0 Then Return CreateError("", SyntaxError_BadLabelDefinition, I, Lines(I), "Not inside a thread")
                        i2 = GetThreadID(FuncsAndSubs(CurrentFunc).FncName)
                    Else
                        i2 = GetThreadID(sp(1))
                    End If
                    If i2 < 0 Then Return CreateError("", SyntaxError_BadLabelDefinition, I, Lines(I), "Thread '" + sp(1) + "' could not be found")

                    tStr = "addiu sp, sp, $fff0" + vbCrLf +
                            "sq t9, $0000(sp)" + vbCrLf

                    'tStr += GenerateSetRegCode(Val(Replace(sp(2), "$", "&h0")), "t8") + vbCrLf
                    tStr += "ori at, zero, " + i2.ToString + vbCrLf
                    tStr += "sll at, at, 4"
                    AddGeneratedToOutput(tStr, MemAddr, CodeOutput, I)

                    tStr = "setreg t9, :_Main_Process_Thread_Table  " + I.ToString
                    LabeledToCodeArray(CodeOutput, MemAddr, tStr, 8)

                    tStr = "addu t9, at, t9" + vbCrLf
                    tStr += "sw zero, $000c(t9)" + vbCrLf

                    tStr += "lq t9, $0000(sp)" + vbCrLf +
                            "addiu sp, sp, $0010"
                    AddGeneratedToOutput(tStr, MemAddr, CodeOutput, I)

                Case "thread.start"
                    i2 = -1
                    If LCase(sp(1)) = "me" Then
                        If CurrentFunc < 0 Then Return CreateError("", SyntaxError_BadLabelDefinition, I, Lines(I), "Not inside a thread")
                        i2 = GetThreadID(FuncsAndSubs(CurrentFunc).FncName)
                    Else
                        i2 = GetThreadID(sp(1))
                    End If
                    If i2 < 0 Then Return CreateError("", SyntaxError_BadLabelDefinition, I, Lines(I), "Thread '" + sp(1) + "' could not be found")

                    tStr = "addiu sp, sp, $fff0" + vbCrLf +
                            "sq t9, $0000(sp)" + vbCrLf

                    'tStr += GenerateSetRegCode(Val(Replace(sp(2), "$", "&h0")), "t8") + vbCrLf
                    tStr += "ori at, zero, " + i2.ToString + vbCrLf
                    tStr += "sll at, at, 4"
                    AddGeneratedToOutput(tStr, MemAddr, CodeOutput, I)

                    tStr = "setreg t9, :_Main_Process_Thread_Table  " + I.ToString
                    LabeledToCodeArray(CodeOutput, MemAddr, tStr, 8)

                    tStr = "addu t9, at, t9" + vbCrLf
                    tStr += "addiu at, zero, 1" + vbCrLf
                    tStr += "sw at, $000c(t9)" + vbCrLf

                    tStr += "lq t9, $0000(sp)" + vbCrLf +
                            "addiu sp, sp, $0010"
                    AddGeneratedToOutput(tStr, MemAddr, CodeOutput, I)

                Case "address"
                    MemAddr = Val(Replace(sp(1), "$", "&H0"))
                    GoTo skipBlank
                Case "addradd"
                    MemAddr += ((Val(Replace(sp(1), "$", "&H0")) \ 4) * 4) + 4
                    GoTo skipBlank
                Case "define"
                    Dim u32 As UInt32
                    u32 = GetVal(sp(2))
                    If u32 > &H7FFFFFFF Then
                        i2 = 0 - (&H100000000 - u32)
                    Else
                        i2 = u32
                    End If
                    rt = AddLabel(Replace(sp(1), ":", ""), i2)
                    If rt < 0 Then Return CreateError("", SyntaxError_BadLabelDefinition, I, Lines(I), "")

                    GoTo skipBlank
                Case "hexcode"
                    If Strings.Left(sp(1), 1) = ":" Then
                        LabeledToCodeArray(CodeOutput, MemAddr, sp(0) + " " + sp(1) + "   " + I.ToString, 4)
                    Else
                        FormatToCodeArray(CodeOutput, MemAddr, GetVal(sp(1))) 'CDec(Replace(sp(1), "$", "&H0")))
                    End If
                Case "hexfloat"
                    If Strings.Left(sp(1), 1) = ":" Then
                        LabeledToCodeArray(CodeOutput, MemAddr, sp(0) + " " + sp(1) + "   " + I.ToString, 4)
                    Else
                        Flt32.f32 = CDec(Replace(sp(1), "$", "&H0"))
                        FormatToCodeArray(CodeOutput, MemAddr, Flt32.u32)
                    End If
                Case "string"
                    If sp(1) <> "" Then
                        tStr = ""
                        If Strings.Left(sp(2), 1) = """" Then
                            i2 = 1
                            Do
                                i2 += 1
                            Loop Until Strings.Mid(Lines(I), i2, 1) = """"
                            Do
                                rt = 0
                                tStr += Strings.Mid(Lines(I), i2, 1)
                                i2 += 1
                            Loop Until i2 > Len(Lines(I)) Or Mid(Lines(I), i2, 1) = """"
                            If Mid(Lines(I), i2, 1) = """" Then
                                tStr += """"
                                AddFooter("", I, "print " + tStr, sp(1))
                            Else
                                Return CreateError("", SyntaxError_BadStringDefinition, I, Lines(I), "Strings must end with a """)
                            End If
                        Else
                            Return CreateError("", SyntaxError_BadStringDefinition, I, Lines(I), "Strings must start with a """)
                        End If
                    Else
                        Return CreateError("", SyntaxError_BadStringDefinition, I, Lines(I), "String definition must have a name")
                    End If
                Case "print"
                    tStr = ""
                    If Strings.Left(sp(1), 1) = """" Then
                        i2 = Len(sp(0)) + 3
                        Do
                            rt = 0
                            If Mid(Lines(I), i2, 2) = "\a" Then
                                tStr += "07"
                                rt = 1
                            End If
                            If Mid(Lines(I), i2, 2) = "\b" Then
                                tStr += "08"
                                rt = 1
                            End If
                            If Mid(Lines(I), i2, 2) = "\f" Then
                                tStr += "0c"
                                rt = 1
                            End If
                            If Mid(Lines(I), i2, 2) = "\n" Then
                                tStr += "0a"
                                rt = 1
                            End If
                            If Mid(Lines(I), i2, 2) = "\r" Then
                                tStr += "0d"
                                rt = 1
                            End If
                            If Mid(Lines(I), i2, 2) = "\t" Then
                                tStr += "09"
                                rt = 1
                            End If
                            If Mid(Lines(I), i2, 2) = "\v" Then
                                tStr += "0b"
                                rt = 1
                            End If
                            If Mid(Lines(I), i2, 2) = "\\" Then
                                tStr += "5c"
                                rt = 1
                            End If
                            If Mid(Lines(I), i2, 2) = "\'" Then
                                tStr += "27"
                                rt = 1
                            End If
                            If Mid(Lines(I), i2, 3) = "\''" Then
                                tStr += "22"
                                rt = 2
                            End If
                            If rt = 0 Then
                                tStr += Strings.Right("00" + Hex(Asc(Mid(Lines(I), i2, 1))), 2)
                                i2 += 1
                            Else
                                i2 += rt + 1
                            End If
                        Loop Until i2 > Len(Lines(I)) Or Mid(Lines(I), i2, 1) = """"
                        If Mid(Lines(I), i2, 1) = """" Then
                            tStr += "00"
                            Do
                                tStr2 = Strings.Left(tStr + "00000000", 8)
                                tStr2 = Mid(tStr2, 7, 2) +
                                        Mid(tStr2, 5, 2) +
                                        Mid(tStr2, 3, 2) +
                                        Mid(tStr2, 1, 2)

                                FormatToCodeArray(CodeOutput, MemAddr, CDec("&H" + tStr2))
                                If Len(tStr) > 8 Then
                                    tStr = Strings.Right(tStr, Len(tStr) - 8)
                                Else
                                    tStr = ""
                                End If
                            Loop Until Len(tStr) <= 0
                        Else
                            Return CreateError("", SyntaxError_BadStringDefinition, I, Lines(I), "Strings must end with a """)
                        End If
                    Else
                        Return CreateError("", SyntaxError_BadStringDefinition, I, Lines(I), "Strings must start with a """)
                    End If

                Case "setreg"
                    If Strings.Left(sp(2), 1) = ":" Then
                        LabeledToCodeArray(CodeOutput, MemAddr, sp(0) + " " + sp(1) + " " + sp(2) + " " + sp(3) + " " + I.ToString, 8)
                    Else
                        sp(2) = Strings.Right("00000000" + Hex(Val(Replace(sp(2), "$", "&H0"))), 8)
                        rt = mpAsm.AssembleInstruction("lui " + sp(1) + ", $" + Strings.Left(sp(2), 4), CodeRet)
                        If rt < 0 Then GoTo AssemblerErrorHandle
                        FormatToCodeArray(CodeOutput, MemAddr, CodeRet)

                        rt = mpAsm.AssembleInstruction("ori " + sp(1) + ", " + sp(1) + ", $" + Strings.Right(sp(2), 4), CodeRet)
                        If rt < 0 Then GoTo AssemblerErrorHandle
                        FormatToCodeArray(CodeOutput, MemAddr, CodeRet)

                    End If
                Case "setfpr"
                    If Strings.Left(sp(2), 1) = ":" Then
                        LabeledToCodeArray(CodeOutput, MemAddr, "setreg at " + sp(2) + " " + sp(3) + " " + I.ToString, 8)

                        rt = mpAsm.AssembleInstruction("mtc1 at, " + sp(1), CodeRet)
                        If rt < 0 Then GoTo AssemblerErrorHandle
                        FormatToCodeArray(CodeOutput, MemAddr, CodeRet)
                    Else
                        sp(2) = Strings.Right("00000000" + Hex(Val(Replace(sp(2), "$", "&H0"))), 8)

                        rt = mpAsm.AssembleInstruction("lui at, $" + Strings.Left(sp(2), 4), CodeRet)
                        FormatToCodeArray(CodeOutput, MemAddr, CodeRet)

                        rt = mpAsm.AssembleInstruction("ori at, at, $" + Strings.Right(sp(2), 4), CodeRet)
                        FormatToCodeArray(CodeOutput, MemAddr, CodeRet)

                        rt = mpAsm.AssembleInstruction("mtc1 at, " + sp(1), CodeRet)
                        If rt < 0 Then GoTo AssemblerErrorHandle
                        FormatToCodeArray(CodeOutput, MemAddr, CodeRet)

                    End If
                Case "setfloat"
                    If Strings.Left(sp(2), 1) = ":" Then
                        LabeledToCodeArray(CodeOutput, MemAddr, "setreg at " + sp(2) + " " + sp(3) + " " + I.ToString, 8)

                        rt = mpAsm.AssembleInstruction("mtc1 at, " + sp(1), CodeRet)
                        If rt < 0 Then GoTo AssemblerErrorHandle
                        FormatToCodeArray(CodeOutput, MemAddr, CodeRet)

                        rt = mpAsm.AssembleInstruction("cvt.s.w " + sp(1) + ", " + sp(1), CodeRet)
                        If rt < 0 Then GoTo AssemblerErrorHandle
                        FormatToCodeArray(CodeOutput, MemAddr, CodeRet)
                    Else
                        sp(2) = Strings.Right("00000000" + Hex(Val(Replace(sp(2), "$", "&H0"))), 8)

                        rt = mpAsm.AssembleInstruction("lui at, $" + Strings.Left(sp(2), 4), CodeRet)
                        FormatToCodeArray(CodeOutput, MemAddr, CodeRet)

                        rt = mpAsm.AssembleInstruction("ori at, at, $" + Strings.Right(sp(2), 4), CodeRet)
                        FormatToCodeArray(CodeOutput, MemAddr, CodeRet)

                        rt = mpAsm.AssembleInstruction("mtc1 at, " + sp(1), CodeRet)
                        If rt < 0 Then GoTo AssemblerErrorHandle
                        FormatToCodeArray(CodeOutput, MemAddr, CodeRet)

                        rt = mpAsm.AssembleInstruction("cvt.s.w " + sp(1) + ", " + sp(1), CodeRet)
                        If rt < 0 Then GoTo AssemblerErrorHandle
                        FormatToCodeArray(CodeOutput, MemAddr, CodeRet)
                    End If
                Case "padding"
                    For i2 = 1 To Val(Replace(sp(1), "$", "&H0")) \ 4
                        FormatToCodeArray(CodeOutput, MemAddr, 0)
                    Next
                Case "goto"
                    If Strings.Left(sp(1), 1) = ":" Then
                        LabeledToCodeArray(CodeOutput, MemAddr, "goto " + sp(1) + "   " + I.ToString, 8)
                    Else
                        rt = mpAsm.AssembleInstruction("j " + sp(1), CodeRet)
                        If rt < 0 Then GoTo AssemblerErrorHandle
                        FormatToCodeArray(CodeOutput, MemAddr, CodeRet)
                        FormatToCodeArray(CodeOutput, MemAddr, 0)
                    End If
                Case "call"
                    'call name(a0, a1, a2..)
                    Dim fIndx As Integer, didCallJal As Boolean, doJalInject As Boolean

                    fIndx = FindFuncOrSub(sp(1))
                    If fIndx < 0 Then Return CreateError("", SyntaxError_LabelNotFound, I, Lines(I), "Function has not been declared")

                    doJalInject = False
                    didCallJal = False
                    i2 = 2
                    i3 = 0
                    Do Until sp(i2) = ""
                        With FuncsAndSubs(fIndx)
                            If i3 > .ArgTypes.Count - 1 Then Return CreateError("", SyntaxError_BadArgumentCount, I, Lines(I), "Wrong number of arguments for this function")
                            If .ArgTypes(i3) <> "" And sp(i2) = "" Then Return CreateError("", SyntaxError_BadArgumentCount, I, Lines(I), "Wrong number of arguments for this function")

                            If .ArgTypes(i3) <> "" And sp(i2) <> "" Then
                                If i3 = (.ArgTypes.Count - 1) And sp(i2 + 1) = "" Then doJalInject = True
                                'LabeledToCodeArray(CodeOutput, MemAddr, "jal :" + .FncName + "   " + I.ToString, 4)
                                'didCallJal = True
                                'End If

                                If .ArgTypes(i3) = "EE" Then
                                    If GetEERegVal(sp(i2)) < 0 Then
                                        If GetCOP1RegVal(sp(i2)) >= 0 Then Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Invalid input argument type")
                                        If GetCOP0RegVal(sp(i2)) >= 0 Then Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Invalid input argument type")
                                        If Strings.Left(sp(i2), 1) = ":" Then
                                            LabeledToCodeArray(CodeOutput, MemAddr, "setreg " + .ArgRegs(i3) + " " + sp(i2) + "  " + I.ToString, 8)
                                        Else
                                            tStr = GenerateSetRegCode(Val(Replace(sp(i2), "$", "&h0")), .ArgRegs(i3))
                                            If doJalInject Then
                                                sp2 = Split(tStr + vbCrLf, vbCrLf)
                                                If sp2.Count > 2 Then
                                                    tStr = sp2(0) + vbCrLf + "jal :" + .FncName + vbCrLf + sp2(1)
                                                Else
                                                    tStr = "jal :" + .FncName + vbCrLf + tStr
                                                End If
                                                didCallJal = True
                                            End If
                                            AddGeneratedToOutput(tStr, MemAddr, CodeOutput, I)
                                        End If
                                    Else
                                        If LCase(sp(i2)) <> LCase(.ArgRegs(i3)) Then
                                            rt = mpAsm.AssembleInstruction("daddu " + .ArgRegs(i3) + ", " + sp(i2) + ", zero", CodeRet)
                                            If rt < 0 Then Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Unknown reason for being here")
                                            FormatToCodeArray(CodeOutput, MemAddr, CodeRet)
                                        End If
                                    End If
                                ElseIf .ArgTypes(i3) = "COP1" Then
                                    If GetCOP1RegVal(sp(i2)) < 0 Then Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Invalid input argument type")
                                    rt = mpAsm.AssembleInstruction("mov.s " + .ArgRegs(i3) + ", " + sp(i2), CodeRet)
                                    If rt < 0 Then Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Unknown reason for being here")
                                    FormatToCodeArray(CodeOutput, MemAddr, CodeRet)
                                Else
                                    Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Function was somehow declared with an improper type")
                                End If
                            Else
                                Return CreateError("", SyntaxError_BadArgumentCount, I, Lines(I), "Wrong number of arguments for this function")
                            End If
                        End With
                        i3 += 1
                        i2 += 1
                    Loop
                    If i2 = 2 And i3 = 0 Then
                        If FuncsAndSubs(fIndx).ArgTypes(i3) <> "" And sp(i2) = "" Then Return CreateError("", SyntaxError_BadArgumentCount, I, Lines(I), "Wrong number of arguments for this function")
                    End If
                    If didCallJal = False Then
                        LabeledToCodeArray(CodeOutput, MemAddr, "jal :" + FuncsAndSubs(fIndx).FncName + "   " + I.ToString, 4)
                        FormatToCodeArray(CodeOutput, MemAddr, 0)
                    End If

                Case "fnc"
fScanFunc:
                    'fnc name(ee a0, ee a1, cop1 f0...)\s0, s1, s2, s3...
                    If Lines(I + 1) <> "{" Then Return CreateError("", SyntaxError_BracingError, I, Lines(I), "Function declaration must be followed by open brace '{'")
                    If fncScan = False Then Lines(I + 1) = ""

                    sp3 = Split(Lines(I) + "\", "\")
                    sp = Split(parseSyntax(sp3(0)) + "      ", " ")
                    sp2 = Split(parseSyntax(sp3(1)) + "      ", " ")

                    Dim fncTypes() As String, fncRegs() As String, fncRCount As Integer, fIndx As Integer
                    Dim fncPreserves() As String, fncRestores() As String, fncPreRes As Integer
                    Dim fncHasNoPres As Boolean

                    fncHasNoPres = False
                    ReDim fncTypes(0)
                    ReDim fncRegs(0)
                    fncRCount = -1
                    i2 = 2
                    Do Until sp(i2) = ""
                        fncRCount += 1
                        ReDim Preserve fncTypes(fncRCount)
                        ReDim Preserve fncRegs(fncRCount)
                        If LCase(sp(i2)) = "ee" Then
                            fncTypes(fncRCount) = "EE"
                            If GetEERegVal(sp(i2 + 1)) < 0 Then Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Register type is different from declaration")
                            fncRegs(fncRCount) = sp(i2 + 1)
                        ElseIf LCase(sp(i2)) = "cop1" Then
                            fncTypes(fncRCount) = "EE"
                            If GetCOP1RegVal(sp(i2 + 1)) < 0 Then Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Register type is different from declaration")
                            fncRegs(fncRCount) = sp(i2 + 1)
                        ElseIf LCase(sp(i2)) = "void" Then

                        Else
                            Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Register type was not defined; EX: fnc myFunc(EE a0, EE a1)")
                        End If
                        i2 += 2
                    Loop
                    fIndx = AddFuncOrSub(sp(1), MemAddr, fncTypes, fncRegs)
                    If fIndx < 0 Then Return CreateError("", SyntaxError_DuplicateLabel, I, Lines(I), "Function name has already been defined")

                    CurrentFunc = fIndx
                    ReDim fncPreserves(0)
                    ReDim fncRestores(0)
                    fncPreRes = 0

                    fncPreserves(0) = "sq ra, $" + Strings.Right("0000" + Hex(fncPreRes), 4) + "(sp)"
                    fncRestores(0) = "lq ra, $" + Strings.Right("0000" + Hex(fncPreRes), 4) + "(sp)"
                    fncPreRes += &H10

                    i2 = 0
                    Do Until sp2(i2) = ""
                        ReDim Preserve fncPreserves(fncPreserves.Count)
                        ReDim Preserve fncRestores(fncRestores.Count)

                        If GetEERegVal(sp2(i2)) > -1 Then
                            fncPreserves(fncPreserves.Count - 1) = "sq " + sp2(i2) + ", $" + Strings.Right("0000" + Hex(fncPreRes), 4) + "(sp)"
                            fncRestores(fncRestores.Count - 1) = "lq " + sp2(i2) + ", $" + Strings.Right("0000" + Hex(fncPreRes), 4) + "(sp)"
                            fncPreRes += &H10
                        Else
                            If GetCOP1RegVal(sp2(i2)) > -1 Then
                                fncPreserves(fncPreserves.Count - 1) = "swc1 " + sp2(i2) + ", $" + Strings.Right("0000" + Hex(fncPreRes), 4) + "(sp)"
                                fncRestores(fncRestores.Count - 1) = "lwc1 " + sp2(i2) + ", $" + Strings.Right("0000" + Hex(fncPreRes), 4) + "(sp)"
                                fncPreRes += &H10
                            Else
                                If LCase(sp2(i2)) = "ee-all" Then
                                    Dim tmpI As Integer, tmpI2 As Integer, tmpI3 As Integer
                                    ReDim fncPreserves(0)
                                    ReDim fncRestores(0)
                                    tmpI2 = 1
                                    tmpI3 = 0
                                    For tmpI = 0 To 30
                                        If GetEERegStr(tmpI2) <> "sp" Then
                                            ReDim Preserve fncPreserves(tmpI3)
                                            ReDim Preserve fncRestores(tmpI3)
                                            fncPreserves(fncPreserves.Count - 1) = "sq " + GetEERegStr(tmpI2) + ", $" + Strings.Right("0000" + Hex(fncPreRes), 4) + "(sp)"
                                            fncRestores(fncRestores.Count - 1) = "lq " + GetEERegStr(tmpI2) + ", $" + Strings.Right("0000" + Hex(fncPreRes), 4) + "(sp)"
                                            fncPreRes += &H10
                                            tmpI3 += 1
                                        End If
                                        tmpI2 += 1
                                    Next
                                ElseIf LCase(sp2(i2)) = "cop1-all" Then
                                    Dim tmpI As Integer
                                    ReDim fncPreserves(31)
                                    ReDim fncPreserves(31)
                                    For tmpI = 1 To 31
                                        fncPreserves(tmpI) = "swc1 " + GetCOP1RegStr(tmpI) + ", $" + Strings.Right("0000" + Hex(fncPreRes), 4) + "(sp)"
                                        fncPreserves(tmpI) = "lwc1 " + GetCOP1RegStr(tmpI) + ", $" + Strings.Right("0000" + Hex(fncPreRes), 4) + "(sp)"
                                        fncPreRes += &H10
                                    Next
                                ElseIf LCase(sp2(i2)) = "-none" Then
                                    ReDim fncPreserves(0)
                                    ReDim fncRestores(0)
                                    fncHasNoPres = True
                                Else
                                    Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Preservation registers must be EE or COP1")
                                End If
                            End If
                        End If

                        i2 += 1
                    Loop
                    If fncHasNoPres = False Then
                        With FuncsAndSubs(fIndx)
                            .FncType = 0

                            ReDim .Preserves(fncPreserves.Count)
                            ReDim .Restores(fncRestores.Count + 1)

                            .Preserves(0) = "addiu sp, sp, $" + Strings.Right("0000" + Hex(&H10000 - fncPreRes), 4)

                            rt = mpAsm.AssembleInstruction(.Preserves(0), CodeRet)
                            If rt < 0 Then Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Unknown reason for being here")
                            FormatToCodeArray(CodeOutput, MemAddr, CodeRet)
                            For i2 = 0 To fncPreserves.Count - 1

                                .Preserves(i2 + 1) = fncPreserves(i2)
                                .Restores(i2) = fncRestores(i2)

                                rt = mpAsm.AssembleInstruction(fncPreserves(i2), CodeRet)
                                If rt < 0 Then Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Unknown reason for being here")
                                FormatToCodeArray(CodeOutput, MemAddr, CodeRet)
                            Next

                            .Restores(.Restores.Count - 2) = "jr ra"
                            .Restores(.Restores.Count - 1) = "addiu sp, sp, $" + Strings.Right("0000" + Hex(fncPreRes), 4)
                        End With
                    Else
                        With FuncsAndSubs(fIndx)
                            ReDim .Restores(1)
                            .Restores(0) = "jr ra"
                            .Restores(1) = "nop"
                        End With
                    End If


                    'Check for closing brace syntax
                    rt = 0
                    For i2 = I + 2 To Lines.Count - 1
                        If Strings.Left(LCase(Lines(i2)), 3) = "fnc" Then Return CreateError("", SyntaxError_BadLabelDefinition, i2, Lines(i2), "Functions cannot be declared inside another function")
                        If Lines(i2) = "{" Then rt += 1
                        If Lines(i2) = "}" Then
                            If rt = 0 Then GoTo fncDeclarationProcess1
                            rt -= 1
                        End If
                    Next
                    Return CreateError("", SyntaxError_BracingError, I, Lines(I), "Function declaration must end with a closing brace '}'")
fncDeclarationProcess1:
                    Lines(i2) = "endfunc " + sp(1)
                    'MsgBox(CurrentFunc.ToString + vbCrLf + FuncsAndSubs(CurrentFunc).FncName)

                    'Function is now opened / set up
                Case "return"
                    If CurrentFunc < 0 Then Return CreateError("", SyntaxError_LabelNotFound, I, Lines(I), "Cannot return from undeclared function")
                    If sp(1) = "" Then Return CreateError("", SyntaxError_BadArgumentCount, I, Lines(I), "Return must contain a value")

                    If Strings.Left(sp(1), 1) = ":" Then
                        LabeledToCodeArray(CodeOutput, MemAddr, "setreg v0, " + sp(1) + "  " + I.ToString, 8)
                        LabeledToCodeArray(CodeOutput, MemAddr, "beq zero, zero, :" + "%exit%func_%" + FuncsAndSubs(CurrentFunc).FncName + "%" + " " + I.ToString, 4)
                    Else
                        If GetEERegVal(sp(1)) < 0 Then
                            i2 = Val(Replace(sp(1), "$", "&H0"))
                            If i2 > -1 And i2 < 65536 Then
                                LabeledToCodeArray(CodeOutput, MemAddr, "beq zero, zero, :" + "%exit%func_%" + FuncsAndSubs(CurrentFunc).FncName + "%" + " " + I.ToString, 4)
                                rt = mpAsm.AssembleInstruction("ori v0, zero, " + i2.ToString, CodeRet)
                                If rt < 0 Then Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Unkown reason to be here")
                                FormatToCodeArray(CodeOutput, MemAddr, CodeRet)
                            ElseIf i2 < &HFFFF8000 Or i2 > &H7FFF Then
                                sp(2) = Strings.Right("00000000" + Hex(i2), 8)

                                rt = mpAsm.AssembleInstruction("lui v0, $" + Strings.Left(sp(2), 4), CodeRet)
                                If rt < 0 Then Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Unkown reason to be here")
                                FormatToCodeArray(CodeOutput, MemAddr, CodeRet)

                                LabeledToCodeArray(CodeOutput, MemAddr, "beq zero, zero, :" + "%exit%func_%" + FuncsAndSubs(CurrentFunc).FncName + "%" + " " + I.ToString, 4)

                                rt = mpAsm.AssembleInstruction("ori v0, v0, " + Strings.Right(sp(2), 4), CodeRet)
                                If rt < 0 Then Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Unkown reason to be here")
                                FormatToCodeArray(CodeOutput, MemAddr, CodeRet)
                            Else
                                LabeledToCodeArray(CodeOutput, MemAddr, "beq zero, zero, :" + "%exit%func_%" + FuncsAndSubs(CurrentFunc).FncName + "%" + " " + I.ToString, 4)
                                rt = mpAsm.AssembleInstruction("addiu v0, zero, " + i2.ToString, CodeRet)
                                If rt < 0 Then Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Unkown reason to be here")
                                FormatToCodeArray(CodeOutput, MemAddr, CodeRet)
                            End If
                        Else
                            LabeledToCodeArray(CodeOutput, MemAddr, "beq zero, zero, :" + "%exit%func_%" + FuncsAndSubs(CurrentFunc).FncName + "%" + " " + I.ToString, 4)
                            rt = mpAsm.AssembleInstruction("daddu v0, " + sp(1) + ", zero", CodeRet)
                            If rt < 0 Then Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Unkown reason to be here")
                            FormatToCodeArray(CodeOutput, MemAddr, CodeRet)
                        End If
                    End If

                Case "endfunc"
                    Dim fIndx As Integer
                    CurrentFunc = -1
                    fIndx = FindFuncOrSub(sp(1))
                    If fIndx < 0 Then Return CreateError("", SyntaxError_LabelNotFound, I, Lines(I), "Attempted to close an undeclared function?")


                    With FuncsAndSubs(fIndx)
                        rt = AddLabel("%exit%func_%" + .FncName + "%", MemAddr)
                        If rt < 0 Then Return CreateError("", SyntaxError_BadLabelDefinition, I, Lines(I), "Unkonwn reason for being here")

                        'debug generate restoration
                        For i2 = 0 To .Restores.Count - 1
                            rt = mpAsm.AssembleInstruction(.Restores(i2), CodeRet)
                            If rt < 0 Then Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Unknown reason for being here")
                            FormatToCodeArray(CodeOutput, MemAddr, CodeRet)
                        Next
                    End With
                Case "extern"
fScanSub:
                    'subroutine $00100000 name(a0, a1, a2...)
                    Dim fncTypes() As String, fncRegs() As String, fncRCount As Integer, fIndx As Integer

                    ReDim fncTypes(0)
                    ReDim fncRegs(0)
                    fncRCount = -1
                    i2 = 3
                    Do Until sp(i2) = ""
                        fncRCount += 1
                        ReDim Preserve fncTypes(fncRCount)
                        ReDim Preserve fncRegs(fncRCount)
                        If LCase(sp(i2)) = "ee" Then
                            fncTypes(fncRCount) = "EE"
                            If GetEERegVal(sp(i2 + 1)) < 0 Then Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Register type is different from declaration")
                            fncRegs(fncRCount) = sp(i2 + 1)
                        ElseIf LCase(sp(i2)) = "cop1" Then
                            fncTypes(fncRCount) = "EE"
                            If GetCOP1RegVal(sp(i2 + 1)) < 0 Then Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Register type is different from declaration")
                            fncRegs(fncRCount) = sp(i2 + 1)
                        ElseIf LCase(sp(i2)) = "void" Then

                        End If
                        i2 += 2
                    Loop
                    If Left(sp(1), 1) = ":" Then Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "External functions must have a static address")
                    If sp(1) = "%thisaddr" Then sp(1) = "$" + Hex(MemAddr)

                    fIndx = AddFuncOrSub(sp(2), Val(Replace(sp(1), "$", "&h0")), fncTypes, fncRegs)
                    If fIndx < 0 Then Return CreateError("", SyntaxError_DuplicateLabel, I, Lines(I), "External functions name has already been defined")
                    FuncsAndSubs(fIndx).FncType = 1

                Case "}"
                    Return CreateError("", SyntaxError_BracingError, I, Lines(I), "Unknown closing brace")
                Case "break"
                    If Len(sp(1)) > 4 Then

                    Else
                        Dim breakLevel As Integer
                        breakLevel = Val(Replace(sp(1), "$", "&h0"))
                        myScope = GetScope(I, breakLevel)

                        If myScope < 0 Then Return CreateError("", -1, I, Lines(I), "Break (" + breakLevel.ToString + ") out of range. (" + MaxScope(I).ToString + ") max value")

                        LabeledToCodeArray(CodeOutput, MemAddr, "goto :" + BraceScopes(myScope).Leave + "   " + I.ToString, 8)
                    End If
                Case "for"
                    If Lines(I + 1) <> "{" Then Return CreateError("", SyntaxError_BracingError, I, Lines(I), "FOR loop declared without opening brace '{'")
                    'for (a0 = 0; a0 < 1; a0 += 1)
                    'for (init; condition; increment)

                    rt = 0
                    For i2 = I + 2 To Lines.Count - 1
                        sp2 = Split(LCase(Lines(i2)) + "      ", " ")
                        If sp2(0) = "{" Then rt += 1
                        If sp2(0) = "}" Then
                            If rt = 0 Then GoTo process_forloop_statement
                            rt -= 1
                        End If
                    Next
                    Return CreateError("", SyntaxError_BracingError, I, Lines(I), "FOR loop declared without closing brace '}'")
process_forloop_statement:
                    Lines(I + 1) = ""
                    myScope = AddScope(I + 1, i2, "")

                    Dim forBranchIndex As Integer, forCondReg1 As String, forCondReg2 As String, forCondVal As Integer, forCondType As String
                    Dim forBranchLeaveLabel As String, forLeaveFormat(10) As String

                    forBranchIndex = i2
                    forCondReg1 = sp(4)
                    forCondType = sp(5)
                    forCondReg2 = sp(6)
                    If GetEERegVal(sp(6)) < 0 Then
                        forCondVal = Val(Replace(sp(6), "$", "&h0"))
                        forCondReg2 = ""
                    End If

                    forBranchLeaveLabel = "%%%%for%loop%branch%addr%" + I.ToString + "%" + MemAddr.ToString + "%leave%" + forBranchIndex.ToString

                    forLeaveFormat(0) = "forloopbranch"
                    'Lines(forBranchIndex) = "forloopbranch %%%%for%loop%branch%addr%" + I.ToString + "%" + MemAddr.ToString + " " + forBranchLeaveLabel

                    If GetEERegVal(sp(1)) < 0 Then Return CreateError("", SyntaxError_ForLoopInit, I, Lines(I), "For loops require an EE register For initialize statement")
                    If GetEERegVal(sp(4)) < 0 Then Return CreateError("", SyntaxError_ForLoopCond, I, Lines(I), "For loops require an EE register For conditional statement")
                    If sp(7) <> "" Then
                        forLeaveFormat(3) = "1"
                        forLeaveFormat(4) = sp(7)
                        forLeaveFormat(5) = sp(8)
                        forLeaveFormat(6) = sp(9)
                        'Lines(forBranchIndex) += " 1 " + sp(7) + " " + sp(8) + " " + sp(9)
                        If GetEERegVal(sp(7)) < 0 Then Return CreateError("", SyntaxError_ForLoopIncr, I, Lines(I), "For loops require an EE register For increment statement")
                    Else
                        forLeaveFormat(3) = "0"
                        'Lines(forBranchIndex) += " 0"
                    End If

                    If sp(2) <> "=" Then Return CreateError("", SyntaxError_ForLoopInit, I, Lines(I), "Bad For Loop initialize statement")
                    '------------------------------------ Init statement
                    If GetEERegVal(sp(3)) < 0 Then
                        sp(3) = Strings.Right("00000000" + Hex(Val(Replace(sp(3), "$", "&h0"))), 8)

                        If Val("&H" + sp(3)) <= &H7FFF And Val("&H" + sp(3)) >= &HFFFF8000 Then
                            rt = mpAsm.AssembleInstruction("addiu " + sp(1) + ", zero, $" + Strings.Right(sp(3), 4), CodeRet)
                            If rt < 0 Then Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Unknown reason for being here")
                            FormatToCodeArray(CodeOutput, MemAddr, CodeRet)
                        ElseIf Val("&H" + sp(3)) > 0 And Val("&H" + sp(3)) < &H10000 Then
                            rt = mpAsm.AssembleInstruction("ori " + sp(1) + ", zero, $" + Strings.Right(sp(3), 4), CodeRet)
                            If rt < 0 Then Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Unknown reason for being here")
                            FormatToCodeArray(CodeOutput, MemAddr, CodeRet)
                        Else
                            rt = mpAsm.AssembleInstruction("lui " + sp(1) + ", $" + Strings.Left(sp(3), 4), CodeRet)
                            If rt < 0 Then Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Unknown reason for being here")
                            FormatToCodeArray(CodeOutput, MemAddr, CodeRet)

                            rt = mpAsm.AssembleInstruction("ori " + sp(1) + ", " + sp(1) + ", $" + Strings.Right(sp(3), 4), CodeRet)
                            If rt < 0 Then Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Unknown reason for being here")
                            FormatToCodeArray(CodeOutput, MemAddr, CodeRet)
                        End If
                    Else
                        rt = mpAsm.AssembleInstruction("daddu " + sp(1) + ", " + sp(3) + ", zero", CodeRet)
                        If rt < 0 Then Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Unknown reason for being here")
                        FormatToCodeArray(CodeOutput, MemAddr, CodeRet)
                    End If

                    rt = AddLabel("%%%%for%loop%branch%addr%" + I.ToString + "%" + MemAddr.ToString, MemAddr)
                    If rt < 0 Then Return CreateError("", SyntaxError_DuplicateLabel, I, Lines(I), "Unknown reason for being here")

                    forLeaveFormat(1) = "%%%%for%loop%branch%addr%" + I.ToString + "%" + MemAddr.ToString
                    forLeaveFormat(2) = forBranchLeaveLabel
                    Lines(forBranchIndex) = Join(forLeaveFormat, " ") + "      "
                    BraceScopes(myScope).Leave = forBranchLeaveLabel

                    If forCondReg2 = "" Then
                        tStr = Strings.Right("00000000" + Hex(forCondVal), 8)

                        If forCondVal <= &H7FFF And forCondVal >= &HFFFF8000 Then
                            rt = mpAsm.AssembleInstruction("addiu at, zero, " + forCondVal.ToString, CodeRet)
                            If rt < 0 Then Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Unknown reason for being here")
                            FormatToCodeArray(CodeOutput, MemAddr, CodeRet)
                        ElseIf forCondVal > 0 And forCondVal < &H10000 Then
                            rt = mpAsm.AssembleInstruction("ori at, zero, " + forCondVal.ToString, CodeRet)
                            If rt < 0 Then Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Unknown reason for being here")
                            FormatToCodeArray(CodeOutput, MemAddr, CodeRet)
                        Else
                            rt = mpAsm.AssembleInstruction("lui at, $" + Strings.Left(tStr, 4), CodeRet)
                            If rt < 0 Then Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Unknown reason for being here")
                            FormatToCodeArray(CodeOutput, MemAddr, CodeRet)

                            rt = mpAsm.AssembleInstruction("ori at, at, $" + Strings.Right(tStr, 4), CodeRet)
                            If rt < 0 Then Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Unknown reason for being here")
                            FormatToCodeArray(CodeOutput, MemAddr, CodeRet)
                        End If
                        forCondReg2 = "at"
                    End If

                    If forCondType = "==" Then
                        LabeledToCodeArray(CodeOutput, MemAddr, "bne " + forCondReg1 + ", " + forCondReg2 + ", :" + forBranchLeaveLabel + " " + I.ToString, 4)
                        FormatToCodeArray(CodeOutput, MemAddr, 0)
                    ElseIf forCondType = "<>" Then
                        LabeledToCodeArray(CodeOutput, MemAddr, "beq " + forCondReg1 + ", " + forCondReg2 + ", :" + forBranchLeaveLabel + " " + I.ToString, 4)
                        FormatToCodeArray(CodeOutput, MemAddr, 0)
                    ElseIf forCondType = "<" Then
                        rt = mpAsm.AssembleInstruction("subu at, " + forCondReg1 + ", " + forCondReg2, CodeRet)
                        If rt < 0 Then Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Unknown reason for being here")
                        FormatToCodeArray(CodeOutput, MemAddr, CodeRet)

                        LabeledToCodeArray(CodeOutput, MemAddr, "bgez at, :" + forBranchLeaveLabel + "  " + I.ToString, 4)
                        FormatToCodeArray(CodeOutput, MemAddr, 0)

                    ElseIf forCondType = "<=" Then
                        rt = mpAsm.AssembleInstruction("subu at, " + forCondReg1 + ", " + forCondReg2, CodeRet)
                        If rt < 0 Then Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Unknown reason for being here")
                        FormatToCodeArray(CodeOutput, MemAddr, CodeRet)

                        LabeledToCodeArray(CodeOutput, MemAddr, "bgtz at, :" + forBranchLeaveLabel + "  " + I.ToString, 4)
                        FormatToCodeArray(CodeOutput, MemAddr, 0)

                    ElseIf forCondType = ">" Then
                        rt = mpAsm.AssembleInstruction("subu at, " + forCondReg1 + ", " + forCondReg2, CodeRet)
                        If rt < 0 Then Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Unknown reason for being here")
                        FormatToCodeArray(CodeOutput, MemAddr, CodeRet)

                        LabeledToCodeArray(CodeOutput, MemAddr, "blez at, :" + forBranchLeaveLabel + "  " + I.ToString, 4)
                        FormatToCodeArray(CodeOutput, MemAddr, 0)

                    ElseIf forCondType = ">=" Then
                        rt = mpAsm.AssembleInstruction("subu at, " + forCondReg1 + ", " + forCondReg2, CodeRet)
                        If rt < 0 Then Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Unknown reason for being here")
                        FormatToCodeArray(CodeOutput, MemAddr, CodeRet)

                        LabeledToCodeArray(CodeOutput, MemAddr, "bltz at, :" + forBranchLeaveLabel + "  " + I.ToString, 4)
                        FormatToCodeArray(CodeOutput, MemAddr, 0)
                    End If

                Case "forloopbranch"
                    Dim forReturnLabel As String, forLeaveLabel As String
                    Dim forIncType As String, forIncReg1 As String, forIncReg2 As String, forIncVal As Integer
                    Dim forIncAfterBEQ As String

                    forReturnLabel = sp(1)
                    forLeaveLabel = sp(2)

                    If sp(3) = "0" Then
                        LabeledToCodeArray(CodeOutput, MemAddr, "beq zero, zero, :" + forReturnLabel + " " + I.ToString, 4)
                        FormatToCodeArray(CodeOutput, MemAddr, 0)
                    Else
                        forIncAfterBEQ = "nop"

                        forIncReg1 = sp(4)
                        forIncType = sp(5)
                        forIncReg2 = sp(6)
                        If GetEERegVal(forIncReg2) < 0 Then
                            forIncVal = Val(Replace(forIncReg2, "$", "&h0"))
                            forIncReg2 = ""
                        End If

                        If forIncType = "++" Then
                            forIncAfterBEQ = "addiu " + forIncReg1 + ", " + forIncReg1 + ", 1"
                        ElseIf forIncType = "--" Then
                            forIncAfterBEQ = "addiu " + forIncReg1 + ", " + forIncReg1 + ", -1"
                        ElseIf forIncType = "+=" Then
                            If forIncReg2 = "" Then
                                If forIncVal >= &HFFFF8000 And forIncVal <= &H7FFF Then
                                    forIncAfterBEQ = "addiu " + forIncReg1 + ", " + forIncReg1 + ", " + forIncVal.ToString
                                End If
                            Else
                                forIncAfterBEQ = "addu " + forIncReg1 + ", " + forIncReg1 + ", " + forIncReg2
                            End If
                        ElseIf forIncType = "-=" Then
                            If forIncReg2 = "" Then
                                If forIncVal >= &HFFFF8000 And forIncVal <= &H7FFF Then
                                    forIncAfterBEQ = "addiu " + forIncReg1 + ", " + forIncReg1 + ", -" + Math.Abs(forIncVal).ToString
                                End If
                            Else
                                forIncAfterBEQ = "subu " + forIncReg1 + ", " + forIncReg1 + ", " + forIncReg2
                            End If
                        End If

                        LabeledToCodeArray(CodeOutput, MemAddr, "beq zero, zero, :" + forReturnLabel + " " + I.ToString, 4)
                        rt = mpAsm.AssembleInstruction(forIncAfterBEQ, CodeRet)
                        If rt < 0 Then Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Unknown reason for being here")
                        FormatToCodeArray(CodeOutput, MemAddr, CodeRet)


                    End If

                    rt = AddLabel(sp(2), MemAddr)
                    If rt < 0 Then Return CreateError("", SyntaxError_BadLabelDefinition, I, Lines(I), "Unkown reason for being here")

                Case "while"
                    Dim wReg1 As String, wReg2 As String, wType As String, wVal As Integer
                    Dim wLeave As String
                    ifsCount += 1
                    If Lines(I + 1) <> "{" Then Return CreateError("", SyntaxError_BracingError, I, Lines(I), "WHILE declared without opening brace '{'")

                    Lines(I + 1) = "%%%%while%loop%" + I.ToString + "%%%" + MemAddr.ToString + "%" + ifsCount.ToString
                    wLeave = "%%%%while%loop%" + I.ToString + "%%%" + MemAddr.ToString + "%" + ifsCount.ToString + "%%wend"

                    For i2 = I + 2 To Lines.Count - 1
                        sp2 = Split(LCase(Lines(i2)) + "      ", " ")
                        If sp2(0) = "{" Then rt += 1
                        If sp2(0) = "}" Then
                            If rt = 0 Then GoTo process_While_Loop
                            rt -= 1
                        End If
                    Next
                    Return CreateError("", SyntaxError_BracingError, I, Lines(I), "WHILE declared without closing brace '}'")
process_While_Loop:
                    myScope = AddScope(I + 1, i2, wLeave)
                    'while (condition)

                    Lines(i2) = "endwhile " + Lines(I + 1) + " " + wLeave
                    rt = AddLabel(Lines(I + 1), MemAddr)
                    If rt < 0 Then Return CreateError("", SyntaxError_DuplicateLabel, I, Lines(I), "Unkown reason for being here")

                    Lines(I + 1) = ""

                    wReg1 = sp(1)
                    wType = sp(2)
                    wReg2 = sp(3)

                    If wReg1 = "1" And wType = "" And wReg2 = "" Then
                        'Infinite Loop
                    ElseIf GetEERegVal(wReg1) > -1 And wType = "" And wReg2 = "" Then
                        LabeledToCodeArray(CodeOutput, MemAddr, "beq " + wReg1 + ", zero, :" + wLeave + " " + I.ToString, 4)
                        FormatToCodeArray(CodeOutput, MemAddr, 0)
                    Else
                        If GetEERegVal(wReg1) < 0 And GetEERegVal(wReg2) < 0 Then Return CreateError("", SyntaxError_BadWhileCondition, I, Lines(I), "WHILE condition requires at least 1 register")
                        If GetEERegVal(wReg1) < 0 And GetEERegVal(wReg2) > -1 Then
                            wVal = Val(Replace(wReg1, "$", "&h0"))
                            wReg1 = wReg2
                            wReg2 = ""
                        ElseIf GetEERegVal(wReg1) > -1 And GetEERegVal(wReg2) < 0 Then
                            wVal = Val(Replace(wReg2, "$", "&h0"))
                            wReg2 = ""
                        End If

                        If wReg2 = "" Then
                            tStr = Strings.Right("00000000" + Hex(wVal), 8)
                            If wVal >= &HFFFF8000 And wVal <= &H7FFF Then
                                rt = mpAsm.AssembleInstruction("addiu at, zero, " + wVal.ToString, CodeRet)
                                If rt < 0 Then Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Unknown reason for being here")
                                FormatToCodeArray(CodeOutput, MemAddr, CodeRet)

                            ElseIf wVal > 0 And wVal < &H10000 Then
                                rt = mpAsm.AssembleInstruction("ori at, zero, " + wVal.ToString, CodeRet)
                                If rt < 0 Then Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Unknown reason for being here")
                                FormatToCodeArray(CodeOutput, MemAddr, CodeRet)

                            Else
                                rt = mpAsm.AssembleInstruction("lui at, $" + Strings.Left(tStr, 4), CodeRet)
                                If rt < 0 Then Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Unknown reason for being here")
                                FormatToCodeArray(CodeOutput, MemAddr, CodeRet)

                                rt = mpAsm.AssembleInstruction("ori at, at, $" + Strings.Right(tStr, 4), CodeRet)
                                If rt < 0 Then Return CreateError("", SyntaxError_BadArgumentType, I, Lines(I), "Unknown reason for being here")
                                FormatToCodeArray(CodeOutput, MemAddr, CodeRet)
                            End If

                            wReg2 = "at"
                        End If

                        If wType = "==" Then
                            LabeledToCodeArray(CodeOutput, MemAddr, "bne " + wReg1 + ", " + wReg2 + ", :" + wLeave + " " + I.ToString, 4)
                            FormatToCodeArray(CodeOutput, MemAddr, 0)
                        ElseIf wType = "<>" Then
                            LabeledToCodeArray(CodeOutput, MemAddr, "beq " + wReg1 + ", " + wReg2 + ", :" + wLeave + " " + I.ToString, 4)
                            FormatToCodeArray(CodeOutput, MemAddr, 0)
                        Else
                            rt = mpAsm.AssembleInstruction("subu at, " + wReg1 + ", " + wReg2, CodeRet)
                            If rt < 0 Then Return CreateError("", SyntaxError_BadIFStatement, I, Lines(I), "Bad value within IF statement")
                            FormatToCodeArray(CodeOutput, MemAddr, CodeRet)
                            If wType = ">" Then
                                LabeledToCodeArray(CodeOutput, MemAddr, "blez at, :" + wLeave + "  " + I.ToString, 4)
                                FormatToCodeArray(CodeOutput, MemAddr, 0)
                            ElseIf wType = ">=" Then
                                LabeledToCodeArray(CodeOutput, MemAddr, "bltz at, :" + wLeave + "  " + I.ToString, 4)
                                FormatToCodeArray(CodeOutput, MemAddr, 0)
                            ElseIf wType = "<" Then
                                LabeledToCodeArray(CodeOutput, MemAddr, "bgez at, :" + wLeave + "  " + I.ToString, 4)
                                FormatToCodeArray(CodeOutput, MemAddr, 0)
                            ElseIf wType = "<=" Then
                                LabeledToCodeArray(CodeOutput, MemAddr, "bgtz at, :" + wLeave + "  " + I.ToString, 4)
                                FormatToCodeArray(CodeOutput, MemAddr, 0)
                            End If
                        End If
                    End If
                Case "endwhile"
                    LabeledToCodeArray(CodeOutput, MemAddr, "goto :" + sp(1) + "   " + I.ToString, 8)
                    rt = AddLabel(sp(2), MemAddr)
                    If rt < 0 Then Return CreateError("", SyntaxError_DuplicateLabel, I, Lines(I), "Unkown reason for being here")

                Case "__ifelse%"
                    If Lines(I + 1) = "" Then Return CreateError("", SyntaxError_BadIFStatement, I + 1, Lines(I + 1), "ELSE statement declared without any operation")
                    If Strings.Left(LCase(Lines(I + 1)), 1) = "{" Then Return CreateError("", SyntaxError_BadIFStatement, I + 1, Lines(I + 1), "Single IF statements cannot be followed by an ELSE with a brace")
                    If Strings.Left(LCase(Lines(I + 1)), 2) = "if" Then Return CreateError("", SyntaxError_BadIFStatement, I + 1, Lines(I + 1), "Single ELSE statements cannot be followed by another IF")
                    If Strings.Left(LCase(Lines(I + 1)), 3) = "fnc" Then Return CreateError("", SyntaxError_BadIFStatement, I + 1, Lines(I + 1), "Single ELSE statements cannot be followed by FNC")
                    If Strings.Left(LCase(Lines(I + 1)), 3) = "for" Then Return CreateError("", SyntaxError_BadIFStatement, I + 1, Lines(I + 1), "Single ELSE statements cannot be followed by FOR")
                    If Strings.Left(LCase(Lines(I + 1)), 5) = "while" Then Return CreateError("", SyntaxError_BadIFStatement, I + 1, Lines(I + 1), "Single ELSE statements cannot be followed by WHILE")
                    If Strings.Left(LCase(Lines(I + 1)), 6) = "switch" Then Return CreateError("", SyntaxError_BadIFStatement, I + 1, Lines(I + 1), "Single ELSE statements cannot be followed by SWITCH")

                    LabeledToCodeArray(CodeOutput, MemAddr, "goto :" + sp(1) + "   " + I.ToString, 8)

                    rt = AddLabel(sp(2), MemAddr)
                    If rt < 0 Then Return CreateError("", SyntaxError_DuplicateLabel, I + 1, Lines(I + 1), "Unknown reason for being here")

                    rt = AddLabel(sp(1), MemAddr + 4)
                    If rt < 0 Then Return CreateError("", SyntaxError_DuplicateLabel, I + 1, Lines(I + 1), "Unknown reason for being here")


                Case "if"
                    Dim ifSingle As Boolean, ifReg1 As String, ifReg2 As String
                    Dim ifType As String

                    ifSingle = True
                    If Lines(I + 1) = "{" Then ifSingle = False

                    If ifSingle Then
                        If Lines(I + 1) = "" Then Return CreateError("", SyntaxError_BadIFStatement, I + 1, Lines(I + 1), "IF statement declared without any operation")
                        If Strings.Left(LCase(Lines(I + 1)), 2) = "if" Then Return CreateError("", SyntaxError_BadIFStatement, I + 1, Lines(I + 1), "Single IF statements cannot be followed by another IF")
                        If Strings.Left(LCase(Lines(I + 1)), 3) = "fnc" Then Return CreateError("", SyntaxError_BadIFStatement, I + 1, Lines(I + 1), "Single IF statements cannot be followed by FNC")
                        If Strings.Left(LCase(Lines(I + 1)), 3) = "for" Then Return CreateError("", SyntaxError_BadIFStatement, I + 1, Lines(I + 1), "Single IF statements cannot be followed by FOR")
                        If Strings.Left(LCase(Lines(I + 1)), 5) = "while" Then Return CreateError("", SyntaxError_BadIFStatement, I + 1, Lines(I + 1), "Single IF statements cannot be followed by WHILE")
                        If Strings.Left(LCase(Lines(I + 1)), 6) = "switch" Then Return CreateError("", SyntaxError_BadIFStatement, I + 1, Lines(I + 1), "Single IF statements cannot be followed by SWITCH")
                        If Strings.Left(LCase(Lines(I + 1)), 6) = "thread" Then Return CreateError("", SyntaxError_BadIFStatement, I + 1, Lines(I + 1), "Single IF statements cannot be followed by a thread modifier")

                        Dim hasElse As Integer, ifLeaveSequence As String, ifElseSequence As String
                        hasElse = -1
                        If LCase(Lines(I + 2)) = "else" Then hasElse = I + 2

                        ifReg1 = sp(1)
                        ifType = sp(2)
                        ifReg2 = sp(3)

                        ifLeaveSequence = "%%if%%single%leave%all%" + I.ToString + "%%%" + MemAddr.ToString
                        ifElseSequence = "%%if%%single%else%go%" + I.ToString + "%%%" + MemAddr.ToString

                        If hasElse = -1 Then
                            tStr = GenerateConditionalStatement(ifReg1, ifReg2, ifType, ifLeaveSequence, False)
                            If Strings.Left(tStr, 5) = "ERROR" Then
                                sp2 = Split(tStr, ":")
                                If sp2(1) = "No EE Register" Then Return CreateError("", SyntaxError_BadIFStatement, I, Lines(I), "IF Statements require at least 1 EE register")
                                If sp2(1) = "Invalid Operator" Then Return CreateError("", SyntaxError_BadIFStatement, I, Lines(I), "IF Statements only supper ==, <>, >, >=, <, <= conditions")
                                Return -99
                            End If
                            rt = AddConditionalToOutput(tStr, MemAddr, CodeOutput, I)
                            If rt < 0 Then Return CreateError("", SyntaxError_DuplicateLabel, I, Lines(I), "Unkown reason for being here")
                            FormatToCodeArray(CodeOutput, MemAddr, 0)

                            Dim instIFSize As Integer, instIFPeek() As String
                            instIFSize = 4
                            instIFPeek = Split(parseSyntax(LCase(Lines(I + 1))) + "      ", " ")

                            If instIFPeek(0) = "" Then Return CreateError("", SyntaxError_BadIFStatement, I, Lines(I), "IF Declared without any operation")
                            If instIFPeek(0) = "return" Then instIFSize = 8
                            If instIFPeek(0) = "break" Then instIFSize = 8
                            If instIFPeek(0) = "goto" Then instIFSize = 8
                            If instIFPeek(0) = "setreg" Then instIFSize = 8
                            If instIFPeek(0) = "setfpr" Then Return CreateError("", SyntaxError_BadIFStatement, I + 1, Lines(I + 1), "IF Declared with invalid operation")
                            If instIFPeek(0) = "setfloat" Then Return CreateError("", SyntaxError_BadIFStatement, I + 1, Lines(I + 1), "IF Declared with invalid operation")


                            rt = AddLabel(ifLeaveSequence, MemAddr + instIFSize)
                            If rt < 0 Then Return CreateError("", SyntaxError_DuplicateLabel, I, Lines(I), "Unknown reason for being here")

                        Else
                            tStr = GenerateConditionalStatement(ifReg1, ifReg2, ifType, ifElseSequence, False)
                            If Strings.Left(tStr, 5) = "ERROR" Then
                                sp2 = Split(tStr, ":")
                                If sp2(1) = "No EE Register" Then Return CreateError("", SyntaxError_BadIFStatement, I, Lines(I), "IF Statements require at least 1 EE register")
                                If sp2(1) = "Invalid Operator" Then Return CreateError("", SyntaxError_BadIFStatement, I, Lines(I), "IF Statements only supper ==, <>, >, >=, <, <= conditions")
                                Return -99
                            End If
                            rt = AddConditionalToOutput(tStr, MemAddr, CodeOutput, I)
                            If rt < 0 Then Return CreateError("", SyntaxError_DuplicateLabel, I, Lines(I), "Unkown reason for being here")
                            FormatToCodeArray(CodeOutput, MemAddr, 0)

                            Lines(I + 2) = "__ifelse% " + ifLeaveSequence + " " + ifElseSequence
                        End If
                    Else
                        Dim ifR1() As String, ifR2() As String, ifT() As String, ifElseLeave As Integer
                        Dim hasElse As Integer, ifRt As Integer, ifElseIf() As Integer, ifElseIfEnd() As Integer
                        Dim ifElseMainEndBrace As Integer


                        hasElse = -1
                        ifsCount += 1
                        ifRt = -1
                        ifElseMainEndBrace = -1
                        rt = 0
                        For i2 = I + 2 To Lines.Count - 1
                            sp2 = Split(LCase(Lines(i2)) + "      ", " ")
                            If sp2(0) = "{" Then rt += 1
                            If sp2(0) = "}" Then
                                If LCase(Strings.Left(Lines(i2 + 1), 7)) = "else if" And rt = 0 Then
                                    If Lines(i2 + 2) <> "{" Then Return CreateError("", SyntaxError_BadIFStatement, i2 + 1, Lines(i2 + 1), "ELSE IF declared without opening brace '{'")
                                    If ifElseMainEndBrace = -1 Then ifElseMainEndBrace = i2

                                    rt = 0
                                    For i3 = i2 + 3 To Lines.Count - 1
                                        sp3 = Split(LCase(Lines(i3)) + "      ", " ")
                                        If sp3(0) = "{" Then rt += 1
                                        If sp3(0) = "}" Then
                                            If rt = 0 Then GoTo processAddIfElseStatement
                                            rt -= 1
                                        End If
                                    Next
                                    Return CreateError("", SyntaxError_BadIFStatement, i2 + 1, Lines(i2 + 1), "ELSE IF declared without closing brace '}'")
processAddIfElseStatement:
                                    ifRt += 1
                                    ReDim Preserve ifElseIf(ifRt)
                                    ReDim Preserve ifElseIfEnd(ifRt)
                                    ifElseIf(ifRt) = i2 + 1
                                    ifElseIfEnd(ifRt) = i3
                                    If LCase(Strings.Left(Lines(i3 + 1), 4)) = "else" Then
                                        rt = 1
                                        i2 += 2
                                    Else
                                        rt = 0
                                    End If
                                    If rt = 0 Then GoTo processAllIfElseStatements
                                Else
                                    If LCase(Strings.Left(Lines(i2 + 1), 4)) = "else" And rt = 0 Then
                                        If Lines(i2 + 2) <> "{" Then Return CreateError("", SyntaxError_BadIFStatement, i2 + 1, Lines(i2 + 1), "ELSE declared without opening brace '{'")

                                        rt = 0
                                        For i3 = i2 + 3 To Lines.Count - 1
                                            sp3 = Split(LCase(Lines(i3)) + "      ", " ")
                                            If sp3(0) = "{" Then rt += 1
                                            If sp3(0) = "}" Then
                                                If rt = 0 Then GoTo processAddIfElseElseStatement
                                                rt -= 1
                                            End If
                                        Next
                                        Return CreateError("", SyntaxError_BadIFStatement, i2 + 1, Lines(i2 + 1), "ELSE declared without closing brace '}'")
processAddIfElseElseStatement:
                                        hasElse = i2 + 1
                                        ifElseLeave = i3
                                    End If
                                    If rt = 0 Then GoTo processAllIfElseStatements
                                End If
                                rt -= 1
                            End If
                        Next
                        Return CreateError("", SyntaxError_BadIFStatement, I, Lines(I), "IF declared without closing brace '}'")
processAllIfElseStatements:
                        Dim ifLeaveSequence As String, ifElseSequence As String, ifElseIfEnter() As String
                        Dim ifEnterSequence As String

                        ifEnterSequence = "%%%%if%cond%enter%" + I.ToString + "%" + MemAddr.ToString + "%"
                        ifLeaveSequence = "%%%%if%leave%all%" + I.ToString + "%" + MemAddr.ToString + "%"
                        ifElseSequence = "%%%%if%else%else%" + I.ToString + "%" + MemAddr.ToString + "%"

                        If ifRt < 0 And hasElse < 0 Then ' Single IF braced statement
                            ifReg1 = sp(1)
                            ifType = sp(2)
                            ifReg2 = sp(3)
                            tStr = GenerateConditionalStatement(ifReg1, ifReg2, ifType, ifLeaveSequence, False)
                            If Strings.Left(tStr, 5) = "ERROR" Then
                                sp2 = Split(tStr, ":")
                                If sp2(1) = "No EE Register" Then Return CreateError("", SyntaxError_BadIFStatement, I, Lines(I), "IF Statements require at least 1 EE register")
                                If sp2(1) = "Invalid Operator" Then Return CreateError("", SyntaxError_BadIFStatement, I, Lines(I), "IF Statements only supper ==, <>, >, >=, <, <= conditions")
                                Return -99
                            End If

                            rt = AddConditionalToOutput(tStr, MemAddr, CodeOutput, I)
                            If rt < 0 Then Return CreateError("", SyntaxError_DuplicateLabel, I, Lines(I), "Unkown reason for being here")
                            FormatToCodeArray(CodeOutput, MemAddr, 0)

                            Lines(I + 1) = ""
                            Lines(i2) = ifLeaveSequence + ":"

                        ElseIf ifRt < 0 And hasElse > 0 Then ' IF{}ELSE{}
                            ifReg1 = sp(1)
                            ifType = sp(2)
                            ifReg2 = sp(3)

                            tStr = GenerateConditionalStatement(ifReg1, ifReg2, ifType, ifElseSequence, False)
                            If Strings.Left(tStr, 5) = "ERROR" Then
                                sp2 = Split(tStr, ":")
                                If sp2(1) = "No EE Register" Then Return CreateError("", SyntaxError_BadIFStatement, I, Lines(I), "IF Statements require at least 1 EE register")
                                If sp2(1) = "Invalid Operator" Then Return CreateError("", SyntaxError_BadIFStatement, I, Lines(I), "IF Statements only supper ==, <>, >, >=, <, <= conditions")
                                Return -99
                            End If

                            rt = AddConditionalToOutput(tStr, MemAddr, CodeOutput, I)
                            If rt < 0 Then Return CreateError("", SyntaxError_DuplicateLabel, I, Lines(I), "Unkown reason for being here")
                            FormatToCodeArray(CodeOutput, MemAddr, 0)

                            Lines(I + 1) = ""
                            Lines(i2) = ""
                            Lines(hasElse) = "goto :" + ifLeaveSequence
                            Lines(hasElse + 1) = ifElseSequence + ":"
                            Lines(ifElseLeave) = ifLeaveSequence + ":"

                        Else 'If ifRt > -1 And hasElse < 0 Then 'If{} else if{} else if {} ...
                            Dim ifElseLastBrace As Integer, ifElseCondFirst As String
                            Dim ifElseConds As String

                            ifElseLastBrace = i2

                            ifReg1 = sp(1)
                            ifType = sp(2)
                            ifReg2 = sp(3)

                            ifElseCondFirst = "%%%%if%cond%enter%" + I.ToString + "%" + MemAddr.ToString + "%first%"

                            tStr = GenerateConditionalStatement(ifReg1, ifReg2, ifType, ifElseCondFirst, True)
                            If Strings.Left(tStr, 5) = "ERROR" Then
                                sp2 = Split(tStr, ":")
                                If sp2(1) = "No EE Register" Then Return CreateError("", SyntaxError_BadIFStatement, I, Lines(I), "IF Statements require at least 1 EE register")
                                If sp2(1) = "Invalid Operator" Then Return CreateError("", SyntaxError_BadIFStatement, I, Lines(I), "IF Statements only supper ==, <>, >, >=, <, <= conditions")
                                Return -99
                            End If
                            rt = AddConditionalToOutput(tStr, MemAddr, CodeOutput, I)
                            If rt < 0 Then Return CreateError("", SyntaxError_DuplicateLabel, I, Lines(I), "Unkown reason for being here")

                            'ifEnterSequence
                            For i2 = 0 To ifRt
                                sp2 = Split(parseSyntax(Lines(ifElseIf(i2))) + "      ", " ")
                                ifReg1 = sp2(2)
                                ifType = sp2(3)
                                ifReg2 = sp2(4)

                                ifElseConds = ifEnterSequence + i2.ToString + "%"

                                tStr = GenerateConditionalStatement(ifReg1, ifReg2, ifType, ifElseConds, True)
                                If Strings.Left(tStr, 5) = "ERROR" Then
                                    sp2 = Split(tStr, ":")
                                    If sp2(1) = "No EE Register" Then Return CreateError("", SyntaxError_BadIFStatement, I, Lines(I), "IF Statements require at least 1 EE register")
                                    If sp2(1) = "Invalid Operator" Then Return CreateError("", SyntaxError_BadIFStatement, I, Lines(I), "IF Statements only supper ==, <>, >, >=, <, <= conditions")
                                    Return -99
                                End If
                                rt = AddConditionalToOutput(tStr, MemAddr, CodeOutput, I)
                                If rt < 0 Then Return CreateError("", SyntaxError_DuplicateLabel, I, Lines(I), "Unkown reason for being here")

                                Lines(ifElseIf(i2)) = "goto :" + ifLeaveSequence
                                Lines(ifElseIf(i2) + 1) = ifElseConds + ":"

                                Lines(ifElseIfEnd(i2)) = ""
                                If i2 = ifRt And hasElse < 0 Then
                                    Lines(ifElseIfEnd(i2)) = ifLeaveSequence + ":"
                                End If
                            Next
                            FormatToCodeArray(CodeOutput, MemAddr, 0)

                            rt = AddLabel(ifElseCondFirst, MemAddr + 8)
                            If rt < 0 Then Return CreateError("", SyntaxError_DuplicateLabel, I, Lines(I), "Unkown reason for being here")

                            If hasElse < 0 Then
                                Lines(I + 1) = "goto :" + ifLeaveSequence
                            Else
                                Lines(I + 1) = "goto :" + ifElseSequence
                                Lines(hasElse) = "goto :" + ifLeaveSequence
                                Lines(hasElse + 1) = ifElseSequence + ":"
                                Lines(ifElseLeave) = ifLeaveSequence + ":"
                            End If
                            Lines(ifElseMainEndBrace) = ""
                            'Else '------------------------------- If{} else if{} else if{} else{}

                        End If
                    End If
                Case "switch"
                    switchesCount += 1
                    If GetEERegVal(sp(1)) < 0 Then Return CreateError("", SyntaxError_BadSwitchDeclaration, I, Lines(I), "SWITCH statements require a single EE register")
                    If Lines(I + 1) <> "{" Then Return CreateError("", SyntaxError_BracingError, I, Lines(I), "SWITCH statements must be followed by opening brace '{'")
                    Lines(I + 1) = ""
                    tStr = "%%%switch%%" + switchesCount.ToString + "%%leave%%%" + MemAddr.ToString

                    Dim switchCases() As String, switchCaseStarts() As String, switchCaseLeaves() As Integer
                    Dim switchDefaultStart As String, switchDefaultLeave As Integer, switchCaseCount As Integer, switchLeave As Integer
                    ReDim switchCases(0)
                    ReDim switchCaseStarts(0)
                    ReDim switchCaseLeaves(0)

                    switchCaseCount = 0
                    switchDefaultStart = ""
                    rt = 0
                    For i2 = I + 2 To Lines.Count - 1
                        sp2 = Split(LCase(Lines(i2)) + "      ", " ")

                        If sp2(0) = "case" And rt = 0 Then
                            If sp2(1) = "" Then Return CreateError("", SyntaxError_SwitchEmptyCase, i2, Lines(i2), "Cases need a value to execute")
                            ReDim Preserve switchCases(switchCaseCount)
                            ReDim Preserve switchCaseStarts(switchCaseCount)
                            ReDim Preserve switchCaseLeaves(switchCaseCount)
                            switchCases(switchCaseCount) = sp2(1)

                            If Lines(i2 + 1) <> "{" Then Return CreateError("", SyntaxError_BracingError, i2, Lines(i2), "All case declarations must be followed by opening brace '{'")
                            switchCaseStarts(switchCaseCount) = "%%%%switch%" + switchesCount.ToString + "%case%%" + (i2 + 1).ToString + "%" + MemAddr.ToString + "%%"
                            Lines(i2 + 1) = switchCaseStarts(switchCaseCount) + ":"

                            rt2 = 0
                            For i3 = i2 + 2 To Lines.Count - 1
                                sp3 = Split(LCase(Lines(i3)) + "      ", " ")
                                If sp3(0) = "{" Then rt2 += 1
                                If sp3(0) = "}" Then
                                    If rt2 = 0 Then
                                        switchCaseLeaves(switchCaseCount) = i3
                                        Lines(i3) = "goto :" + tStr
                                        GoTo switchResumeCasesScan1
                                    Else
                                        rt2 -= 1
                                    End If
                                End If
                            Next
                            Return CreateError("", SyntaxError_BracingError, i2, Lines(i2), "Case declared without closing brace '}'")
switchResumeCasesScan1:
                            Lines(i2) = ""
                            myScope = AddScope(I + 1, i2, tStr)
                            switchCaseCount += 1
                        End If


                        If sp2(0) = "default" And rt = 0 Then
                            If Lines(i2 + 1) <> "{" Then Return CreateError("", SyntaxError_BracingError, i2, Lines(i2), "All case declarations must be followed by opening brace '{'")
                            switchDefaultStart = "%%%%switch%" + switchesCount.ToString + "%def%%" + (i2 + 1).ToString + "%" + MemAddr.ToString + "%%"
                            Lines(i2 + 1) = switchDefaultStart + ":"

                            rt2 = 0
                            For i3 = i2 + 2 To Lines.Count - 1
                                sp3 = Split(LCase(Lines(i3)) + "      ", " ")
                                If sp3(0) = "{" Then rt2 += 1
                                If sp3(0) = "}" Then
                                    If rt2 = 0 Then
                                        switchDefaultLeave = i3
                                        Lines(i3) = "goto :" + tStr
                                        GoTo switchResumeCasesScan2
                                    Else
                                        rt2 -= 1
                                    End If
                                End If
                            Next
                            Return CreateError("", SyntaxError_BracingError, i2, Lines(i2), "Case declared without closing brace '}'")
switchResumeCasesScan2:
                            Lines(i2) = ""
                        End If

                        If sp2(0) = "{" Then rt += 1
                        If sp2(0) = "}" Then
                            If rt = 0 Then GoTo processSwitchStatement
                            rt -= 1
                        End If
                    Next
                    Return CreateError("", SyntaxError_BracingError, I, Lines(I), "SWITCH declared without closing brace '}'")
processSwitchStatement:
                    switchLeave = i2

                    Lines(i2) = tStr + ":"

                    Dim switchVal As Integer

                    For i2 = 0 To switchCaseCount - 1
                        If Strings.Left(switchCases(i2), 1) = ":" Then
                            LabeledToCodeArray(CodeOutput, MemAddr, "setreg at, " + switchCases(i2) + "  " + I.ToString, 8)
                            LabeledToCodeArray(CodeOutput, MemAddr, "beq " + sp(1) + ", at, :" + switchCaseStarts(i2) + " " + I.ToString, 4)
                        Else
                            If GetEERegVal(switchCases(i2)) < 0 Then
                                switchVal = Val(Replace(switchCases(i2), "$", "&H0"))
                                If switchVal > 65535 Or switchVal < &HFFFF8000 Then
                                    LabeledToCodeArray(CodeOutput, MemAddr, "setreg at " + switchVal.ToString + "  " + I.ToString, 8)
                                ElseIf switchVal > -1 And switchVal < 65535 Then
                                    rt = mpAsm.AssembleInstruction("ori at, zero, " + switchVal.ToString, CodeRet)
                                    If rt < 0 Then Return CreateError("", SyntaxError_BadSwitchDeclaration, I, Lines(I), "Unknown reason to be here")
                                    FormatToCodeArray(CodeOutput, MemAddr, CodeRet)
                                Else
                                    rt = mpAsm.AssembleInstruction("addiu at, zero, " + switchVal.ToString, CodeRet)
                                    If rt < 0 Then Return CreateError("", SyntaxError_BadSwitchDeclaration, I, Lines(I), "Unknown reason to be here")
                                    FormatToCodeArray(CodeOutput, MemAddr, CodeRet)
                                End If
                                LabeledToCodeArray(CodeOutput, MemAddr, "beq " + sp(1) + ", at, :" + switchCaseStarts(i2) + " " + I.ToString, 4)
                            Else
                                LabeledToCodeArray(CodeOutput, MemAddr, "beq " + sp(1) + ", " + switchCases(i2) + ", :" + switchCaseStarts(i2) + " " + I.ToString, 4)
                            End If
                        End If
                    Next
                    FormatToCodeArray(CodeOutput, MemAddr, 0)

                    If switchDefaultStart = "" Then
                        LabeledToCodeArray(CodeOutput, MemAddr, "goto :" + tStr + "  " + I.ToString, 8)
                    Else
                        LabeledToCodeArray(CodeOutput, MemAddr, "goto :" + switchDefaultStart + "  " + I.ToString, 8)
                    End If


                Case Else
                    '------------------------------------------------ Label Definition
                    If Strings.Right(sp(0), 1) = ":" Then
                        sp(0) = Left(sp(0), Len(sp(0)) - 1)
                        If Lines(I) <> sp(0) + ":" Or Lines(I) = ":" Then Return CreateError("", SyntaxError_BadLabelDefinition, I, Lines(I), "")

                        rt = AddLabel(sp(0), MemAddr)
                        If rt < 0 Then Return CreateError("", SyntaxError_DuplicateLabel, I, Lines(I), "")

                        GoTo skipBlank
                    End If

                    '------------------------------------------------ Label Usage
                    If Strings.Left(sp(1), 1) = ":" Then
                        LabeledToCodeArray(CodeOutput, MemAddr, sp(0) + " " + sp(1) + " " + sp(2) + " " + sp(3) + " " + I.ToString, 4)
                        GoTo skipBlank
                    End If
                    If Strings.Left(sp(2), 1) = ":" Then
                        LabeledToCodeArray(CodeOutput, MemAddr, sp(0) + " " + sp(1) + " " + sp(2) + " " + sp(3) + " " + I.ToString, 4)
                        GoTo skipBlank
                    End If
                    If Strings.Left(sp(3), 1) = ":" Then
                        LabeledToCodeArray(CodeOutput, MemAddr, sp(0) + " " + sp(1) + " " + sp(2) + " " + sp(3) + " " + I.ToString, 4)
                        GoTo skipBlank
                    End If

                    '------------------------------------------------ Shorthand Math
                    If GetEERegVal(sp(0)) > -1 Then
                        If sp(1) = "++" Then
                            Lines(I) = "addiu " + sp(0) + ", " + sp(0) + ", 1"
                        ElseIf sp(1) = "+=" And sp(2) <> "" Then
                            If GetEERegVal(sp(2)) > -1 Then
                                Lines(I) = "addu " + sp(0) + ", " + sp(0) + ", " + sp(2)
                            Else
                                i2 = Val(Replace(sp(2), "$", "&h0"))
                                If i2 >= &HFFFF8000 And i2 <= &H7FFF Then Lines(I) = "addiu " + sp(0) + ", " + sp(0) + ", " + i2.ToString
                            End If
                        ElseIf sp(1) = "--" Then
                            Lines(I) = "addiu " + sp(0) + ", " + sp(0) + ", -1"
                        ElseIf sp(1) = "-=" And sp(2) <> "" Then
                            If GetEERegVal(sp(2)) > -1 Then
                                Lines(I) = "subu " + sp(0) + ", " + sp(0) + ", " + sp(2)
                            Else
                                i2 = Val(Replace(sp(2), "$", "&h0"))
                                If i2 >= &HFFFF8000 And i2 <= &H7FFF Then Lines(I) = "addiu " + sp(0) + ", " + sp(0) + ", " + (0 - i2).ToString
                            End If
                        ElseIf sp(1) = "=" And sp(2) <> "" Then
                            If GetEERegVal(sp(2)) > -1 Then
                                Lines(I) = "daddu " + sp(0) + ", " + sp(2) + ", zero"
                            Else
                                i2 = Val(Replace(sp(2), "$", "&h0"))
                                If i2 >= &HFFFF8000 And i2 <= &H7FFF Then Lines(I) = "addiu " + sp(0) + ", zero, " + i2.ToString
                            End If
                        ElseIf sp(1) = ">>" And sp(2) <> "" Then
                            If GetEERegVal(sp(2)) < 0 Or sp(2) = "0" Then
                                i2 = Val(Replace(sp(2), "$", "&h0"))
                                If i2 >= 0 And i2 <= 31 Then Lines(I) = "srl " + sp(0) + ", " + sp(0) + ", " + i2.ToString
                            End If
                        ElseIf sp(1) = "<<" And sp(2) <> "" Then
                            If GetEERegVal(sp(2)) < 0 Or sp(2) = "0" Then
                                i2 = Val(Replace(sp(2), "$", "&h0"))
                                If i2 >= 0 And i2 <= 31 Then Lines(I) = "sll " + sp(0) + ", " + sp(0) + ", " + i2.ToString
                            End If
                        ElseIf sp(1) = ">>>" And sp(2) <> "" Then
                            If GetEERegVal(sp(2)) < 0 Or sp(2) = "0" Then
                                i2 = Val(Replace(sp(2), "$", "&h0"))
                                If i2 >= 0 And i2 <= 31 Then Lines(I) = "dsrl " + sp(0) + ", " + sp(0) + ", " + i2.ToString
                            End If
                        ElseIf sp(1) = "<<<" And sp(2) <> "" Then
                            If GetEERegVal(sp(2)) < 0 Or sp(2) = "0" Then
                                i2 = Val(Replace(sp(2), "$", "&h0"))
                                If i2 >= 0 And i2 <= 31 Then Lines(I) = "dsll " + sp(0) + ", " + sp(0) + ", " + i2.ToString
                            End If
                        ElseIf sp(1) = ">>32" And sp(2) <> "" Then
                            If GetEERegVal(sp(2)) < 0 Or sp(2) = "0" Then
                                i2 = Val(Replace(sp(2), "$", "&h0"))
                                If i2 >= 0 And i2 <= 31 Then Lines(I) = "dsrl32 " + sp(0) + ", " + sp(0) + ", " + i2.ToString
                            End If
                        ElseIf sp(1) = "<<32" And sp(2) <> "" Then
                            If GetEERegVal(sp(2)) < 0 Or sp(2) = "0" Then
                                i2 = Val(Replace(sp(2), "$", "&h0"))
                                If i2 >= 0 And i2 <= 31 Then Lines(I) = "dsll32 " + sp(0) + ", " + sp(0) + ", " + i2.ToString
                            End If
                        End If
                    End If


                    If Len(sp(0)) = 8 And Len(sp(1)) = 8 And sp(2) = "" And sp(3) = "" Then
                        tStr = LCase(Strings.Right("00000000" + Hex(Val("&H" + sp(0))), 8))
                        tStr2 = LCase(Strings.Right("00000000" + Hex(Val("&H" + sp(1))), 8))

                        If tStr = LCase(sp(0)) And tStr2 = LCase(sp(1)) Then
                            AppendToCodeArray(CodeOutput, sp(0), sp(1))
                            GoTo skipBlank
                        End If
                    End If

                    '------------------------------------------------ Instruction
                    rt = mpAsm.AssembleInstruction(Lines(I), CodeRet)
                    If rt < 0 Then
AssemblerErrorHandle:
                        ErrDetail.LineData = Lines(I)
                        ErrDetail.LineNumber = I
                        ErrDetail.ExtraDetails = ""
                        Select Case rt
                            Case -1
                                ErrDetail.ErrorNumber = SyntaxError_EmptyString
                            Case -2
                                ErrDetail.ErrorNumber = SyntaxError_NoSignifcantData
                            Case -3
                                ErrDetail.ErrorNumber = SyntaxError_UnknownInstruction
                                ErrDetail.ExtraDetails = "Bad Instruction: " + sp(0)
                            Case -4
                                ErrDetail.ErrorNumber = SyntaxError_DataTypeUnknown
                                ErrDetail.ExtraDetails = "Bad Argument: " + sp(CodeRet + 1)
                            Case -5
                                ErrDetail.ErrorNumber = SyntaxError_BadArgumentCount
                                ErrDetail.ExtraDetails = "Provied: " + ((CodeRet >> 8) And &HFF).ToString +
                                                         vbCrLf +
                                                         "Required: " + (CodeRet And &HFF).ToString
                            Case -6
                                ErrDetail.ErrorNumber = SyntaxError_BadArgumentType
                                ErrDetail.ExtraDetails = "Bad Argument: " + sp(CodeRet + 1)
                        End Select
                        Return ErrDetail.ErrorNumber
                    End If
                    FormatToCodeArray(CodeOutput, MemAddr, CodeRet)
            End Select


skipBlank:
            If Strings.Left(Lines(I), 2) = "*/" Then isInComments = False
        Next

        If fncScan Then
            If projFncScan Then Return 0
            fncScan = False
            GoTo fncScanCompleteRestart
        End If

        'Patch Label Usage
        If isProj = False Then
            tStr = GatherFooterData()
            If tStr <> "" Then
                ClearFooter()
                Lines = Split(tStr + vbCrLf, vbCrLf)
                GoTo compileFooter
            End If

            rt = PatchLabels(CodeOutput)
            If rt < 0 Then Return rt
            cOut = Join(CodeOutput, vbCrLf)
        Else
            projAddr = MemAddr
            cOut = Join(CodeOutput, "})|%%{[NEW]%[LINE]%}%%|({")
        End If


        Return 0
    End Function
    Private Sub LabeledToCodeArray(ByRef CodeArr() As String, ByRef MemAddr As Int32, CodeString As String, Incr As Integer)
        CodeArr(CodeArr.Count - 1) = "*" + Strings.Right("00000000" + Hex(MemAddr), 8) +
                                     " " +
                                     CodeString
        ReDim Preserve CodeArr(CodeArr.Count)
        MemAddr += Incr
    End Sub
    Private Sub FormatToCodeArray(ByRef CodeArr() As String, ByRef MemAddr As Int32, ByVal CodeVal As UInt32)
        CodeArr(CodeArr.Count - 1) = FormatCode(MemAddr, CodeVal)
        ReDim Preserve CodeArr(CodeArr.Count)
        MemAddr += 4
    End Sub
    Private Sub AppendToCodeArray(ByRef CodeArr() As String, CodeAddr As String, CodeData As String)
        CodeArr(CodeArr.Count - 1) = CodeAddr + " " + CodeData
        ReDim Preserve CodeArr(CodeArr.Count)
    End Sub
    Private Function FormatCode(MemAddr As Int32, CodeVal As UInt32) As String
        Return CodeFormat +
               Strings.Right("00000000" + Hex(MemAddr), 7) +
               " " +
               Strings.Right("00000000" + Hex(CodeVal), 8)
    End Function

    Private Function AddConditionalToOutput(Str As String, ByRef MemAddr As Int32, ByRef CodeArray() As String, LineNum As Integer) As Integer
        Dim I As Integer, lines() As String, sp() As String, rt As Integer
        Dim CodeRet As UInt32

        lines = Split(Str, vbCrLf)
        For I = 0 To lines.Count - 1
            sp = Split(lines(I) + ":", ":")
            If sp.Count > 1 Then
                sp = Split(parseSyntax(lines(I)) + "        ", " ")
                LabeledToCodeArray(CodeArray, MemAddr, sp(0) + " " + sp(1) + " " + sp(2) + " " + sp(3) + " " + LineNum.ToString, 4)
            Else
                rt = mpAsm.AssembleInstruction(lines(I), CodeRet)
                If rt < 0 Then Return rt
                FormatToCodeArray(CodeArray, MemAddr, CodeRet)
            End If
        Next

        Return 0
    End Function
    Private Function GenerateConditionalStatement(r1 As String, r2 As String, rType As String, brLabel As String, onTrue As Boolean) As String
        Dim rVal As Integer, reg1 As String, reg2 As String, asmLines() As String
        Dim i As Integer

        If GetEERegVal(r1) < 0 And GetEERegVal(r2) < 0 Then Return "ERROR:No EE Register"

        ReDim asmLines(0)
        i = 0

        reg1 = r1
        reg2 = r2
        If GetEERegVal(r1) < 0 Then
            rVal = Val(Replace(r1, "$", "&h0"))
            reg1 = reg2
            reg2 = "at"
            asmLines = Split(GenerateSetATCode(rVal), vbCrLf)
            If asmLines(1) = "" Then ReDim Preserve asmLines(0)
            i = asmLines.Count
        End If
        If GetEERegVal(r2) < 0 Then
            rVal = Val(Replace(r2, "$", "&h0"))
            reg2 = "at"
            asmLines = Split(GenerateSetATCode(rVal), vbCrLf)
            If asmLines(1) = "" Then ReDim Preserve asmLines(0)
            i = asmLines.Count
        End If

        Select Case rType
            Case "=="
                ReDim Preserve asmLines(i)
                asmLines(i) = "bne " + reg1 + ", " + reg2 + ", :" + brLabel
                If onTrue Then asmLines(i) = "beq " + reg1 + ", " + reg2 + ", :" + brLabel
            Case "<>"
                ReDim Preserve asmLines(i)
                asmLines(i) = "beq " + reg1 + ", " + reg2 + ", :" + brLabel
                If onTrue Then asmLines(i) = "bne " + reg1 + ", " + reg2 + ", :" + brLabel
            Case ">"
                ReDim Preserve asmLines(i)
                asmLines(i) = "subu at, " + reg1 + ", " + reg2
                i += 1
                ReDim Preserve asmLines(i)
                asmLines(i) = "blez at, :" + brLabel
                If onTrue Then asmLines(i) = "bgtz at, :" + brLabel
            Case ">="
                ReDim Preserve asmLines(i)
                asmLines(i) = "subu at, " + reg1 + ", " + reg2
                i += 1
                ReDim Preserve asmLines(i)
                asmLines(i) = "bltz at, :" + brLabel
                If onTrue Then asmLines(i) = "bgez at, :" + brLabel
            Case "<"
                ReDim Preserve asmLines(i)
                asmLines(i) = "subu at, " + reg1 + ", " + reg2
                i += 1
                ReDim Preserve asmLines(i)
                asmLines(i) = "bgez at, :" + brLabel
                If onTrue Then asmLines(i) = "bltz at, :" + brLabel
            Case "<="
                ReDim Preserve asmLines(i)
                asmLines(i) = "subu at, " + reg1 + ", " + reg2
                i += 1
                ReDim Preserve asmLines(i)
                asmLines(i) = "bgtz at, :" + brLabel
                If onTrue Then asmLines(i) = "blez at, :" + brLabel
            Case Else
                Return "ERROR:Invalid Operator"
        End Select

        Return Join(asmLines, vbCrLf)
    End Function
    Private Function GenerateSetATCode(rVal As Integer) As String
        If rVal >= &HFFFF8000 And rVal <= &H7FFF Then Return "addiu at, zero, " + rVal.ToString + vbCrLf
        If rVal > 0 And rVal < &H10000 Then Return "ori at, zero, " + rVal.ToString + vbCrLf

        Dim tstr As String
        tstr = Strings.Right("00000000" + Hex(rVal), 8)

        Return "lui at, $" + Strings.Left(tstr, 4) + vbCrLf +
               "ori at, at, $" + Strings.Right(tstr, 4)

    End Function
    Private Function GenerateSetRegCode(rVal As Integer, Reg As String) As String
        If rVal = 0 Then Return "daddu " + Reg + ", zero, zero"
        If rVal >= &HFFFF8000 And rVal <= &H7FFF Then Return "addiu " + Reg + ", zero, " + rVal.ToString
        If rVal > 0 And rVal < &H10000 Then Return "ori " + Reg + ", zero, " + rVal.ToString

        Dim tstr As String
        tstr = Strings.Right("00000000" + Hex(rVal), 8)

        Return "lui " + Reg + ", $" + Strings.Left(tstr, 4) + vbCrLf +
               "ori " + Reg + ", " + Reg + ", $" + Strings.Right(tstr, 4)

    End Function
    Private Function AddGeneratedToOutput(Str As String, ByRef MemAddr As Int32, ByRef CodeArray() As String, LineNum As Integer) As Integer
        Dim I As Integer, lines() As String, sp() As String, rt As Integer
        Dim CodeRet As UInt32

        lines = Split(Str + vbCrLf, vbCrLf)
        For I = 0 To lines.Count - 1
            If lines(I) <> "" Then
                sp = Split(lines(I) + ":", ":")
                If sp.Count > 1 Then
                    sp = Split(parseSyntax(lines(I)) + "        ", " ")
                    LabeledToCodeArray(CodeArray, MemAddr, sp(0) + " " + sp(1) + " " + sp(2) + " " + sp(3) + " " + LineNum.ToString, 4)
                Else
                    rt = mpAsm.AssembleInstruction(lines(I), CodeRet)
                    If rt < 0 Then Return rt
                    FormatToCodeArray(CodeArray, MemAddr, CodeRet)
                End If
            End If
        Next

        Return 0
    End Function

    Private Function PatchLabels(ByRef CodeOutput() As String) As Integer
        Dim I As Int64, i2 As Int32, sp() As String, MemAddr As Int32, rt As Integer
        Dim CodeRet As UInt32, LabelType As Integer, tStr As String, tStr2 As String

        For I = 0 To CodeOutput.Count - 1
            If Strings.Left(CodeOutput(I), 1) = "*" Then
                sp = Split(Strings.Right(CodeOutput(I), Len(CodeOutput(I)) - 10), " ")
                CodeOutput(I) = Strings.Mid(CodeOutput(I), 2, 9)

                MemAddr = Val("&H" + Strings.Left(CodeOutput(I), 8))

                Select Case sp(0)
                    Case "elf.entry"
                        If Strings.Left(sp(1), 1) = ":" Then sp(1) = Strings.Right(sp(1), Len(sp(1)) - 1)
                        rt = GetLabel(sp(1), i2)
                        If rt < 0 Then GoTo LabelNotFound
                        ElfCfg.Entry = i2
                        CodeOutput(I) = ""
                    Case "hook"
                        'hook %me $00100000 -j
                        'hook labelname $00100000 -jal
                        'hook labelname $00100000 -pointer

                        If Strings.Left(sp(1), 1) = ":" Then sp(1) = Strings.Right(sp(1), Len(sp(1)) - 1)
                        rt = GetLabel(sp(1), i2)
                        If rt < 0 Then GoTo LabelNotFound
                        sp(1) = Strings.Right("00000000" + Hex(i2), 8)

                        sp(2) = Strings.Right("00000000" + Hex(Val(Replace(sp(2), "$", "&h0"))), 8)

                        tStr = CodeFormat + Strings.Right(sp(2), 7)
                        If LCase(sp(3)) = "-j" Then
                            rt = mpAsm.AssembleInstruction("j $" + sp(1), CodeRet)
                            If rt < 0 Then GoTo LabelAsmError
                            tStr2 = Strings.Right("00000000" + Hex(CodeRet), 8)
                        ElseIf LCase(sp(3)) = "-jal" Then
                            rt = mpAsm.AssembleInstruction("jal $" + sp(1), CodeRet)
                            If rt < 0 Then GoTo LabelAsmError
                            tStr2 = Strings.Right("00000000" + Hex(CodeRet), 8)
                        ElseIf LCase(sp(3)) = "-pointer" Then
                            tStr2 = sp(1)
                        Else
                            rt = mpAsm.AssembleInstruction("j $" + sp(1), CodeRet)
                            If rt < 0 Then GoTo LabelAsmError
                            tStr2 = Strings.Right("00000000" + Hex(CodeRet), 8)
                        End If

                        CodeOutput(I) = tStr + " " + tStr2

                    Case "hexcode"
                        If Strings.Left(sp(1), 1) = ":" Then sp(1) = Strings.Right(sp(1), Len(sp(1)) - 1)
                        rt = GetLabel(sp(1), i2)
                        If rt < 0 Then GoTo LabelNotFound

                        CodeOutput(I) = CodeFormat + Strings.Right(CodeOutput(I), 8)
                        CodeOutput(I) += Strings.Right("00000000" + Hex(i2), 8)

                    Case "setreg"
                        If Strings.Left(sp(2), 1) = ":" Then sp(2) = Strings.Right(sp(2), Len(sp(2)) - 1)
                        rt = GetLabel(sp(2), i2)
                        If rt < 0 Then GoTo LabelNotFound

                        sp(3) = Strings.Right("00000000" + Hex(i2), 8)

                        rt = mpAsm.AssembleInstruction("lui " + sp(1) + ", $" + Strings.Left(sp(3), 4), CodeRet)
                        If rt < 0 Then GoTo LabelAsmError
                        CodeOutput(I) = CodeFormat + Strings.Right("00000000" + Hex(MemAddr), 7) + " "
                        CodeOutput(I) += Strings.Right("00000000" + Hex(CodeRet), 8)
                        MemAddr += 4

                        rt = mpAsm.AssembleInstruction("ori " + sp(1) + ", " + sp(1) + ", $" + Strings.Right(sp(3), 4), CodeRet)
                        If rt < 0 Then GoTo LabelAsmError
                        CodeOutput(I) += vbCrLf + CodeFormat + Strings.Right("00000000" + Hex(MemAddr), 7) + " "
                        CodeOutput(I) += Strings.Right("00000000" + Hex(CodeRet), 8)
                    Case "goto"
                        If Strings.Left(sp(1), 1) = ":" Then
                            sp(1) = Right(sp(1), Len(sp(1)) - 1)
                            rt = GetLabel(sp(1), i2)
                            If rt < 0 Then GoTo LabelNotFound

                            If ((i2 - (MemAddr + 4)) \ 4) > &H7FFF Or ((i2 - (MemAddr + 4)) \ 4) < &HFFFF8000 Then
                                rt = mpAsm.AssembleInstruction("j " + i2.ToString, CodeRet)
                                If rt < 0 Then GoTo LabelAsmError
                            Else
                                rt = mpAsm.AssembleInstruction("beq zero, zero, " + ((i2 - (MemAddr + 4)) \ 4).ToString, CodeRet)
                                If rt < 0 Then GoTo LabelAsmError
                            End If

                            CodeOutput(I) = CodeFormat + Strings.Right("00000000" + Hex(MemAddr), 7) + " "
                            CodeOutput(I) += Strings.Right("00000000" + Hex(CodeRet), 8)
                            CodeOutput(I) += vbCrLf
                            MemAddr += 4
                            CodeOutput(I) += CodeFormat + Strings.Right("00000000" + Hex(MemAddr), 7) + " "
                            CodeOutput(I) += "00000000"
                        Else
                            GoTo LabelNotFound
                        End If
                    Case Else
                        LabelType = 0
                        If Strings.Left(sp(0), 1) = "b" Then LabelType = 1
                        If Strings.Left(sp(0), 1) = "t" Then LabelType = 1

                        If Strings.Left(sp(1), 1) = ":" Then
                            sp(1) = Right(sp(1), Len(sp(1)) - 1)
                            rt = GetLabel(sp(1), i2)
                            If rt < 0 Then GoTo LabelNotFound

                            If LabelType = 0 Then sp(1) = i2.ToString
                            If LabelType = 1 Then sp(1) = ((i2 - (MemAddr + 4)) \ 4).ToString
                        End If
                        If Strings.Left(sp(2), 1) = ":" Then
                            sp(2) = Right(sp(2), Len(sp(2)) - 1)
                            rt = GetLabel(sp(2), i2)
                            If rt < 0 Then GoTo LabelNotFound

                            If LabelType = 0 Then sp(2) = i2.ToString
                            If LabelType = 1 Then sp(2) = ((i2 - (MemAddr + 4)) \ 4).ToString
                        End If
                        If Strings.Left(sp(3), 1) = ":" Then
                            sp(3) = Right(sp(3), Len(sp(3)) - 1)
                            rt = GetLabel(sp(3), i2)
                            If rt < 0 Then GoTo LabelNotFound

                            If LabelType = 0 Then sp(3) = i2.ToString
                            If LabelType = 1 Then sp(3) = ((i2 - (MemAddr + 4)) \ 4).ToString
                        End If

                        rt = mpAsm.AssembleInstruction(sp(0) + " " + sp(1) + " " + sp(2) + " " + sp(3), CodeRet)
                        If rt < 0 Then GoTo LabelAsmError

                        CodeOutput(I) = CodeFormat + Strings.Right(CodeOutput(I), 8)
                        CodeOutput(I) += Strings.Right("00000000" + Hex(CodeRet), 8)
                End Select
            End If
        Next

        Return 0
LabelNotFound:
        ErrDetail.LineData = sp(0) + " " + sp(1) + " " + sp(2) + " " + sp(3)
        ErrDetail.LineNumber = Val(sp(4))
        ErrDetail.ErrorNumber = SyntaxError_LabelNotFound
        ErrDetail.ExtraDetails = ""

        Return SyntaxError_LabelNotFound
LabelAsmError:
        ErrDetail.LineData = sp(0) + " " + sp(1) + " " + sp(2) + " " + sp(3)
        ErrDetail.LineNumber = Val(sp(4))
        ErrDetail.ExtraDetails = ""
        Select Case rt
            Case -1
                ErrDetail.ErrorNumber = SyntaxError_EmptyString
            Case -2
                ErrDetail.ErrorNumber = SyntaxError_NoSignifcantData
            Case -3
                ErrDetail.ErrorNumber = SyntaxError_UnknownInstruction
                ErrDetail.ExtraDetails = "Bad Instruction: " + sp(0)
            Case -4
                ErrDetail.ErrorNumber = SyntaxError_DataTypeUnknown
                ErrDetail.ExtraDetails = "Bad Argument: " + sp(CodeRet + 1)
            Case -5
                ErrDetail.ErrorNumber = SyntaxError_BadArgumentCount
                ErrDetail.ExtraDetails = "Provied: " + ((CodeRet >> 8) And &HFF).ToString +
                                                         vbCrLf +
                                                         "Required: " + (CodeRet And &HFF).ToString
            Case -6
                ErrDetail.ErrorNumber = SyntaxError_BadArgumentType
                ErrDetail.ExtraDetails = "Bad Argument: " + sp(CodeRet + 1)
        End Select
        Return ErrDetail.ErrorNumber
    End Function

    Public Function Decompile(CodeText As String) As String
        Dim Lines() As String, sp() As String, I As Integer
        Dim MemAddr As Int32, tmpAddr As Int32, ret() As String, retC As Integer
        Dim tmpCode As UInt32

        MemAddr = -1
        ReDim ret(0)
        retC = -1

        Lines = Split(CodeText + vbCrLf, vbCrLf)
        For I = 0 To Lines.Count - 2
            sp = Split(Lines(I) + " ", " ")
            If sp(0) <> "" Then
                tmpAddr = Val("&H" + Strings.Right(sp(0), 7))
                If tmpAddr <> MemAddr Then
                    MemAddr = tmpAddr
                    retC += 1
                    ReDim Preserve ret(retC)
                    ret(retC) = "address $" + Strings.Right("00000000" + Hex(MemAddr), 8) + vbCrLf
                End If
            End If
            If sp(1) <> "" Then
                tmpCode = CDec("&H" + Strings.Right(sp(1), 8))

                retC += 1
                ReDim Preserve ret(retC)
                ret(retC) = mpAsm.DisassembleValue(tmpCode)
            End If
            MemAddr += 4
        Next

        Return Join(ret, vbCrLf)
    End Function

    Private Function ProjectImportScan(Pages() As String)
        Dim ret As String

        ret = SingleImportLibrary(Join(Pages, vbCrLf))

        Return ret
    End Function

    Private Function SingleImportLibrary(PgData As String) As String
        Dim Lines() As String, LineData As String, I As Integer, i2 As Integer, sp() As String

        Dim Libraries() As String, LibCount As Integer
        Dim Calls() As String, CallCount As Integer
        Dim Fncs() As String, FncsCount As Integer

        Dim LibRet As String, Dependancies() As String, Libs() As String, CDL As New CDLibrary
        Dim i3 As Integer, i4 As Integer, Gathered() As String, GatheredC As Integer
        Dim ret As String, eventTable As String, threadTable As String, tStr As String, tStr2 As String
        Dim mainHook As String

        GatheredC = -1
        ret = ""
        eventTable = ""
        threadTable = ""
        mainHook = ""

        LibCount = -1
        CallCount = -1
        FncsCount = -1

        Lines = Split(PgData + vbCrLf, vbCrLf)
        For I = 0 To Lines.Count - 2
            LineData = stripWhiteSpace(Lines(I))
            sp = Split(parseSyntax(LineData) + "        ", " ")
            Select Case LCase(sp(0))
                Case "import"
                    LibCount += 1
                    ReDim Preserve Libraries(LibCount)
                    Libraries(LibCount) = sp(1)
                Case "call"
                    CallCount += 1
                    ReDim Preserve Calls(CallCount)
                    Calls(CallCount) = sp(1)
                Case "fnc"
                    FncsCount += 1
                    ReDim Preserve Fncs(FncsCount)
                    Fncs(FncsCount) = sp(1)
                Case "extern"
                    FncsCount += 1
                    ReDim Preserve Fncs(FncsCount)
                    Fncs(FncsCount) = sp(2)
                Case "prochook"
                    'procHook $00100000 -j/-jal/-pointer

                    mainHook = "hook :_Main_Process_Thread_Manager " + sp(1) + " " + sp(2)

                    'mainHook = "address $" + Strings.Right("00000000" + Hex(Val(Replace(sp(1), "$", "&h0"))), 8) + vbCrLf
                    'If LCase(sp(2)) = "-j" Then
                    'mainHook += "j :_Main_Process_Thread_Manager" + vbCrLf
                    'ElseIf LCase(sp(2)) = "-jal" Then
                    'mainHook += "jal :_Main_Process_Thread_Manager" + vbCrLf
                    'ElseIf LCase(sp(2)) = "-point" Then
                    'mainHook += "hexcode :_Main_Process_Thread_Manager" + vbCrLf
                    'Else
                    'mainHook += "j :_Main_Process_Thread_Manager" + vbCrLf
                    'End If
                Case "thread"
                    'thread main /delay 100
                    threadTable += "hexcode :" + sp(1) + vbCrLf
                    If LCase(sp(2)) = "/delay" Then
                        threadTable += "hexcode " + Val(Replace(sp(3), "$", "&h0")).ToString + vbCrLf
                    Else
                        threadTable += "nop" + vbCrLf
                    End If
                    threadTable += "nop" + vbCrLf

                    If LCase(sp(2)) = "/off" Or LCase(sp(3)) = "/off" Then
                        threadTable += "hexcode 0" + vbCrLf
                    Else
                        If LCase(sp(4)) = "/off" Or LCase(sp(4)) = "/off" Then
                            threadTable += "hexcode 0" + vbCrLf
                        Else
                            threadTable += "hexcode 1" + vbCrLf
                        End If
                    End If
                    AddThread(sp(1))

                Case "event"
                    'event padInput_X $007157dc $fbff /h
                    tStr = Strings.Right("00000000" + Hex(Val(Replace(sp(2), "$", "&h0"))), 8)
                    tStr2 = Strings.Right(tStr, 4)
                    tStr = Strings.Left(tStr, 4)
                    If Val("&H" + tStr2) < 0 Then tStr = Strings.Right("0000" + Hex(Val("&H" + tStr) + 1), 4)

                    eventTable += "lui a2, $" + tStr + vbCrLf
                    eventTable += "lh a0, $" + tStr2 + "(a2)" + vbCrLf

                    tStr = Strings.Right("00000000" + Hex(Val(Replace(sp(3), "$", "&h0"))), 8)
                    If LCase(sp(4)) = "/b" Then
                        eventTable += "addiu a1, zero, $00" + Strings.Right(tStr, 2) + vbCrLf
                    ElseIf LCase(sp(4)) = "/h" Then
                        eventTable += "addiu a1, zero, $" + Strings.Right(tStr, 4) + vbCrLf
                    ElseIf LCase(sp(4)) = "/w" Then
                        eventTable += GenerateSetRegCode(Val("&H" + tStr), "a1") + vbCrLf
                    Else
                        eventTable += GenerateSetRegCode(Val("&H" + tStr), "a1") + vbCrLf
                    End If

                    eventTable += "bne a0, a1, 3" + vbCrLf
                    eventTable += "nop" + vbCrLf
                    eventTable += "jal :" + sp(1) + vbCrLf
                    eventTable += "addiu a2, a2, $" + tStr2 + vbCrLf

            End Select
        Next

        If eventTable <> "" Then
            ret += vbCrLf
            ret += vbCrLf
            ret += "fnc _Main_Process_Event_Handler(void)" + vbCrLf
            ret += "{" + vbCrLf
            ret += eventTable + vbCrLf
            ret += "}" + vbCrLf + vbCrLf
        End If

        If ret <> "" Or threadTable <> "" Then
            debugOut("Notes: Thread management enabled")
            ret += vbCrLf
            ret += vbCrLf
            ret += "fnc _Main_Process_Thread_Manager(void) \at,v0,v1,a0,a1,a2,a3,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,s0,s1,s2,s3,s4,s5,s6,s7,k0,k1,gp,fp" + vbCrLf
            ret += "{" + vbCrLf
            If eventTable <> "" Then ret += "call _Main_Process_Event_Handler()" + vbCrLf
            If threadTable <> "" Then ret += "call _Main_Process_Thread_Launcher()" + vbCrLf
            ret += "}" + vbCrLf
        End If

        If threadTable <> "" Then
            ret += vbCrLf
            ret += "fnc _Main_Process_Thread_Launcher(void) \s0,s1,s2" + vbCrLf
            ret += "{" + vbCrLf
            ret += "setreg s0, :_Main_Process_Thread_Table" + vbCrLf
            ret += "lw t9, $0000(s0)" + vbCrLf


            ret += "while (t9)" + vbCrLf
            ret += "{" + vbCrLf

            ret += "daddu a0, zero, zero" + vbCrLf
            ret += "lw v0, $0004(s0)" + vbCrLf
            ret += "lw v1, $0008(s0)" + vbCrLf
            ret += "lw a1, $000c(s0)" + vbCrLf

            ret += "if (a1 > 0)" + vbCrLf
            ret += "{" + vbCrLf

            ret += "if (v0 > 0)" + vbCrLf
            ret += "{" + vbCrLf +
                        "if (v1 > v0)" + vbCrLf +
                        "{" + vbCrLf +
                            "sw zero, $0008(s0)" + vbCrLf +
                            "addiu a0, zero, 1" + vbCrLf +
                        "}" + vbCrLf +
                        "else" + vbCrLf +
                        "{" + vbCrLf +
                            "addiu v1, v1, 1" + vbCrLf +
                            "sw v1, $0008(s0)" + vbCrLf +
                        "}" + vbCrLf +
                    "}" + vbCrLf +
                    "else" + vbCrLf +
                    "{" + vbCrLf +
                        "addiu a0, zero, 1" + vbCrLf +
                    "}" + vbCrLf +
                    "if (a0 <> 0)" + vbCrLf +
                    "{" + vbCrLf +
                        "jalr t9" + vbCrLf +
                        "nop" + vbCrLf +
                    "}" + vbCrLf

            ret += "}" + vbCrLf

            ret += "s0 += 16" + vbCrLf +
                    "lw t9, $0000(s0)" + vbCrLf

            ret += "}" + vbCrLf
            ret += "}" + vbCrLf
            ret += "_Main_Process_Thread_Table:" + vbCrLf
            ret += threadTable + vbCrLf
        End If
        If ret <> "" And mainHook = "" Then debugOut("Warning: Thread management enabled without declaring process hook")
        If ret = "" And mainHook <> "" Then debugOut("Warning: Process hook declared without using thread management")
        ret += vbCrLf + mainHook + vbCrLf


        For I = 0 To CallCount
            For i2 = 0 To FncsCount
                If Calls(I) = Fncs(i2) Then Calls(I) = ""
            Next
            For i2 = I + 1 To CallCount
                If Calls(I) = Calls(i2) Then Calls(i2) = ""
            Next
        Next
restartGathering:
        ReDim Dependancies(0)
        For I = 0 To CallCount
            For i2 = 0 To GatheredC
                If Calls(I) = Gathered(i2) Then Calls(I) = ""
            Next
            If Calls(I) <> "" Then
                For i2 = 0 To LibCount

                    LibRet = CDL.LoadFromLibrary(Libraries(i2), Calls(I), Dependancies)


                    'If LibRet = "" Then
                    '    debugOut("Library '" + Libraries(i2) + "' either doesn't contain '" + Calls(I) + "' or doesn't exist")
                    '    CreateError("", SyntaxError_LabelNotFound, 0, "", "Import from library failed")
                    '    Return "{%ERROR%}"
                    'End If

                    If LibRet <> "" Then
                        debugOut("Importing '" + Calls(I) + "' from library '" + Libraries(i2) + "'")

                        GatheredC += 1
                        ReDim Preserve Gathered(GatheredC)
                        Gathered(GatheredC) = Calls(I)

                        ret += vbCrLf + LibRet + vbCrLf
                        For i3 = 0 To Dependancies.Count - 1
                            If Dependancies(i3) <> "" Then
                                CallCount += 1
                                ReDim Preserve Calls(CallCount)
                                Calls(CallCount) = Dependancies(i3)
                            End If
                        Next
                        GoTo restartGathering
                    End If
                Next
            End If
        Next

        Return ret
    End Function

    Private Function GetVal(strIn As String) As UInt32
        Dim I As Int64
        I = CDec(Replace(strIn, "$", "&H0"))
        If I < 0 Then
            I = &H100000000 - (0 - I)
            Return I And 4294967295
        Else
            Return I And 4294967295
        End If
    End Function


    Private Function parseSyntax(strIn As String) As String
        Dim ret As String, cmtStrip() As String
        ret = strIn
        If ret = "" Then Return ret

        cmtStrip = Split(ret + "//", "//")
        If Len(cmtStrip(0)) <= 0 Then Return ""
        ret = cmtStrip(0)

        ret = Replace(ret, vbTab, " ")
        ret = Replace(ret, ",", " ")
        ret = Replace(ret, ";", " ")
        ret = Replace(ret, "--", " --")
        ret = Replace(ret, "++", " ++")
        ret = Replace(ret, "(", " ")
        ret = Replace(ret, ")", " ")
        ret = Replace(ret, "0x", "$")
        ret = stripSpaces(ret)
        Return ret
    End Function
    Private Function stripSpaces(strIn As String) As String
        Dim lastLen As Integer, ret As String
        ret = strIn
        Do
            lastLen = Len(ret)
            ret = Replace(ret, "  ", " ")
            If Left(ret, 1).Equals(" ") Then ret = Right(ret, Len(ret) - 1)
            If Right(ret, 1).Equals(" ") Then ret = Left(ret, Len(ret) - 1)
            If ret = "" Then Return ""
        Loop Until lastLen = Len(ret)
        Return ret
    End Function
    Private Function stripWhiteSpace(strIn As String) As String
        Dim ret As String

        ret = strIn
        Do While Strings.Left(ret, 1) = " " Or Strings.Left(ret, 1) = vbTab
            If ret = "" Then Return ""
            ret = Strings.Right(ret, Len(ret) - 1)
        Loop

        Return ret
    End Function



End Class
