Public Class MIPSAssembly
    Private mipsOps() As OperationCode
    Private OpCodes(255) As OpIndexing
    Private assemblerStruct As MIPSAssemblingStructure

    Private IndexOfNOP

    Private Const I_Type = 0
    Private Const J_Type = 1
    Private Const R_Type = 2

    Private Structure ArgMask
        Dim Maskhex As String
        Dim MaskVal As Int64
        Dim DivVal As Int64
        Dim ArgReg As String
    End Structure
    Private Structure OperationCode
        Dim Instruction As String
        Dim Details As String

        Dim Arguments As String
        Dim ParsedSyntax() As String
        Dim HasMultiArgs As Boolean
        Dim MultiArgDefs As String

        Dim BINCode As String
        Dim BINMask As String

        Dim hexCode As String
        Dim HEXMask As String

        Dim DECCode As Int64
        Dim DECMask As Int64

        Dim ArgMasks() As ArgMask
        Dim ArgCount As Integer

        Dim Processor As String

        Dim BitOrder
    End Structure


    Private Structure IndexedSubR3
        Dim StaticIndex As Integer
    End Structure
    Private Structure IndexedSubR2
        Dim SubR3() As IndexedSubR3 '15
        Dim StaticIndex As Integer
    End Structure
    Private Structure IndexedSubR
        Dim SubR2() As IndexedSubR2 '31
        Dim StaticIndex As Integer
    End Structure
    Private Structure IndexedSubI
        Dim StaticIndex As Integer
    End Structure
    Private Structure OpIndexing
        Dim OpType As Integer
        Dim SubR() As IndexedSubR '63
        Dim SubI() As IndexedSubI '31
        Dim StaticIndex As Integer
    End Structure

    Public Sub Shutdown()
        ReDim mipsOps(0)
    End Sub
    Private Sub initStruct()

        '---------------------------------------------------- EE
        assemblerStruct.rs = Function(r As UInt32)
                                 assemblerStruct.u32 = (assemblerStruct.u32 And 4229955583) Or (r << 21)
                                 Return 0
                             End Function
        assemblerStruct.rt = Function(r As UInt32)
                                 assemblerStruct.u32 = (assemblerStruct.u32 And 4292935679) Or (r << 16)
                                 Return 0
                             End Function
        assemblerStruct.rd = Function(r As UInt32)
                                 assemblerStruct.u32 = (assemblerStruct.u32 And 4294903807) Or (r << 11)
                                 Return 0
                             End Function
        assemblerStruct.sa = Function(r As UInt32)
                                 assemblerStruct.u32 = (assemblerStruct.u32 And 4294965311) Or (r << 6)
                                 Return 0
                             End Function
        assemblerStruct.target = Function(t As Int32)
                                     assemblerStruct.u32 = (assemblerStruct.u32 And 4261412864) Or (t \ 4)
                                     Return 0
                                 End Function

        '---------------------------------------------------- COP1
        assemblerStruct.ft = Function(f As UInt32)
                                 assemblerStruct.u32 = (assemblerStruct.u32 And 4292935679) Or (f << 16)
                                 Return 0
                             End Function
        assemblerStruct.fs = Function(f As UInt32)
                                 assemblerStruct.u32 = (assemblerStruct.u32 And 4294903807) Or (f << 11)
                                 Return 0
                             End Function
        assemblerStruct.fd = Function(f As UInt32)
                                 assemblerStruct.u32 = (assemblerStruct.u32 And 4294965311) Or (f << 6)
                                 Return 0
                             End Function

        '---------------------------------------------------- COP0
        assemblerStruct.reg = Function(c As UInt32)
                                  assemblerStruct.u32 = (assemblerStruct.u32 And 4294903807) Or (c << 11)
                                  Return 0
                              End Function
        assemblerStruct.sel = Function(c As UInt32)
                                  assemblerStruct.u32 = (assemblerStruct.u32 And 4294965311) Or (c << 6)
                                  Return 0
                              End Function

        '---------------------------------------------------- CACHE
        assemblerStruct.cvar = Function(cv As UInt32)
                                   assemblerStruct.u32 = (assemblerStruct.u32 And 4292935679) Or (cv << 16)
                                   Return 0
                               End Function

    End Sub
    Public Function init() As Integer
        Dim rt As Integer

        initStruct()

        rt = Initialize_MIPS_R5900()
        StripDuplicateOps()
        SortMIPS()

        CalculateOpValues()

        GenerateArgMasks()
        BuildIndexes()

        Return mipsOps.Count
    End Function

    Public Function DumpOps() As String
        Dim I As Int32, ret As String

        ret = ""
        For I = 0 To mipsOps.Count - 1
            ret = ret + Left(mipsOps(I).Instruction + "          ", 10) + " " + mipsOps(I).hexCode + vbCrLf
        Next

        Return ret
    End Function

    Public Function GetInstrName(Index As Integer) As String
        Return mipsOps(Index).Instruction
    End Function

    Public Function AssembleInstruction(strInstruct As String, ByRef output As UInt32) As Integer
        Dim strLine As String, Sp() As String, argCount As Integer, Index As Integer, rt As SByte
        Dim parsed() As String, I As Integer, isCountRight As Boolean, DefaultVal() As String

        If strInstruct Is Nothing Then Return -1
        If strInstruct.Length < 1 Then Return -1 'Empty string
        strLine = parseSyntax(strInstruct)
        If strLine.Length < 1 Then Return -2 'String contains no significant data

        Sp = Split(strLine + " ", " ")
        Index = InstrToIndex(Sp(0))
        If Index < 0 Then Return -3 'Unknown Instruction

        argCount = Sp.Count - 2
        If mipsOps(Index).Arguments = "" And argCount < 1 Then
            output = mipsOps(Index).DECCode
            Return 0
        End If

        isCountRight = False
        parsed = Split(mipsOps(Index).ParsedSyntax(0) + " ", " ")
        If argCount = parsed.Count - 1 Then isCountRight = True
        If mipsOps(Index).HasMultiArgs Then DefaultVal = Split(LCase(mipsOps(Index).MultiArgDefs), "=")
        If mipsOps(Index).HasMultiArgs And isCountRight = False Then
            parsed = Split(mipsOps(Index).ParsedSyntax(1) + " ", " ")
            If argCount = parsed.Count - 1 Then isCountRight = True
        End If

        If isCountRight Then
            assemblerStruct.u32 = mipsOps(Index).DECCode

            If mipsOps(Index).HasMultiArgs Then
                Select Case DefaultVal(0)
                    Case "rd"
                        assemblerStruct.rd(Val(DefaultVal(1)))
                    Case "sel"
                        assemblerStruct.sel(Val(DefaultVal(1)))
                    Case Else
                        MsgBox("Missing: " + DefaultVal(0))
                End Select
            End If

            For I = 0 To parsed.Count - 1

                Select Case parsed(I)
                    Case ""
                        'Nothing to do here
                    Case "rs"
                        rt = GetEERegVal(Sp(I + 1))
                        If rt < 0 Then
                            output = I 'Specify argument index
                            Return -6 'Invalid Argument
                        End If
                        assemblerStruct.rs(rt)
                    Case "rt"
                        rt = GetEERegVal(Sp(I + 1))
                        If rt < 0 Then
                            output = I 'Specify argument index
                            Return -6 'Invalid Argument
                        End If
                        assemblerStruct.rt(rt)
                    Case "rd"
                        rt = GetEERegVal(Sp(I + 1))
                        If rt < 0 Then
                            output = I 'Specify argument index
                            Return -6 'Invalid Argument
                        End If
                        assemblerStruct.rd(rt)
                    Case "sa"
                        Sp(I + 1) = Replace(Sp(I + 1), "$", "&h0")
                        Sp(I + 1) = Replace(Sp(I + 1), "0x", "&h0")
                        assemblerStruct.sa(Val(Sp(I + 1)) And 31)
                    Case "base"
                        rt = GetEERegVal(Sp(I + 1))
                        If rt < 0 Then
                            output = I 'Specify argument index
                            Return -6 'Invalid Argument
                        End If
                        assemblerStruct.rs(rt)
                    Case "hint"
                        rt = GetEERegVal(Sp(I + 1))
                        If rt < 0 Then
                            output = I 'Specify argument index
                            Return -6 'Invalid Argument
                        End If
                        assemblerStruct.rt(rt)
                    Case "%i"
                        Sp(I + 1) = Replace(Sp(I + 1), "$", "&h0")
                        Sp(I + 1) = Replace(Sp(I + 1), "0x", "&h0")
                        assemblerStruct.s64 = Val(Sp(I + 1))
                        assemblerStruct.u16 = assemblerStruct.uImm
                    Case "target"
                        Sp(I + 1) = Replace(Sp(I + 1), "$", "&h0")
                        Sp(I + 1) = Replace(Sp(I + 1), "0x", "&h0")
                        Try
                            assemblerStruct.target(CDec(Sp(I + 1)))
                        Catch
                            Return -6 'Invalid Argument
                        End Try
                    Case "%d"
                            Sp(I + 1) = Replace(Sp(I + 1), "$", "")
                        Sp(I + 1) = Replace(Sp(I + 1), "0x", "")
                        If Sp(0) = "syscall" Then
                            assemblerStruct.u32 = (assemblerStruct.u32 And 4227858495) Or ((Val("&H" + Sp(I + 1)) And 1048575) << 6)
                        ElseIf Sp(0) = "break" Then
                            assemblerStruct.u32 = (assemblerStruct.u32 And 4227858495) Or ((Val("&H" + Sp(I + 1)) And 1048575) << 6)
                        Else
                            assemblerStruct.u32 = (assemblerStruct.u32 And 4294901823) Or ((Val("&H" + Sp(I + 1)) And 1023) << 6)
                        End If
                    Case "fd"
                        rt = GetCOP1RegVal(Sp(I + 1))
                        If rt < 0 Then
                            output = I 'Specify argument index
                            Return -6 'Invalid Argument
                        End If
                        assemblerStruct.fd(rt)
                    Case "fs"
                        rt = GetCOP1RegVal(Sp(I + 1))
                        If rt < 0 Then
                            output = I 'Specify argument index
                            Return -6 'Invalid Argument
                        End If
                        assemblerStruct.fs(rt)
                    Case "ft"
                        rt = GetCOP1RegVal(Sp(I + 1))
                        If rt < 0 Then
                            output = I 'Specify argument index
                            Return -6 'Invalid Argument
                        End If
                        assemblerStruct.ft(rt)
                    Case "reg"
                        rt = GetCOP0RegVal(Sp(I + 1))
                        If rt < 0 Then
                            output = I 'Specify argument index
                            Return -6 'Invalid Argument
                        End If
                        assemblerStruct.reg(rt)
                    Case "sel"
                        rt = GetCOP0RegVal(Sp(I + 1))
                        If rt < 0 Then
                            output = I 'Specify argument index
                            Return -6 'Invalid Argument
                        End If
                        assemblerStruct.sel(rt)
                    Case "cvar"
                        rt = GetCACHEVarVal(Sp(I + 1))
                        If rt < 0 Then
                            output = I 'Specify argument index
                            Return -6 'Invalid Argument
                        End If
                        assemblerStruct.cvar(rt)
                    Case Else
                        MsgBox("Missing: " + parsed(I))
                        Return -4 'Must be incomplete var list?
                End Select
            Next

            output = assemblerStruct.u32
            Return 0 'Success
        End If

        output = (argCount << 8) + (parsed.Count - 1)
        Return -5 'Invalid argument count
    End Function

    Public Function DisassembleValue(decCode As Int64) As String
        Return Disassemble_WI(decCode, FetchInstr(decCode))
    End Function
    Public Function Disassemble_WI(decVal As Int64, Index As Integer) As String
        Dim I As Int64, argOut As String
        Dim mulArgs() As String, hasRD As Boolean, RDVal As Integer, s() As String

        Dim Cache As Int64
        Dim Cache2(3) As String

        argOut = ""
        hasRD = False
        If Index < 0 Then Return "hexcode $" + Right("00000000" + Hex(decVal), 8)

        If mipsOps(Index).HasMultiArgs Then
            s = Split(mipsOps(Index).MultiArgDefs, "=")
            RDVal = Val(s(1))
        End If

        If mipsOps(Index).ArgCount > 0 Then

            For I = 0 To mipsOps(Index).ArgMasks.Count() - 1
                With mipsOps(Index).ArgMasks(I)

                    Cache = (decVal And .MaskVal) / .DivVal

                    Select Case .ArgReg
                        Case "rs"
                            Cache2(I) = GetEERegStr(Cache)
                        Case "rd"
                            Cache2(I) = GetEERegStr(Cache)
                            If Cache <> RDVal Then hasRD = True
                        Case "rt"
                            Cache2(I) = GetEERegStr(Cache)
                        Case "base"
                            Cache2(I) = GetEERegStr(Cache)
                        Case "hint"
                            Cache2(I) = GetEERegStr(Cache)
                        Case "%i"
                            Cache2(I) = "$" + Right("0000" + Hex(Cache), 4)
                        Case "target"
                            Cache2(I) = "$" + Right("00000000" + Hex((Cache * 4)), 8)
                        Case "%d"
                            Cache2(I) = Right("00000" + Hex(Cache), 5)
                        Case "fd"
                            Cache2(I) = GetCOP1RegStr(Cache)
                        Case "fs"
                            Cache2(I) = GetCOP1RegStr(Cache)
                        Case "ft"
                            Cache2(I) = GetCOP1RegStr(Cache)
                        Case "reg"
                            Cache2(I) = GetCOP0RegStr(Cache)
                        Case "sel"
                            Cache2(I) = Cache.ToString
                        Case "cvar"
                            Cache2(I) = GetCACHEVarStr(Cache)
                        Case Else
                            Cache2(I) = Cache.ToString
                    End Select

                End With
            Next I
        End If

        If mipsOps(Index).HasMultiArgs Then
            mulArgs = Split(mipsOps(Index).Arguments, vbCrLf)
            If hasRD = False Then argOut = FormatArgumentStr2(mulArgs(0), Index, Cache2)
            If hasRD = True Then argOut = FormatArgumentStr2(mulArgs(1), Index, Cache2)
        Else
            argOut = FormatArgumentStr2(mipsOps(Index).Arguments, Index, Cache2)
        End If

        If argOut = "" Then
            Return LCase(mipsOps(Index).Instruction)
        Else
            Return LCase(mipsOps(Index).Instruction + " " + argOut)
        End If

    End Function
    Public Function FormatArgumentStr2(Arguments As String, Index As Integer, aCache2() As String) As String
        Dim I As Int64, i2 As Int64, ret() As String, rI As Integer, ArgBits() As String, DidCopy As Boolean

        rI = 0
        For I = 1 To Len(Arguments)
            For i2 = 0 To mipsOps(Index).ArgMasks.Count() - 1
                With mipsOps(Index).ArgMasks(i2)

                    If LCase(Mid(Arguments, I, Len(.ArgReg))) = LCase(.ArgReg) Then

                        ReDim Preserve ret(rI)
                        ret(rI) = aCache2(i2)
                        rI += 1
                        I += Len(.ArgReg)

                    End If

                End With
            Next i2

            ReDim Preserve ret(rI)
            ret(rI) = Mid(Arguments, I, 1)
            rI += 1
        Next I

        Return Join(ret, "")
    End Function



    Public Function FetchRegs(DECCode As Int64, InstrIndex As Integer, ByRef dCache() As Int32) As Integer
        Dim I As Int64

        With mipsOps(InstrIndex)
            For I = 0 To mipsOps(InstrIndex).ArgCount - 1
                dCache(I) = (DECCode And .ArgMasks(I).MaskVal) \ .ArgMasks(I).DivVal
            Next

            Return .ArgCount
        End With

    End Function
    Public Function FetchInstr(DECCode As Int64) As Integer
        Dim vOpCode As Byte, vSubI As Byte, vSubR1 As Byte, vSubR2 As Byte, vSubR3 As Byte
        Dim mOpIndex As Integer, Retried As Boolean

        If DECCode = 0 Then Return IndexOfNOP

        Retried = False

        vOpCode = (DECCode \ &H1000000) And &HFF
doRetry:
        If OpCodes(vOpCode).OpType = I_Type Then
            mOpIndex = OpCodes(vOpCode).StaticIndex
            If mOpIndex < 0 Then
                '---------------------------------------------------- SubI
                vSubI = (DECCode \ &H10000) And &H1F
                mOpIndex = OpCodes(vOpCode).SubI(vSubI).StaticIndex
            End If
        ElseIf OpCodes(vOpCode).OpType = J_Type Then
            mOpIndex = OpCodes(vOpCode).StaticIndex
        ElseIf OpCodes(vOpCode).OpType = R_Type Then
            mOpIndex = OpCodes(vOpCode).StaticIndex
            If mOpIndex < 0 Then
                '---------------------------------------------------- SubR1
                vSubR1 = DECCode And &H3F
                mOpIndex = OpCodes(vOpCode).SubR(vSubR1).StaticIndex
                If mOpIndex < 0 Then
                    '---------------------------------------------------- SubR2
                    vSubR2 = (DECCode \ 64) And &H1F
                    mOpIndex = OpCodes(vOpCode).SubR(vSubR1).SubR2(vSubR2).StaticIndex
                    If mOpIndex < 0 Then
                        '---------------------------------------------------- SubR3
                        vSubR3 = (DECCode \ &H100000) And &HE
                        mOpIndex = OpCodes(vOpCode).SubR(vSubR1).SubR2(vSubR2).SubR3(vSubR3).StaticIndex
                    End If
                End If
            End If
        Else
            If Retried = False Then
                Retried = True
                vOpCode = (DECCode \ &H1000000) And &HFC
                GoTo doRetry
            End If
            Return -2 'Invalid
            Exit Function
        End If

        If mOpIndex < 0 Then
            If Retried = False Then
                Retried = True
                vOpCode = (DECCode \ &H1000000) And &HFC
                GoTo doRetry
            End If
            Return -1 'Unknown
        Else
            Return mOpIndex 'Success
        End If

    End Function

    Public Function InstrToIndex(strInst As String) As Integer
        Dim high As Integer, low As Integer, i As Integer
        Dim strIn As String

        strIn = UCase(strInst)
        low = 0
        high = mipsOps.Count - 1

        While (low <= high)
            i = (low + high) \ 2
            Select Case Strings.StrComp(strIn, mipsOps(i).Instruction)
                Case -1
                    high = i - 1
                Case 0
                    Return i
                Case 1
                    low = i + 1
            End Select
        End While

        Return -1
    End Function




    Private Function Initialize_MIPS_R5900() As Integer
        Dim Ops() As String, Lines() As String, sp() As String, I As Int32
        Dim i2 As Int32

        Ops = Split(MIPS_File, "========================================" + vbCrLf)
        ReDim mipsOps(Ops.Count - 2)

        For I = 1 To Ops.Count - 1
            With mipsOps(I - 1)
                Lines = Split(Ops(I), vbCrLf)

                '--------------------------------------------------- Instruction String / Details
                sp = Split(Lines(0), " : ")
                .Instruction = sp(0)
                .Details = sp(1)

                '--------------------------------------------------- Argument Setups
                i2 = 1
                .Arguments = ""
                Do Until Left(Lines(i2), Len(.Instruction)) <> .Instruction
                    If Len(Lines(i2)) > Len(.Instruction) Then .Arguments = .Arguments + Right(Lines(i2), Len(Lines(i2)) - (Len(.Instruction) + 1)) + vbCrLf
                    i2 = i2 + 1
                Loop
                Do Until Right(.Arguments, 2) <> vbCrLf
                    .Arguments = Left(.Arguments, Len(.Arguments) - 2)
                Loop
                .Arguments = Replace(.Arguments, "immediate", "%i")
                .Arguments = Replace(.Arguments, "offset", "%i")
                .Arguments = Replace(.Arguments, "code", "%d")
                sp = Split(.Arguments, vbCrLf)
                If sp.Count - 1 > 0 Then .HasMultiArgs = True
                If sp.Count - 1 <= 0 Then .HasMultiArgs = False

                .ParsedSyntax = Split(.Arguments + vbCrLf, vbCrLf)
                'ReDim Preserve .ParsedSyntax(.ParsedSyntax.Count - 2)
                .ParsedSyntax(0) = parseSyntax(.ParsedSyntax(0))
                If .HasMultiArgs Then .ParsedSyntax(1) = parseSyntax(.ParsedSyntax(1))

                '--------------------------------------------------- Binary Setup
                .BINCode = Lines(i2)
                .BINMask = Lines(i2 + 1)
                i2 = i2 + 2

                '--------------------------------------------------- Processor
                .Processor = Lines(i2)
                i2 = i2 + 1

                '--------------------------------------------------- Bit Order setup
                .BitOrder = ""
                Do Until i2 = Lines.Count
                    .BitOrder = .BitOrder + Lines(i2) + vbCrLf
                    i2 = i2 + 1
                Loop
                Do Until Right(.BitOrder, 2) <> vbCrLf
                    .BitOrder = Left(.BitOrder, Len(.BitOrder) - 2)
                Loop
                .BitOrder = Replace(.BitOrder, "immediate", "%i")
                .BitOrder = Replace(.BitOrder, "offset", "%i")
                .BitOrder = Replace(.BitOrder, "code", "%d")

                '--------------------------------------------------- Multi Argument Settings
                .MultiArgDefs = ""
                If .HasMultiArgs Then
                    sp = Split(.BitOrder, vbCrLf)
                    .MultiArgDefs = sp(sp.Count - 1)
                    ReDim Preserve sp(sp.Count - 2)
                    .BitOrder = Join(sp, vbCrLf)
                End If

            End With
        Next I

        Return mipsOps.Count
    End Function

    Private Function parseSyntax(strIn As String) As String
        Dim ret As String, cmtStrip() As String
        ret = LCase(strIn)
        If ret = "" Then Return ret

        cmtStrip = Split(ret + "//", "//")
        ret = cmtStrip(0)

        ret = Replace(ret, ",", " ")
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
            lastLen = ret.Length
            ret = Replace(ret, "  ", " ")
            If Left(ret, 1).Equals(" ") Then ret = Right(ret, ret.Length - 1)
            If Right(ret, 1).Equals(" ") Then ret = Left(ret, ret.Length - 1)
            If ret = "" Then Return ""
        Loop Until lastLen = ret.Length
        Return ret
    End Function


    Private Sub SortMIPS()
        Dim I As Integer, tmp As OperationCode, didSwitch As Integer
        Dim rt As Integer

        didSwitch = True

RestartLoop:
        didSwitch = 0
        For I = 0 To mipsOps.Count() - 2
            If mipsOps(I + 1).Instruction IsNot Nothing Then
                rt = String.Compare(mipsOps(I).Instruction, mipsOps(I + 1).Instruction)
                If rt > 0 Then

                    copyMIPOps(mipsOps(I), tmp)
                    copyMIPOps(mipsOps(I + 1), mipsOps(I))
                    copyMIPOps(tmp, mipsOps(I + 1))

                    didSwitch = 1
                End If
            End If
        Next I
        If didSwitch = 1 Then GoTo RestartLoop

        tmp = Nothing
    End Sub

    Private Sub StripDuplicateOps()
        Dim I As Integer, I2 As Integer
restartScan:
        For I = 0 To mipsOps.Count - 1
            For I2 = I + 1 To mipsOps.Count - 1
                If mipsOps(I).Instruction.CompareTo(mipsOps(I2).Instruction) = 0 Then
                    If mipsOps(I).Arguments.Length < mipsOps(I2).Arguments.Length Then
                        DeleteOp(I)
                        GoTo restartScan
                    Else
                        DeleteOp(I2)
                    End If
                End If
            Next
        Next

    End Sub

    Private Function CalculateOpValues()
        Dim I As Integer

        For I = 0 To mipsOps.Count - 1
            With mipsOps(I)
                .DECCode = BinToDec(.BINCode)
                .DECMask = BinToDec(.BINMask)

                .hexCode = Right("00000000" & Hex(.DECCode), 8)
                .HEXMask = Right("00000000" & Hex(.DECMask), 8)

            End With
        Next I
    End Function

    Public Sub DeleteOp(Index As Integer)
        Dim I As Integer

        For I = Index To mipsOps.Count - 2
            copyMIPOps(mipsOps(I + 1), mipsOps(I))
        Next I

        If mipsOps.Count > 0 Then
            mipsOps(mipsOps.Count - 1) = Nothing
            ReDim Preserve mipsOps(mipsOps.Count - 2)
        Else
            ReDim Preserve mipsOps(0)
        End If
    End Sub


    Public Sub GenerateArgMasks()
        Dim I As Int64, i2 As Int64, i3 As Int64, shiftBy As Integer, bI As Integer, ret As String, ArgOrder() As String, ArgBits() As String
        Dim HEXMask As String, tmp As String, divBy As Int64, useI As Integer

        useI = 0
        For I = 0 To mipsOps.Count() - 1
            With mipsOps(I)
                ArgOrder = Split(mipsOps(I).BitOrder, vbCrLf)

                '00000011111000000000000000000000
                '00000000001000000000000000000000

                shiftBy = 32
                bI = 1
                useI = 0
                .ArgCount = 0

                For i2 = 0 To ArgOrder.Count() - 1
                    ArgBits = Split(ArgOrder(i2), ":")

                    shiftBy = shiftBy - Val(ArgBits(0))
                    tmp = ""
                    For i3 = 1 To shiftBy
                        tmp += "0"
                    Next i3
                    HEXMask = ""
                    For i3 = 1 To Val(ArgBits(0))
                        HEXMask += "1"
                    Next i3
                    HEXMask = Right("00000000" + Hex(BinToDec(HEXMask + tmp)), 8)
                    divBy = (2 ^ shiftBy)
                    If UsesArg(ArgBits(1), mipsOps(I).Arguments) Then
                        ReDim Preserve mipsOps(I).ArgMasks(useI)
                        .ArgMasks(useI).Maskhex = HEXMask
                        .ArgMasks(useI).MaskVal = CDec("&H" + HEXMask)
                        .ArgMasks(useI).DivVal = divBy
                        .ArgMasks(useI).ArgReg = ArgBits(1)
                        useI = useI + 1
                        .ArgCount = useI
                    End If

                    bI = bI + Val(ArgBits(0))
                Next i2
            End With
        Next I

    End Sub

    Private Function UsesArg(regStr As String, ArgList As String) As Boolean
        Dim tmp As String, s() As String, I As Integer

        UsesArg = False

        tmp = ArgList
        tmp = Replace(tmp, vbCrLf, " ")
        tmp = Replace(tmp, ",", " ")
        tmp = Replace(tmp, "(", " ")
        tmp = Replace(tmp, ")", " ")
        s = Split(tmp + " ", " ")
        For I = 0 To s.Count() - 1
            If LCase(s(I)) = LCase(regStr) Then UsesArg = True
        Next I
    End Function






    Private Function BuildIndexes()
        Dim I As Integer, i2 As Integer, i3 As Integer, i4 As Integer, tStr As String, vOpCode As Byte, vSubI As Byte, vSubR1 As Byte, vSubR2 As Byte, vSubR3 As Byte
        Dim sp() As String, tmpType As Integer, tVal As Int64

        '============================================================================= Create fresh table
        For I = 0 To 255
            OpCodes(I).StaticIndex = -1

            ReDim OpCodes(I).SubR(63)
            For i2 = 0 To 63
                OpCodes(I).SubR(i2).StaticIndex = -1
                ReDim OpCodes(I).SubR(i2).SubR2(31)
                For i3 = 0 To 31
                    OpCodes(I).SubR(i2).SubR2(i3).StaticIndex = -1
                    ReDim OpCodes(I).SubR(i2).SubR2(i3).SubR3(15)
                    For i4 = 0 To 15
                        OpCodes(I).SubR(i2).SubR2(i3).SubR3(i4).StaticIndex = -1
                    Next i4
                Next i3
            Next i2

            ReDim OpCodes(I).SubI(31)
            For i2 = 0 To 31
                OpCodes(I).SubI(i2).StaticIndex = -1
            Next i2

            OpCodes(I).OpType = -1
        Next I

        '============================================================================= Populate the table
        For I = 0 To mipsOps.Count() - 1
            With mipsOps(I)

                If .Instruction.Equals("NOP") Then
                    IndexOfNOP = I
                    GoTo skipForIType
                End If

                tVal = CLng("&H" + .hexCode)
                If tVal < 0 Then tVal = (CLng("&H100000000") - (0 - tVal))

                vOpCode = Int(tVal / &H1000000) And &HFF
                vSubI = Int((tVal - (Int(tVal / &H1000000) * &H1000000)) / &H10000) And &H1F
                vSubR1 = (tVal - (Int(tVal / &H100) * &H100)) And &H3F
                vSubR2 = Int(tVal / (2 ^ 6)) And &H1F
                vSubR3 = Int((tVal - (Int(tVal / &H1000000) * &H1000000)) / &H100000) And &HE

                tmpType = -1
                For i2 = 1 To Len(.Arguments) - 1
                    If Mid(.Arguments, i2, 2) = "%i" Then tmpType = I_Type
                Next i2
                If tmpType < 0 Then
                    If LCase(.Instruction) = "j" Or LCase(.Instruction) = "jal" Then
                        tmpType = J_Type
                    Else
                        tmpType = R_Type
                    End If
                End If

            End With

            With OpCodes(vOpCode)
                .OpType = tmpType
                If .StaticIndex = -1 Then
                    .StaticIndex = I
                Else
                    .StaticIndex = -2
                End If

                If .OpType = J_Type Then GoTo skipForIType

                If .OpType = I_Type Then
                    .SubI(vSubI).StaticIndex = I
                    GoTo skipForIType
                End If
            End With

            With OpCodes(vOpCode).SubR(vSubR1)
                If .StaticIndex = -1 Then
                    .StaticIndex = I
                Else
                    .StaticIndex = -2
                End If
            End With
            With OpCodes(vOpCode).SubR(vSubR1).SubR2(vSubR2)
                If .StaticIndex = -1 Then
                    .StaticIndex = I
                Else
                    .StaticIndex = -2
                End If
            End With
            With OpCodes(vOpCode).SubR(vSubR1).SubR2(vSubR2).SubR3(vSubR3)
                If .StaticIndex = -1 Then
                    .StaticIndex = I
                Else
                    .StaticIndex = -2
                End If
            End With
skipForIType:
        Next I

    End Function

    Public Function DumpInstructionSet() As String
        Dim I As Int64, ret() As String, i2 As Integer

        ReDim ret(mipsOps.Count() - 1)
        For I = 0 To mipsOps.Count() - 1
            With mipsOps(I)
                ret(I) = .Instruction + ";" + .Processor + ";" + .ArgCount.ToString + ";" + Replace(.Arguments, vbCrLf, " | ") + ";"
                If .ArgCount > 0 Then
                    For i2 = 0 To .ArgMasks.Count() - 1
                        ret(I) = ret(I) + .ArgMasks(i2).ArgReg + ";"
                    Next i2
                End If
            End With
        Next I

        Return Join(ret, vbCrLf)
    End Function


    Private Sub copyMIPOps(srcM As OperationCode, ByRef dstM As OperationCode)
        With dstM
            .Instruction = srcM.Instruction
            .Details = srcM.Details
            .Arguments = srcM.Arguments
            .HasMultiArgs = srcM.HasMultiArgs
            .MultiArgDefs = srcM.MultiArgDefs
            .ParsedSyntax = Split(Join(srcM.ParsedSyntax, vbCrLf), vbCrLf)
            .BINCode = srcM.BINCode
            .BINMask = srcM.BINMask
            .hexCode = srcM.hexCode
            .HEXMask = srcM.HEXMask
            .DECCode = srcM.DECCode
            .DECMask = srcM.DECMask
            .BitOrder = srcM.BitOrder
            .Processor = srcM.Processor
        End With
    End Sub

End Class
