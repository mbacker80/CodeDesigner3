Public Class EEProcessor

    Private mpAsm As MIPSAssembly, asmOp() As MIPSOperation

    '=============================================================== Registers
    Private GPr(32) As Register128, HIloSA(2) As Register128
    Private CPr(31) As UInt32
    Private FPr(31) As RegisterFloat, FCr(31) As RegisterFloat, FAcc As RegisterFloat
    Private rc(10) As Int32
    Private PerformanceCounter As Int64

    '=============================================================== EE Positioning
    Private GlobalAddress As Int32, GlobalNewAddress As Int32, GlobalSpace As Int16
    Private GlobalIndex As Int32, GlobalNewIndex As Int32, GlobalMaxIndex As Int32
    Private isBranching As Byte

    Private LoadAddr As Int32, StoreAddr As Int32

    '=============================================================== Memory Structures
    Private MemData32 As Word32Bit, asmStruct As MIPSAsmStructure

    '=============================================================== Error Returns
    Private Const NoError = 0
    Private Const UnknownInstruction = -1
    Private Const User_Break = -98
    Private Const NotImplementedYet = -99

    Private Const EXCEPTION_TLB_Modification = -2
    Private Const EXCEPTION_TLB_Load_InstFetch = -3
    Private Const EXCEPTION_TLB_Store = -4
    Private Const EXCEPTION_TLB_AddressLoad_InstFetch = -5
    Private Const EXCEPTION_TLB_AddressStore = -6
    Private Const EXCEPTION_TLB_BusError_Instr = -7
    Private Const EXCEPTION_TLB_BusError_Data = -8
    Private Const EXCEPTION_TLB_SysCall = -9
    Private Const EXCEPTION_TLB_BreakPoint = -10
    Private Const EXCEPTION_TLB_Reserved_Instr = -11
    Private Const EXCEPTION_TLB_CoprocesserUnusable = -12
    Private Const EXCEPTION_TLB_Overflow = -13
    Private Const EXCEPTION_TLB_Unknown = -14

    Public vRAM(8388608) As UInt32

    '=============================================================== Structures
    Private Structure MIPSOperation
        Dim Instruction As String
        Dim Processor As String
        Dim Syntax As String

        Dim ArgCount As Integer
        Dim ArgOrder() As String

        Dim Exec As Func(Of Integer)
    End Structure

    '=============================================================== Properties
    Public Property Position() As Int32
        Get
            Return GlobalIndex * 4 'RevertMemAddress(GlobalIndex * 4, GlobalSpace)
        End Get
        Set(newPos As Int32)
            GlobalAddress = newPos
            GlobalIndex = newPos \ 4
            'GlobalIndex = PatchMemAddress(newPos, GlobalSpace)
            'If GlobalIndex >= 0 And GlobalSpace >= 0 Then
            'GlobalIndex \= 4
            'GlobalMaxIndex = vRAM.Count - 1 'PSMemory(GlobalSpace).W.Count - 1
            'Else
            'GlobalMaxIndex = -1
            'End If
        End Set
    End Property
    Public Property PerfCount() As Int64
        Get
            Return PerformanceCounter
        End Get
        Set(value As Int64)
            PerformanceCounter = value
        End Set
    End Property
    '=============================================================== Initialization
    Public Sub FlushRegisters()
        Dim I As Int32

        For I = 0 To 31
            GPr(I).u64_1 = 0
            GPr(I).u64_2 = 0
            GPr(I).u64_3 = 0
            GPr(I).u64_4 = 0
            CPr(I) = 0
            FPr(I).u32 = 0
            FCr(I).u32 = 0
            If I < 3 Then
                HIloSA(I).u64_1 = 0
                HIloSA(I).u64_2 = 0
                HIloSA(I).u64_3 = 0
                HIloSA(I).u64_4 = 0
            End If
        Next
    End Sub
    Private Sub InitAsmStruct()
        asmStruct.loadBytes = Function(arr() As Byte, ind As Int32)
                                  asmStruct.b1 = arr(ind)
                                  asmStruct.b2 = arr(ind + 1)
                                  asmStruct.b3 = arr(ind + 2)
                                  asmStruct.b4 = arr(ind + 3)
                                  Return 0
                              End Function

        asmStruct.rs = Function()
                           Return ((asmStruct.b4 And 3) << 3) + ((asmStruct.b3 And 224) >> 5)
                       End Function

        asmStruct.rt = Function()
                           Return (asmStruct.b3 And 31)
                       End Function

        asmStruct.rd = Function()
                           Return ((asmStruct.b2 And 248) >> 3)
                       End Function

        asmStruct.sa = Function()
                           Return ((asmStruct.b2 And 7) << 2) + ((asmStruct.b1 And 192) >> 6)
                       End Function
        asmStruct.target = Function()
                               Return (asmStruct.u32 And 33554431)
                           End Function
    End Sub

    Public Sub Init(ByRef mAsm As MIPSAssembly)
        Dim s() As String, s2() As String, i As Integer

        mpAsm = mAsm
        InitAsmStruct()

        '------------------------------------------------------- Set up function table
        s = Split(mpAsm.DumpInstructionSet, vbCrLf)
        ReDim asmOp(s.Count - 1)
        For i = 0 To s.Count - 1
            With asmOp(i)
                s2 = Split(s(i), ";")

                .Instruction = s2(0)
                .Processor = s2(1)
                .ArgCount = s2(2)
                .Syntax = s2(3)

                If .ArgCount > 0 Then
                    ReDim .ArgOrder(.ArgCount)
                    For i2 = 1 To .ArgCount
                        .ArgOrder(i2 - 1) = s2(i2 + 3)
                    Next i2
                End If

            End With
        Next i
        InitOps()

        '------------------------------------------------------- Flush All Registers
        FlushRegisters()

        GlobalAddress = Val("&H80000000")
        GlobalIndex = -1
        isBranching = 0
        GlobalMaxIndex = vRAM.Count - 1
    End Sub

    Private Function SetIndex() As Integer
        If GlobalIndex < 0 Then
            If GlobalAddress < -2146959360 Then GlobalAddress += 2147483648

            GlobalIndex = GlobalAddress 'PatchMemAddress(GlobalAddress, GlobalSpace)
            If GlobalIndex < 0 Or GlobalSpace < 0 Then Return EXCEPTION_TLB_AddressLoad_InstFetch
            GlobalIndex \= 4
            GlobalMaxIndex = vRAM.Count - 1 'PSMemory(GlobalSpace).IC.Count - 1
        End If

        Return 0
    End Function

    '=============================================================== Virtual Memory
    Public Sub InjectCode(Addrs() As UInt32, Datas() As UInt32)
        Dim I As Integer
        For I = 0 To Addrs.Count - 1
            vRAM(Addrs(I) \ 4) = Datas(I)
        Next
    End Sub
    Public Sub UploadDump(Dump() As Byte)
        Dim i As Integer, i2 As Integer, d As UInt32
        i2 = 0
        For i = 0 To Dump.Count - 5
            d = (Convert.ToUInt32(Dump(i + 3)) * &H1000000)
            d += (Convert.ToUInt32(Dump(i + 2)) * &H10000)
            d += (Convert.ToUInt32(Dump(i + 1)) * &H100)
            d += Convert.ToUInt32(Dump(i))

            vRAM(i2) = d

            i += 3
            i2 += 1
        Next
    End Sub
    '=============================================================== Execution
    Public Function Run1() As Integer
        Dim ind As Int32, rt As Integer, i As Int32
        Dim doJump As Byte

        PerformanceCounter = 0
        If SetIndex() < 0 Then Return EXCEPTION_TLB_AddressLoad_InstFetch
        Do
            doJump = isBranching
            Do

                ind = -1 'PSMemory(GlobalSpace).IC(GlobalIndex)
                Select Case ind
                    Case -5
                        Return User_Break
                    Case -1

                        ind = mpAsm.FetchInstr(asmStruct.u32)
                        If ind < 0 Then Return UnknownInstruction
                        If asmOp(ind).ArgCount > 0 Then mpAsm.FetchRegs(asmStruct.u32, ind, rc)

                        'PSMemory(GlobalSpace).IC(GlobalIndex) = ind
                        'For i = 0 To 2
                        '   PSMemory(GlobalSpace).RC(GlobalIndex).r(i) = rc(i)
                        'Next

                        asmStruct.u32 = vRAM(GlobalIndex) 'PSMemory(GlobalSpace).W(GlobalIndex)
                        If ind <> 120 Then rt = asmOp(ind).Exec()
                        If rt < 0 Then Return rt
                    Case 120
                        ' Do nothing it's a NOP
                    Case Else
                        'For i = 0 To 2
                        '    rc(i) = PSMemory(GlobalSpace).RC(GlobalIndex).r(i)
                        'Next

                        asmStruct.u32 = vRAM(GlobalIndex) 'PSMemory(GlobalSpace).W(GlobalIndex)
                        rt = asmOp(ind).Exec()
                        If rt < 0 Then Return rt
                End Select

                GlobalIndex += 1
                PerformanceCounter += 1
            Loop Until isBranching > 0 Or GlobalIndex > GlobalMaxIndex

            Select Case doJump
                Case 0 'No branching yet
                    'Not ready yet
                Case 1 'Branch
                    GlobalIndex = GlobalNewIndex
                    isBranching = 0
                    If GlobalIndex < 0 Then Return EXCEPTION_TLB_AddressLoad_InstFetch
                Case 2 'Jump
                    GlobalAddress = GlobalNewAddress
                    GlobalIndex = -1
                    If SetIndex() < 0 Then Return EXCEPTION_TLB_AddressLoad_InstFetch
                    isBranching = 0
            End Select
        Loop Until GlobalIndex > GlobalMaxIndex

        Return EXCEPTION_TLB_AddressLoad_InstFetch
    End Function

    Public Function Exec1() As Integer
        Dim ind As Int32, rt As Integer, i As Int32
        Dim doJump As Byte

        If SetIndex() < 0 Then Return EXCEPTION_TLB_AddressLoad_InstFetch

        doJump = isBranching
        If GlobalIndex > GlobalMaxIndex Then Return EXCEPTION_TLB_AddressLoad_InstFetch

        asmStruct.u32 = vRAM(GlobalIndex) 'PSMemory(GlobalSpace).W(GlobalIndex)

        ind = -1 'PSMemory(GlobalSpace).IC(GlobalIndex)
        Select Case ind
            Case -5
                Return User_Break
            Case -1

                ind = mpAsm.FetchInstr(asmStruct.u32)
                If ind < 0 Then Return UnknownInstruction
                If asmOp(ind).ArgCount > 0 Then mpAsm.FetchRegs(asmStruct.u32, ind, rc)

                'PSMemory(GlobalSpace).IC(GlobalIndex) = ind
                'For i = 0 To 2
                'PSMemory(GlobalSpace).RC(GlobalIndex).r(i) = rc(i)
                'Next

                rt = asmOp(ind).Exec()
                If rt < 0 Then Return rt
            Case 120
                ' Do nothing it's a NOP
            Case Else
                'For i = 0 To 2
                'rc(i) = PSMemory(GlobalSpace).RC(GlobalIndex).r(i)
                'Next

                rt = asmOp(ind).Exec()
                If rt < 0 Then Return rt
        End Select

        GlobalIndex += 1
        If doJump = 1 Then 'Branch
            GlobalIndex = GlobalNewIndex
            isBranching = 0
            If GlobalIndex < 0 Then Return EXCEPTION_TLB_AddressLoad_InstFetch
        ElseIf doJump = 2 Then 'Jump
            GlobalAddress = GlobalNewAddress
            GlobalIndex = -1
            If SetIndex() < 0 Then Return EXCEPTION_TLB_AddressLoad_InstFetch
            isBranching = 0
        End If

        Return 0
    End Function


    '=============================================================== Special
    Public Function DumpGPR(Index As Integer) As String
        Dim ret As String
        ret = ""
        ret += Right("00000000" + Hex(GPr(Index).s32_4), 8) + " "
        ret += Right("00000000" + Hex(GPr(Index).s32_3), 8) + " "
        ret += Right("00000000" + Hex(GPr(Index).s32_2), 8) + " "
        ret += Right("00000000" + Hex(GPr(Index).s32_1), 8)
        Return ret
    End Function
    Public Function DumpCPR(Index As Integer) As String
        Return Right("00000000" + Hex(CPr(Index)), 8)
    End Function
    Public Function DumpFPR(Index As Integer) As String
        Return Right("00000000" + Hex(FPr(Index).u32), 8)
    End Function
    Public Function DumpGPRs() As String
        Dim ret As String, I As Integer

        ret = ""
        For I = 0 To 31
            ret += Right("00000000" + Hex(GPr(I).s32_4), 8) + " "
            ret += Right("00000000" + Hex(GPr(I).s32_3), 8) + " "
            ret += Right("00000000" + Hex(GPr(I).s32_2), 8) + " "
            ret += Right("00000000" + Hex(GPr(I).s32_1), 8) + " "
            ret += GetEERegStr(I) + vbCrLf
        Next

        Return ret
    End Function
    Public Function DumpR5900_Table() As String
        Dim ret As String, i As Int32

        ret = "private sub InitOps()" + vbCrLf + vbCrLf

        For i = 0 To asmOp.Count - 1
            ret += "asmop(" + i.ToString + ").exec = function() as integer ' " + LCase(asmOp(i).Instruction) + vbCrLf +
                                                   "'" + LCase(asmOp(i).Instruction) + " " + asmOp(i).Syntax + vbCrLf +
                                                   "'" + asmOp(i).ArgCount.ToString + "-> " + Join(asmOp(i).ArgOrder, ", ") + vbCrLf +
                                                   "return notimplementedyet" + vbCrLf +
                                                   "end function '----------" + LCase(asmOp(i).Instruction) + vbCrLf
        Next
        ret += "end sub"

        Return ret
    End Function

    '=============================================================== Operations
    Private Sub InitOps()
        asmOp(120).Exec = Function() As Integer ' nop
                              'nop 
                              '0-> 
                              Return 0
                          End Function '----------nop
        '==================================================================== Branches
        asmOp(66).Exec = Function() As Integer ' j
                             'j target
                             '1-> target, 
                             isBranching = 1
                             GlobalNewIndex = rc(0) ' PatchMemIndex(rc(0), GlobalSpace)
                             Return 0
                         End Function '----------j
        asmOp(69).Exec = Function() As Integer ' jr
                             'jr rs
                             '1-> rs, 
                             isBranching = 2
                             GlobalNewAddress = GPr(rc(0)).s32_1
                             Return 0
                         End Function '----------jr
        asmOp(67).Exec = Function() As Integer ' jal
                             'jal target
                             '1-> target, 
                             isBranching = 1
                             GlobalNewIndex = rc(0) ' PatchMemIndex(rc(0), GlobalSpace)

                             GPr(31).s32_1 = (GlobalIndex + 2) * 4 ' (RevertMemAddress(GlobalIndex * 4, GlobalSpace) + 8)

                             Return 0
                         End Function '----------jal
        asmOp(68).Exec = Function() As Integer ' jalr
                             'jalr rs | rd, rs
                             '2-> rs, rd, 
                             isBranching = 2
                             GlobalNewAddress = GPr(rc(0)).s32_1

                             If rc(1) > 0 Then GPr(rc(1)).s32_1 = (GlobalIndex + 2) * 4 ' (RevertMemAddress(GlobalIndex * 4, GlobalSpace) + 8)

                             Return 0
                         End Function '----------jalr
        asmOp(17).Exec = Function() As Integer ' beq
                             'beq rs, rt, %i
                             '3-> rs, rt, %i, 
                             If GPr(rc(0)).s32_1 = GPr(rc(1)).s32_1 And GPr(rc(0)).s32_2 = GPr(rc(1)).s32_2 Then
                                 isBranching = 1
                                 GlobalNewIndex = GlobalIndex + asmStruct.sImmediate + 1
                             End If
                             Return 0
                         End Function '----------beq
        asmOp(18).Exec = Function() As Integer ' beql
                             'beql rs, rt, %i
                             '3-> rs, rt, %i, 
                             If GPr(rc(0)).s32_1 = GPr(rc(1)).s32_1 And GPr(rc(0)).s32_2 = GPr(rc(1)).s32_2 Then
                                 isBranching = 1
                                 GlobalNewIndex = GlobalIndex + asmStruct.sImmediate + 1
                             Else
                                 GlobalIndex += 1
                             End If
                             Return 0
                         End Function '----------beql
        asmOp(19).Exec = Function() As Integer ' bgez
                             'bgez rs, %i
                             '2-> rs, %i, 
                             If GPr(rc(0)).s32_1 >= 0 And GPr(rc(0)).s32_2 >= 0 Then
                                 isBranching = 1
                                 GlobalNewIndex = GlobalIndex + asmStruct.sImmediate + 1
                             End If
                             Return 0
                         End Function '----------bgez
        asmOp(22).Exec = Function() As Integer ' bgezl
                             'bgezl rs, %i
                             '2-> rs, %i, 
                             If GPr(rc(0)).s32_1 >= 0 And GPr(rc(0)).s32_2 >= 0 Then
                                 isBranching = 1
                                 GlobalNewIndex = GlobalIndex + asmStruct.sImmediate + 1
                             Else
                                 GlobalIndex += 1
                             End If
                             Return 0
                         End Function '----------bgezl
        asmOp(20).Exec = Function() As Integer ' bgezal
                             'bgezal rs, %i
                             '2-> rs, %i, 
                             If GPr(rc(0)).s32_1 >= 0 And GPr(rc(0)).s32_2 >= 0 Then
                                 isBranching = 1
                                 GlobalNewIndex = GlobalIndex + asmStruct.sImmediate + 1
                                 GPr(31).s32_1 = (GlobalIndex + 2) * 4 'RevertMemAddress(GlobalIndex * 4, GlobalSpace) + 8
                             End If
                             Return 0
                         End Function '----------bgezal
        asmOp(21).Exec = Function() As Integer ' bgezall
                             'bgezall rs, %i
                             '2-> rs, %i, 
                             If GPr(rc(0)).s32_1 >= 0 And GPr(rc(0)).s32_2 >= 0 Then
                                 isBranching = 1
                                 GlobalNewIndex = GlobalIndex + asmStruct.sImmediate + 1
                                 GPr(31).s32_1 = (GlobalIndex + 2) * 4 'RevertMemAddress(GlobalIndex * 4, GlobalSpace) + 8
                             Else
                                 GlobalIndex += 1
                             End If
                             Return 0
                         End Function '----------bgezall
        asmOp(23).Exec = Function() As Integer ' bgtz
                             'bgtz rs, %i
                             '2-> rs, %i, 
                             If GPr(rc(0)).s32_1 > 0 And GPr(rc(0)).s32_2 >= 0 Then
                                 isBranching = 1
                                 GlobalNewIndex = GlobalIndex + asmStruct.sImmediate + 1
                             End If
                             Return 0
                         End Function '----------bgtz
        asmOp(24).Exec = Function() As Integer ' bgtzl
                             'bgtzl rs, %i
                             '2-> rs, %i, 
                             If GPr(rc(0)).s32_1 > 0 And GPr(rc(0)).s32_2 >= 0 Then
                                 isBranching = 1
                                 GlobalNewIndex = GlobalIndex + asmStruct.sImmediate + 1
                             Else
                                 GlobalIndex += 1
                             End If
                             Return 0
                         End Function '----------bgtzl
        asmOp(25).Exec = Function() As Integer ' blez
                             'blez rs, %i
                             '2-> rs, %i, 
                             If GPr(rc(0)).s32_1 <= 0 And GPr(rc(0)).s32_2 <= 0 Then
                                 isBranching = 1
                                 GlobalNewIndex = GlobalIndex + asmStruct.sImmediate + 1
                             End If
                             Return 0
                         End Function '----------blez
        asmOp(26).Exec = Function() As Integer ' blezl
                             'blezl rs, %i
                             '2-> rs, %i, 
                             If GPr(rc(0)).s32_1 <= 0 And GPr(rc(0)).s32_2 <= 0 Then
                                 isBranching = 1
                                 GlobalNewIndex = GlobalIndex + asmStruct.sImmediate + 1
                             Else
                                 GlobalIndex += 1
                             End If
                             Return 0
                         End Function '----------blezl
        asmOp(27).Exec = Function() As Integer ' bltz
                             'bltz rs, %i
                             '2-> rs, %i, 
                             If GPr(rc(0)).s32_1 < 0 And GPr(rc(0)).s32_2 < 0 Then
                                 isBranching = 1
                                 GlobalNewIndex = GlobalIndex + asmStruct.sImmediate + 1
                             End If
                             Return 0
                         End Function '----------bltz
        asmOp(30).Exec = Function() As Integer ' bltzl
                             'bltzl rs, %i
                             '2-> rs, %i, 
                             If GPr(rc(0)).s32_1 < 0 And GPr(rc(0)).s32_2 < 0 Then
                                 isBranching = 1
                                 GlobalNewIndex = GlobalIndex + asmStruct.sImmediate + 1
                             Else
                                 GlobalIndex += 1
                             End If
                             Return 0
                         End Function '----------bltzl
        asmOp(28).Exec = Function() As Integer ' bltzal
                             'bltzal rs, %i
                             '2-> rs, %i, 
                             If GPr(rc(0)).s32_1 < 0 And GPr(rc(0)).s32_2 < 0 Then
                                 isBranching = 1
                                 GlobalNewIndex = GlobalIndex + asmStruct.sImmediate + 1
                                 GPr(31).s32_1 = (GlobalIndex + 2) * 4 'RevertMemAddress(GlobalIndex * 4, GlobalSpace) + 8
                             End If
                             Return 0
                         End Function '----------bltzal
        asmOp(29).Exec = Function() As Integer ' bltzall
                             'bltzall rs, %i
                             '2-> rs, %i, 
                             If GPr(rc(0)).s32_1 < 0 And GPr(rc(0)).s32_2 < 0 Then
                                 isBranching = 1
                                 GlobalNewIndex = GlobalIndex + asmStruct.sImmediate + 1
                                 GPr(31).s32_1 = (GlobalIndex + 2) * 4 'RevertMemAddress(GlobalIndex * 4, GlobalSpace) + 8
                             Else
                                 GlobalIndex += 1
                             End If
                             Return 0
                         End Function '----------bltzall
        asmOp(31).Exec = Function() As Integer ' bne
                             'bne rs, rt, %i
                             '3-> rs, rt, %i, 
                             If GPr(rc(0)).u32_1 <> GPr(rc(1)).u32_1 Or GPr(rc(0)).u32_2 <> GPr(rc(1)).u32_2 Then
                                 isBranching = 1
                                 GlobalNewIndex = GlobalIndex + asmStruct.sImmediate + 1
                             End If
                             Return 0
                         End Function '----------bne
        asmOp(32).Exec = Function() As Integer ' bnel
                             'bnel rs, rt, %i
                             '3-> rs, rt, %i, 
                             If GPr(rc(0)).u32_1 <> GPr(rc(1)).u32_1 Or GPr(rc(0)).u32_2 <> GPr(rc(1)).u32_2 Then
                                 isBranching = 1
                                 GlobalNewIndex = GlobalIndex + asmStruct.sImmediate + 1
                             Else
                                 GlobalIndex += 1
                             End If
                             Return 0
                         End Function '----------bnel

        '==================================================================== Basic Math
        asmOp(1).Exec = Function() As Integer ' add
                            'add rd, rs, rt
                            '3-> rs, rt, rd, 
                            If rc(2) = 0 Then Return 0
                            Dim tmp As Int64

                            tmp = (GPr(rc(0)).s32_1 + GPr(rc(1)).s32_1)
                            If tmp > &H7FFFFFFF Or tmp < &H80000000 Then Return EXCEPTION_TLB_Overflow

                            GPr(rc(2)).s32_1 = (GPr(rc(0)).s32_1 + GPr(rc(1)).s32_1)

                            GPr(rc(2)).s32_2 = 0
                            If GPr(rc(2)).s32_1 < 0 Then GPr(rc(2)).s32_2 = -1

                            Return 0
                        End Function '----------add
        asmOp(6).Exec = Function() As Integer ' addu
                            'addu rd, rs, rt
                            '3-> rs, rt, rd, 
                            If rc(2) = 0 Then Return 0
                            GPr(rc(2)).s64_1 = GPr(rc(0)).u32_1 + GPr(rc(1)).u32_1
                            GPr(rc(2)).s32_2 = 0
                            If GPr(rc(2)).s32_1 < 0 Then GPr(rc(2)).s32_2 = -1
                            Return 0
                        End Function '----------addu
        asmOp(234).Exec = Function() As Integer ' sub
                              'sub rd, rs, rt
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0
                              Dim tmp As Int64

                              tmp = (GPr(rc(0)).s32_1 - GPr(rc(1)).s32_1)
                              If tmp > &H7FFFFFFF Or tmp < &H80000000 Then Return EXCEPTION_TLB_Overflow

                              GPr(rc(2)).s32_1 = (GPr(rc(0)).s32_1 - GPr(rc(1)).s32_1)

                              GPr(rc(2)).s32_2 = 0
                              If GPr(rc(2)).s32_1 < 0 Then GPr(rc(2)).s32_2 = -1

                              Return 0
                          End Function '----------sub
        asmOp(237).Exec = Function() As Integer ' subu
                              'subu rd, rs, rt
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0
                              GPr(rc(2)).s64_1 = GPr(rc(0)).s64_1 - GPr(rc(1)).s64_1
                              GPr(rc(2)).s32_2 = 0
                              If GPr(rc(2)).s32_1 < 0 Then GPr(rc(2)).s32_2 = -1
                              Return 0
                          End Function '----------subu
        asmOp(4).Exec = Function() As Integer ' addi
                            'addi rt, rs, %i
                            '3-> rs, rt, %i, 
                            If rc(1) = 0 Then Return 0
                            Dim tmp As Int64

                            tmp = (GPr(rc(0)).s32_1 + asmStruct.sImmediate)
                            If tmp > &H7FFFFFFF Or tmp < &H80000000 Then Return EXCEPTION_TLB_Overflow

                            GPr(rc(1)).s32_1 = GPr(rc(0)).s32_1 + asmStruct.sImmediate

                            GPr(rc(1)).s32_2 = 0
                            If GPr(rc(1)).s32_1 < 0 Then GPr(rc(1)).s32_2 = -1

                            Return 0
                        End Function '----------addi
        asmOp(5).Exec = Function() As Integer ' addiu
                            'addiu rt, rs, %i
                            '3-> rs, rt, %i, 
                            If rc(1) = 0 Then Return 0

                            GPr(rc(1)).s64_1 = GPr(rc(0)).u32_1 + asmStruct.sImmediate

                            GPr(rc(1)).s32_2 = 0
                            If GPr(rc(1)).s32_1 < 0 Then GPr(rc(1)).s32_2 = -1

                            Return 0
                        End Function '----------addiu
        asmOp(43).Exec = Function() As Integer ' dadd
                             'dadd rd, rs, rt
                             '3-> rs, rt, rd, 
                             If rc(2) = 0 Then Return 0

                             GPr(32).s64_1 = GPr(rc(0)).u32_1 + GPr(rc(1)).u32_1
                             GPr(32).s64_2 = GPr(rc(0)).s32_2 + GPr(rc(1)).s32_2 + GPr(32).Overflow_1

                             If GPr(32).s64_2 > &H7FFFFFFF Or GPr(32).s64_2 < &H80000000 Then Return EXCEPTION_TLB_Overflow

                             GPr(rc(2)).u32_1 = GPr(32).u32_1
                             GPr(rc(2)).s32_2 = GPr(32).s32_2

                             Return 0
                         End Function '----------dadd
        asmOp(46).Exec = Function() As Integer ' daddu
                             'daddu rd, rs, rt
                             '3-> rs, rt, rd, 
                             If rc(2) = 0 Then Return 0

                             GPr(rc(2)).s64_1 = GPr(rc(0)).u32_1 + GPr(rc(1)).u32_1
                             GPr(rc(2)).s64_2 = GPr(rc(0)).s32_2 + GPr(rc(1)).s32_2 + GPr(rc(2)).Overflow_1

                             Return 0
                         End Function '----------daddu
        asmOp(62).Exec = Function() As Integer ' dsub
                             'dsub rd, rs, rt
                             '3-> rs, rt, rd, 
                             If rc(2) = 0 Then Return 0

                             GPr(32).s64_1 = GPr(rc(0)).u32_1 - GPr(rc(1)).u32_1
                             GPr(32).s64_2 = GPr(rc(0)).u32_2 - GPr(rc(1)).u32_2
                             GPr(32).s64_2 += GPr(rc(2)).Overflow_1

                             If GPr(32).s64_2 > &H7FFFFFFF Or GPr(32).s64_2 < &H80000000 Then Return EXCEPTION_TLB_Overflow

                             GPr(rc(2)).s32_1 = GPr(32).s32_1
                             GPr(rc(2)).s32_2 = GPr(32).s32_2

                             Return 0
                         End Function '----------dsub
        asmOp(63).Exec = Function() As Integer ' dsubu
                             'dsubu rd, rs, rt
                             '3-> rs, rt, rd, 
                             If rc(2) = 0 Then Return 0

                             GPr(rc(2)).s64_1 = GPr(rc(0)).u32_1 - GPr(rc(1)).u32_1
                             GPr(rc(2)).s64_2 = GPr(rc(0)).u32_2 - GPr(rc(1)).u32_2
                             GPr(rc(2)).s64_2 += GPr(rc(2)).Overflow_1

                             Return 0
                         End Function '----------dsubu
        asmOp(44).Exec = Function() As Integer ' daddi
                             'daddi rt, rs, %i
                             '3-> rs, rt, %i, 
                             If rc(1) = 0 Then Return 0

                             GPr(32).s64_1 = GPr(rc(0)).u32_1 + asmStruct.sImmediate
                             GPr(32).s64_2 = GPr(rc(0)).s32_2 + GPr(rc(1)).Overflow_1

                             If GPr(32).s64_2 > &H7FFFFFFF Or GPr(32).s64_2 < &H80000000 Then Return EXCEPTION_TLB_Overflow

                             GPr(rc(2)).s32_1 = GPr(32).s32_1
                             GPr(rc(2)).s32_2 = GPr(32).s32_2
                             Return 0
                         End Function '----------daddi
        asmOp(45).Exec = Function() As Integer ' daddiu
                             'daddiu rt, rs, %i
                             '3-> rs, rt, %i, 
                             If rc(1) = 0 Then Return 0

                             GPr(rc(1)).s64_1 = GPr(rc(0)).u32_1 + asmStruct.sImmediate
                             GPr(rc(1)).s64_2 = GPr(rc(0)).s32_2 + GPr(rc(1)).Overflow_1

                             Return 0
                         End Function '----------daddiu

        '==================================================================== Hard Math

        asmOp(48).Exec = Function() As Integer ' div
                             'div rs, rt
                             '2-> rs, rt, 
                             Return NotImplementedYet
                         End Function '----------div
        asmOp(50).Exec = Function() As Integer ' div1
                             'div1 rs, rt
                             '2-> rs, rt, 
                             Return NotImplementedYet
                         End Function '----------div1
        asmOp(51).Exec = Function() As Integer ' divu
                             'divu rs, rt
                             '2-> rs, rt, 
                             Return NotImplementedYet
                         End Function '----------divu
        asmOp(52).Exec = Function() As Integer ' divu1
                             'divu1 rs, rt
                             '2-> rs, rt, 
                             Return NotImplementedYet
                         End Function '----------divu1
        asmOp(84).Exec = Function() As Integer ' madd
                             'madd rs, rt | rd, rs, rt
                             '3-> rs, rt, rd, 
                             Return NotImplementedYet
                         End Function '----------madd
        asmOp(86).Exec = Function() As Integer ' madd1
                             'madd1 rs, rt | rd, rs, rt
                             '3-> rs, rt, rd, 
                             Return NotImplementedYet
                         End Function '----------madd1
        asmOp(88).Exec = Function() As Integer ' maddu
                             'maddu rs, rt | rd, rs, rt
                             '3-> rs, rt, rd, 
                             Return NotImplementedYet
                         End Function '----------maddu
        asmOp(89).Exec = Function() As Integer ' maddu1
                             'maddu1 rs, rt | rd, rs, rt
                             '3-> rs, rt, rd, 
                             Return NotImplementedYet
                         End Function '----------maddu1
        asmOp(115).Exec = Function() As Integer ' mult
                              'mult rs, rt | rd, rs, rt
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------mult
        asmOp(116).Exec = Function() As Integer ' mult1
                              'mult1 rs, rt | rd, rs, rt
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------mult1
        asmOp(117).Exec = Function() As Integer ' multu
                              'multu rs, rt | rd, rs, rt
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------multu
        asmOp(118).Exec = Function() As Integer ' multu1
                              'multu1 rs, rt | rd, rs, rt
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------multu1

        '==================================================================== Bitwise
        asmOp(7).Exec = Function() As Integer ' and
                            'and rd, rs, rt
                            '3-> rs, rt, rd, 
                            If rc(2) = 0 Then Return 0
                            GPr(rc(2)).u32_1 = GPr(rc(0)).u32_1 And GPr(rc(1)).u32_1
                            GPr(rc(2)).u32_2 = GPr(rc(0)).u32_2 And GPr(rc(1)).u32_2
                            Return 0
                        End Function '----------and
        asmOp(8).Exec = Function() As Integer ' andi
                            'andi rt, rs, %i
                            '3-> rs, rt, %i, 
                            If rc(1) = 0 Then Return 0
                            GPr(rc(1)).u32_1 = GPr(rc(0)).u32_1 And asmStruct.uImmediate
                            GPr(rc(1)).u32_2 = 0
                            Return 0
                        End Function '----------andi
        asmOp(122).Exec = Function() As Integer ' or
                              'or rd, rs, rt
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0

                              GPr(rc(2)).u32_1 = GPr(rc(0)).u32_1 Or GPr(rc(1)).u32_1
                              GPr(rc(2)).u32_2 = GPr(rc(0)).u32_2 Or GPr(rc(1)).u32_2

                              Return 0
                          End Function '----------or
        asmOp(123).Exec = Function() As Integer ' ori
                              'ori rt, rs, %i
                              '3-> rs, rt, %i, 
                              If rc(1) = 0 Then Return 0

                              GPr(rc(1)).u32_1 = GPr(rc(0)).u32_1 Or asmStruct.uImmediate

                              Return 0
                          End Function '----------ori
        asmOp(121).Exec = Function() As Integer ' nor
                              'nor rd, rs, rt
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0

                              GPr(rc(2)).u32_1 = Not GPr(rc(0)).u32_1 Or GPr(rc(1)).u32_1
                              GPr(rc(2)).u32_2 = Not GPr(rc(0)).u32_2 Or GPr(rc(1)).u32_2

                              Return 0
                          End Function '----------nor
        asmOp(257).Exec = Function() As Integer ' xor
                              'xor rd, rs, rt
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0

                              GPr(rc(2)).u32_1 = GPr(rc(0)).u32_1 Xor GPr(rc(1)).u32_1
                              GPr(rc(2)).u32_2 = GPr(rc(0)).u32_2 Xor GPr(rc(1)).u32_2

                              Return 0
                          End Function '----------xor
        asmOp(258).Exec = Function() As Integer ' xori
                              'xori rt, rs, %i
                              '3-> rs, rt, %i, 
                              If rc(1) = 0 Then Return 0

                              GPr(rc(1)).u32_1 = GPr(rc(0)).u32_1 Xor asmStruct.uImmediate
                              GPr(rc(1)).u32_2 = GPr(rc(0)).u32_2 Xor 0
                              Return 0
                          End Function '----------xori
        asmOp(224).Exec = Function() As Integer ' slt
                              'slt rd, rs, rt
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0
                              GPr(rc(2)).u32_1 = (GPr(rc(0)).s32_1 < GPr(rc(1)).s32_1) And 1
                              Return 0
                          End Function '----------slt
        asmOp(227).Exec = Function() As Integer ' sltu
                              'sltu rd, rs, rt
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0
                              GPr(rc(2)).u32_1 = (GPr(rc(0)).u32_1 < GPr(rc(1)).u32_1) And 1
                              Return 0
                          End Function '----------sltu
        asmOp(225).Exec = Function() As Integer ' slti
                              'slti rt, rs, %i
                              '3-> rs, rt, %i, 
                              If rc(1) = 0 Then Return 0
                              GPr(rc(1)).u32_1 = (GPr(rc(0)).s32_1 < asmStruct.sImmediate) And 1
                              Return 0
                          End Function '----------slti
        asmOp(226).Exec = Function() As Integer ' sltiu
                              'sltiu rt, rs, %i
                              '3-> rs, rt, %i, 
                              If rc(1) = 0 Then Return 0
                              GPr(rc(1)).u32_1 = (GPr(rc(0)).u32_1 < asmStruct.uImmediate) And 1
                              Return 0
                          End Function '----------sltiu

        '==================================================================== Shifts
        asmOp(222).Exec = Function() As Integer ' sll
                              'sll rd, rt, sa
                              '3-> rt, rd, sa, 
                              If rc(1) = 0 Then Return 0

                              GPr(rc(1)).u64_1 = GPr(rc(0)).u32_1 * (2 ^ rc(2))
                              GPr(rc(1)).u64_2 = 0

                              Return 0
                          End Function '----------sll
        asmOp(223).Exec = Function() As Integer ' sllv
                              'sllv rd, rt, rs
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0

                              GPr(rc(2)).u64_1 = GPr(rc(1)).u32_1 * (2 ^ GPr(rc(0)).u32_1)
                              GPr(rc(2)).u64_2 = 0

                              Return 0
                          End Function '----------sllv
        asmOp(232).Exec = Function() As Integer ' srl
                              'srl rd, rt, sa
                              '3-> rt, rd, sa, 
                              If rc(1) = 0 Then Return 0

                              GPr(rc(1)).u64_1 = GPr(rc(0)).u32_1 \ (2 ^ rc(2))
                              GPr(rc(1)).u64_2 = 0

                              Return 0
                          End Function '----------srl
        asmOp(233).Exec = Function() As Integer ' srlv
                              'srlv rd, rt, rs
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0

                              GPr(rc(2)).u64_1 = GPr(rc(1)).u32_1 \ (2 ^ GPr(rc(0)).u32_1)
                              GPr(rc(2)).u64_2 = 0

                              Return 0
                          End Function '----------srlv
        asmOp(230).Exec = Function() As Integer ' sra
                              'sra rd, rt sa
                              '3-> rt, rd, sa, 
                              If rc(1) = 0 Then Return 0

                              GPr(rc(1)).s32_1 = GPr(rc(0)).s32_1 \ (2 ^ rc(2))
                              GPr(rc(1)).s32_2 = 0
                              If GPr(rc(1)).s32_1 < 0 Then GPr(rc(1)).s32_2 = -1
                              Return 0
                          End Function '----------sra
        asmOp(231).Exec = Function() As Integer ' srav
                              'srav rd, rt, rs
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0

                              GPr(rc(2)).s32_1 = GPr(rc(1)).s32_1 \ (2 ^ GPr(rc(0)).u32_1)
                              GPr(rc(1)).s32_2 = 0
                              If GPr(rc(1)).s32_1 < 0 Then GPr(rc(1)).s32_2 = -1

                              Return 0
                          End Function '----------srav
        asmOp(53).Exec = Function() As Integer ' dsll
                             'dsll rd, rt, sa
                             '3-> rt, rd, sa, 
                             If rc(1) = 0 Then Return 0

                             GPr(rc(1)).u64_1 = GPr(rc(0)).u32_1 * (2 ^ rc(2))
                             GPr(rc(1)).u64_2 = (GPr(rc(0)).u32_2 * (2 ^ rc(2))) + GPr(rc(1)).Overflow_1

                             Return 0
                         End Function '----------dsll
        asmOp(59).Exec = Function() As Integer ' dsrl
                             'dsrl rd, rt, sa
                             '3-> rt, rd, sa, 
                             If rc(1) = 0 Then Return 0
                             Dim tmp As Int64

                             GPr(rc(1)).u64_2 = Math.DivRem(GPr(rc(0)).u32_2, CLng(2 ^ rc(2)), tmp)
                             GPr(rc(1)).u64_1 = ((GPr(rc(0)).u32_1 + (tmp * &H100000000)) \ (2 ^ rc(2)))

                             Return 0
                         End Function '----------dsrl
        asmOp(54).Exec = Function() As Integer ' dsll32
                             'dsll32 rd, rt, sa
                             '3-> rt, rd, sa, 
                             If rc(1) = 0 Then Return 0
                             GPr(rc(1)).u64_2 = GPr(rc(0)).u32_1 * (2 ^ rc(2))
                             GPr(rc(1)).u32_1 = 0
                             Return 0
                         End Function '----------dsll32
        asmOp(60).Exec = Function() As Integer ' dsrl32
                             'dsrl32 rd, rt, sa
                             '3-> rt, rd, sa, 
                             If rc(1) = 0 Then Return 0
                             GPr(rc(1)).u64_1 = GPr(rc(0)).u32_2 \ (2 ^ rc(2))
                             GPr(rc(1)).u32_2 = 0
                             Return 0
                         End Function '----------dsrl32
        asmOp(55).Exec = Function() As Integer ' dsllv
                             'dsllv rd, rt, rs
                             '3-> rs, rt, rd, 
                             If rc(2) = 0 Then Return 0

                             GPr(rc(2)).u64_1 = GPr(rc(1)).u32_1 * (2 ^ GPr(rc(0)).u32_1)
                             GPr(rc(2)).u64_2 = (GPr(rc(1)).u32_2 * (2 ^ GPr(rc(0)).u32_1)) + GPr(rc(2)).Overflow_1

                             Return 0
                         End Function '----------dsllv
        asmOp(61).Exec = Function() As Integer ' dsrlv
                             'dsrlv rd, rt, rs
                             '3-> rs, rt, rd, 
                             If rc(2) = 0 Then Return 0
                             Dim tmp As Int64

                             GPr(rc(2)).u64_2 = Math.DivRem(GPr(rc(1)).u32_2, CLng(2 ^ GPr(rc(0)).u32_1), tmp)
                             GPr(rc(2)).u64_1 = ((GPr(rc(1)).u32_1 + (tmp * &H100000000)) \ (2 ^ GPr(rc(0)).u32_1))

                             Return 0
                         End Function '----------dsrlv
        asmOp(56).Exec = Function() As Integer ' dsra
                             'dsra rd, rt, sa
                             '3-> rt, rd, sa, 
                             If rc(1) = 0 Then Return 0
                             Dim tmp As Int64

                             GPr(rc(1)).s64_2 = Math.DivRem(GPr(rc(0)).s32_2, CLng(2 ^ rc(2)), tmp)
                             GPr(rc(1)).s64_1 = ((GPr(rc(0)).u32_1 + (tmp * &H100000000)) \ (2 ^ rc(2)))

                             Return 0
                         End Function '----------dsra
        asmOp(58).Exec = Function() As Integer ' dsrav
                             'dsrav rd, rt, rs
                             '3-> rs, rt, rd, 
                             If rc(2) = 0 Then Return 0
                             Dim tmp As Int64

                             GPr(rc(2)).s64_2 = Math.DivRem(GPr(rc(1)).s32_2, CLng(2 ^ GPr(rc(0)).u32_1), tmp)
                             GPr(rc(2)).s64_1 = ((GPr(rc(1)).u32_1 + (tmp * &H100000000)) \ (2 ^ GPr(rc(0)).u32_1))

                             Return 0
                         End Function '----------dsrav
        asmOp(57).Exec = Function() As Integer ' dsra32
                             'dsra32 rd, rt, sa
                             '3-> rt, rd, sa, 
                             If rc(1) = 0 Then Return 0
                             GPr(rc(1)).u64_1 = GPr(rc(0)).s32_2 \ (2 ^ rc(2))
                             GPr(rc(1)).u32_2 = 0
                             Return 0
                         End Function '----------dsra32
        asmOp(215).Exec = Function() As Integer ' qfsrv
                              'qfsrv rd, rs, rt
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0

                              Dim tmp As Int64, tmp2 As Int64

                              '128 bit shift >> sa
                              GPr(32).u64_4 = Math.DivRem(GPr(rc(0)).u32_4, CLng(2 ^ HIloSA(2).u32_1), tmp)
                              tmp *= &H100000000 \ CLng(2 ^ HIloSA(2).u32_1)
                              GPr(32).u64_3 = Math.DivRem(GPr(rc(0)).u32_3, CLng(2 ^ HIloSA(2).u32_1), tmp2) + tmp
                              tmp2 *= &H100000000 \ CLng(2 ^ HIloSA(2).u32_1)
                              GPr(32).u64_2 = Math.DivRem(GPr(rc(0)).u32_2, CLng(2 ^ HIloSA(2).u32_1), tmp) + tmp2
                              tmp *= &H100000000 \ CLng(2 ^ HIloSA(2).u32_1)
                              GPr(32).u64_1 = Math.DivRem(GPr(rc(0)).u32_1, CLng(2 ^ HIloSA(2).u32_1), tmp2) + tmp
                              tmp2 *= &H100000000 \ CLng(2 ^ HIloSA(2).u32_1)

                              GPr(rc(2)).u64_4 = Math.DivRem(GPr(rc(1)).u32_4, CLng(2 ^ HIloSA(2).u32_1), tmp) + tmp2
                              tmp *= &H100000000 \ CLng(2 ^ HIloSA(2).u32_1)
                              GPr(rc(2)).u64_3 = Math.DivRem(GPr(rc(1)).u32_3, CLng(2 ^ HIloSA(2).u32_1), tmp2) + tmp
                              tmp2 *= &H100000000 \ CLng(2 ^ HIloSA(2).u32_1)
                              GPr(rc(2)).u64_2 = Math.DivRem(GPr(rc(1)).u32_2, CLng(2 ^ HIloSA(2).u32_1), tmp) + tmp2
                              tmp *= &H100000000 \ CLng(2 ^ HIloSA(2).u32_1)
                              GPr(rc(2)).u64_1 = (GPr(rc(1)).u32_1 \ CLng(2 ^ HIloSA(2).u32_1)) + tmp 'Math.DivRem(GPr(rc(1)).u32_1 + (tmp * &H100000000), CLng(2 ^ HIloSA(2).u32_1), tmp)

                              'GPr(rc(2)).u64_2 = Math.DivRem(GPr(rc(0)).u32_2 + (tmp * &H100000000), CLng(2 ^ HIloSA(2).u32_1), tmp)
                              'GPr(rc(2)).u64_1 = ((GPr(rc(0)).u32_1 + (tmp * &H100000000)) \ (2 ^ HIloSA(2).u32_1))



                              Return 0
                          End Function '----------qfsrv

        '==================================================================== Data Move
        asmOp(39).Exec = Function() As Integer ' cfc1
                             'cfc1 rt, fs
                             '2-> rt, fs, 
                             If rc(1) = 0 Or rc(1) = 31 Then
                                 GPr(rc(0)).u32_1 = FCr(rc(1)).u32
                                 GPr(rc(0)).u32_2 = 0
                             End If
                             Return 0
                         End Function '----------cfc1
        asmOp(40).Exec = Function() As Integer ' ctc1
                             'ctc1 rt, fs
                             '2-> rt, fs, 
                             If rc(1) = 0 Or rc(1) = 31 Then
                                 FCr(rc(1)).u32 = GPr(rc(0)).u32_1
                             End If
                             Return 0
                         End Function '----------ctc1
        asmOp(91).Exec = Function() As Integer ' mfc0
                             'mfc0 rt, reg | rt, reg, sel
                             '3-> rt, reg, sel, 
                             If rc(0) = 0 Then Return 0
                             GPr(rc(0)).u32_1 = CPr(rc(1))
                             GPr(rc(0)).u32_2 = 0
                             Return 0
                         End Function '----------mfc0
        asmOp(104).Exec = Function() As Integer ' mtc0
                              'mtc0 rt, reg | rt, reg, sel
                              '3-> rt, reg, sel, 
                              CPr(rc(1)) = GPr(rc(0)).u32_1
                              Return 0
                          End Function '----------mtc0
        asmOp(105).Exec = Function() As Integer ' mtc1
                              'mtc1 rt, fs
                              '2-> rt, fs, 
                              FPr(rc(1)).u32 = GPr(rc(0)).u32_1
                              Return 0
                          End Function '----------mtc1
        asmOp(92).Exec = Function() As Integer ' mfc1
                             'mfc1 rt, fs
                             '2-> rt, fs, 
                             If rc(0) = 0 Then Return 0
                             GPr(rc(0)).u32_1 = FPr(rc(1)).u32
                             GPr(rc(0)).u32_2 = 0
                             Return 0
                         End Function '----------mfc1
        asmOp(108).Exec = Function() As Integer ' mtlo
                              'mtlo rs
                              '1-> rs, 
                              HIloSA(0).u32_1 = GPr(rc(0)).u32_1
                              HIloSA(0).u32_2 = GPr(rc(0)).u32_2
                              Return 0
                          End Function '----------mtlo
        asmOp(95).Exec = Function() As Integer ' mflo
                             'mflo rd
                             '1-> rd, 
                             If rc(0) = 0 Then Return 0
                             GPr(rc(0)).u32_1 = HIloSA(0).u32_1
                             GPr(rc(0)).u32_2 = HIloSA(0).u32_2
                             Return 0
                         End Function '----------mflo
        asmOp(109).Exec = Function() As Integer ' mtlo1
                              'mtlo1 rs
                              '1-> rs, 
                              HIloSA(0).u32_3 = GPr(rc(0)).u32_1
                              HIloSA(0).u32_4 = GPr(rc(0)).u32_2
                              Return 0
                          End Function '----------mtlo1
        asmOp(96).Exec = Function() As Integer ' mflo1
                             'mflo1 rd
                             '1-> rd, 
                             If rc(0) = 0 Then Return 0
                             GPr(rc(0)).u32_1 = HIloSA(0).u32_3
                             GPr(rc(0)).u32_2 = HIloSA(0).u32_4
                             Return 0
                         End Function '----------mflo1
        asmOp(106).Exec = Function() As Integer ' mthi
                              'mthi rs
                              '1-> rs, 
                              HIloSA(1).u32_1 = GPr(rc(0)).u32_1
                              HIloSA(1).u32_2 = GPr(rc(0)).u32_2
                              Return 0
                          End Function '----------mthi
        asmOp(93).Exec = Function() As Integer ' mfhi
                             'mfhi rd
                             '1-> rd, 
                             If rc(0) = 0 Then Return 0
                             GPr(rc(0)).u32_1 = HIloSA(1).u32_1
                             GPr(rc(0)).u32_2 = HIloSA(1).u32_2
                             Return 0
                         End Function '----------mfhi
        asmOp(107).Exec = Function() As Integer ' mthi1
                              'mthi1 rs
                              '1-> rs, 
                              HIloSA(1).u32_3 = GPr(rc(0)).u32_1
                              HIloSA(1).u32_4 = GPr(rc(0)).u32_2
                              Return 0
                          End Function '----------mthi1
        asmOp(94).Exec = Function() As Integer ' mfhi1
                             'mfhi1 rd
                             '1-> rd, 
                             If rc(0) = 0 Then Return 0
                             GPr(rc(0)).u32_1 = HIloSA(1).u32_3
                             GPr(rc(0)).u32_2 = HIloSA(1).u32_4
                             Return 0
                         End Function '----------mfhi1
        asmOp(110).Exec = Function() As Integer ' mtsa
                              'mtsa rs
                              '1-> rs,
                              HIloSA(2).u32_1 = GPr(rc(0)).u32_1
                              HIloSA(2).u32_2 = GPr(rc(0)).u32_2
                              Return 0
                          End Function '----------mtsa
        asmOp(97).Exec = Function() As Integer ' mfsa
                             'mfsa rd
                             '1-> rd,
                             If rc(0) = 0 Then Return 0
                             GPr(rc(0)).u32_1 = HIloSA(2).u32_1
                             GPr(rc(0)).u32_2 = HIloSA(2).u32_2
                             Return 0
                         End Function '----------mfsa
        asmOp(111).Exec = Function() As Integer ' mtsab
                              'mtsab rs, %i
                              '2-> rs, %i, 
                              HIloSA(2).u32_1 = ((GPr(rc(0)).u32_1 And 15) Xor (rc(1) And 15)) * 8
                              Return 0
                          End Function '----------mtsab
        asmOp(112).Exec = Function() As Integer ' mtsah
                              'mtsah rs, %i
                              '2-> rs, %i, 
                              HIloSA(2).u32_1 = ((GPr(rc(0)).u32_1 And 15) Xor (rc(1) And 15)) * 16
                              Return 0
                          End Function '----------mtsah
        asmOp(100).Exec = Function() As Integer ' movn
                              'movn rd, rs, rt
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0
                              If GPr(rc(1)).u32_1 <> 0 Or GPr(rc(1)).u32_2 <> 0 Then
                                  GPr(rc(2)).u32_1 = GPr(rc(0)).u32_1
                                  GPr(rc(2)).u32_2 = GPr(rc(0)).u32_2
                              End If
                              Return 0
                          End Function '----------movn
        asmOp(101).Exec = Function() As Integer ' movz
                              'movz rd, rs, rt
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0
                              If GPr(rc(1)).u32_1 = 0 And GPr(rc(1)).u32_2 = 0 Then
                                  GPr(rc(2)).u32_1 = GPr(rc(0)).u32_1
                                  GPr(rc(2)).u32_2 = GPr(rc(0)).u32_2
                              End If
                              Return 0
                          End Function '----------movz

        '==================================================================== Load/Store
        asmOp(78).Exec = Function() As Integer ' lui
                             'lui rt, %i
                             '2-> rt, %i, 
                             If rc(0) = 0 Then Return 0

                             GPr(rc(0)).s32_1 = asmStruct.sImmediate * &H10000
                             GPr(rc(0)).s32_2 = 0
                             If GPr(rc(0)).s32_1 < 0 Then GPr(rc(0)).s32_2 = -1
                             Return 0
                         End Function '----------lui
        asmOp(70).Exec = Function() As Integer ' lb
                             'lb rt, %i(base)
                             '3-> base, rt, %i, 
                             Dim tAddr As Int32, tRS As Int16

                             LoadAddr = GPr(rc(0)).s32_1 + asmStruct.sImmediate
                             If LoadAddr < -2146959360 Then LoadAddr += 2147483648

                             tAddr = LoadAddr 'PatchMemAddress(LoadAddr, tRS)
                             If tAddr < 0 Or tRS < 0 Then Return EXCEPTION_TLB_Load_InstFetch

                             MemData32.u32 = vRAM(tAddr \ 4) 'PSMemory(tRS).W(tAddr \ 4)

                             If rc(1) = 0 Then Return 0
                             Select Case (LoadAddr And 3)
                                 Case 0
                                     GPr(rc(1)).s32_1 = MemData32.s8_1
                                 Case 1
                                     GPr(rc(1)).s32_1 = MemData32.s8_2
                                 Case 2
                                     GPr(rc(1)).s32_1 = MemData32.s8_3
                                 Case 3
                                     GPr(rc(1)).s32_1 = MemData32.s8_4
                             End Select
                             GPr(rc(1)).s32_2 = 0
                             If GPr(rc(1)).s32_1 < 0 Then GPr(rc(1)).s32_2 = -1
                             Return 0
                         End Function '----------lb
        asmOp(75).Exec = Function() As Integer ' lh
                             'lh rt, %i(base)
                             '3-> base, rt, %i, 
                             Dim tAddr As Int32, tRS As Int16

                             LoadAddr = GPr(rc(0)).s32_1 + asmStruct.sImmediate
                             If LoadAddr < -2146959360 Then LoadAddr += 2147483648

                             tAddr = LoadAddr ' PatchMemAddress(LoadAddr, tRS)
                             If tAddr < 0 Or tRS < 0 Then Return EXCEPTION_TLB_Load_InstFetch

                             MemData32.u32 = vRAM(tAddr \ 4) 'PSMemory(tRS).W(tAddr \ 4)

                             If rc(1) = 0 Then Return 0
                             Select Case (LoadAddr And 3)
                                 Case 0
                                     GPr(rc(1)).s32_1 = MemData32.s16_1
                                 Case 2
                                     GPr(rc(1)).s32_1 = MemData32.s16_2
                                 Case Else
                                     Return EXCEPTION_TLB_Load_InstFetch
                             End Select
                             GPr(rc(1)).s32_2 = 0
                             If GPr(rc(1)).s32_1 < 0 Then GPr(rc(1)).s32_2 = -1
                             Return 0
                         End Function '----------lh
        asmOp(79).Exec = Function() As Integer ' lw
                             'lw rt, %i(base)
                             '3-> base, rt, %i, 
                             Dim tAddr As Int32, tRS As Int16

                             LoadAddr = GPr(rc(0)).s32_1 + asmStruct.sImmediate
                             If LoadAddr < -2146959360 Then LoadAddr += 2147483648

                             tAddr = LoadAddr ' PatchMemAddress(LoadAddr, tRS)
                             If tAddr < 0 Or tRS < 0 Then Return EXCEPTION_TLB_Load_InstFetch

                             MemData32.u32 = vRAM(tAddr \ 4) 'PSMemory(tRS).W(tAddr \ 4)

                             If (LoadAddr And 3) <> 0 Then Return EXCEPTION_TLB_Load_InstFetch

                             If rc(1) = 0 Then Return 0
                             GPr(rc(1)).s32_1 = MemData32.s32
                             GPr(rc(1)).s32_2 = 0
                             If GPr(rc(1)).s32_1 < 0 Then GPr(rc(1)).s32_2 = -1
                             Return 0
                         End Function '----------lw
        asmOp(72).Exec = Function() As Integer ' ld
                             'ld rt, %i(base)
                             '3-> base, rt, %i, 
                             Dim tAddr As Int32, tRS As Int16

                             LoadAddr = GPr(rc(0)).s32_1 + asmStruct.sImmediate
                             If LoadAddr < -2146959360 Then LoadAddr += 2147483648

                             tAddr = LoadAddr ' PatchMemAddress(LoadAddr, tRS)
                             If tAddr < 0 Or tRS < 0 Then Return EXCEPTION_TLB_Load_InstFetch
                             tAddr \= 4

                             If (LoadAddr And 7) <> 0 Then Return EXCEPTION_TLB_Load_InstFetch

                             If rc(1) = 0 Then Return 0
                             GPr(rc(1)).u32_1 = vRAM(tAddr) 'PSMemory(tRS).W(tAddr)
                             GPr(rc(1)).u32_2 = vRAM(tAddr + 1) 'PSMemory(tRS).W(tAddr + 1)
                             Return 0
                         End Function '----------ld
        asmOp(77).Exec = Function() As Integer ' lq
                             'lq rt, %i(base)
                             '3-> base, rt, %i, 
                             Dim tAddr As Int32, tRS As Int16

                             LoadAddr = GPr(rc(0)).s32_1 + asmStruct.sImmediate
                             If LoadAddr < -2146959360 Then LoadAddr += 2147483648

                             tAddr = LoadAddr 'PatchMemAddress(LoadAddr, tRS)
                             If tAddr < 0 Or tRS < 0 Then Return EXCEPTION_TLB_Load_InstFetch
                             tAddr \= 4

                             If (LoadAddr And 15) <> 0 Then Return EXCEPTION_TLB_Load_InstFetch

                             If rc(1) = 0 Then Return 0
                             GPr(rc(1)).u32_1 = vRAM(tAddr) 'PSMemory(tRS).W(tAddr)
                             GPr(rc(1)).u32_2 = vRAM(tAddr + 1) 'PSMemory(tRS).W(tAddr + 1)
                             GPr(rc(1)).u32_3 = vRAM(tAddr + 2) 'PSMemory(tRS).W(tAddr + 2)
                             GPr(rc(1)).u32_4 = vRAM(tAddr + 3) 'PSMemory(tRS).W(tAddr + 3)
                             Return 0
                         End Function '----------lq
        asmOp(71).Exec = Function() As Integer ' lbu
                             'lbu rt, %i(base)
                             '3-> base, rt, %i, 
                             Dim tAddr As Int32, tRS As Int16

                             LoadAddr = GPr(rc(0)).s32_1 + asmStruct.sImmediate
                             If LoadAddr < -2146959360 Then LoadAddr += 2147483648

                             tAddr = LoadAddr 'PatchMemAddress(LoadAddr, tRS)
                             If tAddr < 0 Or tRS < 0 Then Return EXCEPTION_TLB_Load_InstFetch

                             MemData32.u32 = vRAM(tAddr \ 4) 'PSMemory(tRS).W(tAddr \ 4)

                             If rc(1) = 0 Then Return 0
                             Select Case (LoadAddr And 3)
                                 Case 0
                                     GPr(rc(1)).u32_1 = MemData32.u8_1
                                 Case 1
                                     GPr(rc(1)).u32_1 = MemData32.u8_2
                                 Case 2
                                     GPr(rc(1)).u32_1 = MemData32.u8_3
                                 Case 3
                                     GPr(rc(1)).u32_1 = MemData32.u8_4
                             End Select
                             GPr(rc(1)).u32_2 = 0
                             Return 0
                         End Function '----------lbu
        asmOp(76).Exec = Function() As Integer ' lhu
                             'lhu rt, %i(base)
                             '3-> base, rt, %i, 
                             Dim tAddr As Int32, tRS As Int16, Bn As Byte

                             LoadAddr = GPr(rc(0)).s32_1 + asmStruct.sImmediate
                             If LoadAddr < -2146959360 Then LoadAddr += 2147483648

                             tAddr = LoadAddr 'PatchMemAddress(LoadAddr, tRS)
                             If tAddr < 0 Or tRS < 0 Then Return EXCEPTION_TLB_Load_InstFetch

                             MemData32.u32 = vRAM(tAddr \ 4) 'PSMemory(tRS).W(tAddr \ 4)

                             If rc(1) = 0 Then Return 0
                             Select Case (LoadAddr And 3)
                                 Case 0
                                     GPr(rc(1)).u32_1 = MemData32.u16_1
                                 Case 2
                                     GPr(rc(1)).u32_1 = MemData32.u16_2
                                 Case Else
                                     Return EXCEPTION_TLB_Load_InstFetch
                             End Select
                             GPr(rc(1)).s32_2 = 0
                             Return 0
                         End Function '----------lhu
        asmOp(83).Exec = Function() As Integer ' lwu
                             'lwu rt, %i(base)
                             '3-> base, rt, %i, 
                             Dim tAddr As Int32, tRS As Int16

                             LoadAddr = GPr(rc(0)).s32_1 + asmStruct.sImmediate
                             If LoadAddr < -2146959360 Then LoadAddr += 2147483648

                             tAddr = LoadAddr 'PatchMemAddress(LoadAddr, tRS)
                             If tAddr < 0 Or tRS < 0 Then Return EXCEPTION_TLB_Load_InstFetch

                             MemData32.u32 = vRAM(tAddr \ 4) 'PSMemory(tRS).W(tAddr \ 4)

                             If (LoadAddr And 3) <> 0 Then Return EXCEPTION_TLB_Load_InstFetch

                             If rc(1) = 0 Then Return 0
                             GPr(rc(1)).u32_1 = MemData32.u32
                             GPr(rc(1)).u32_2 = 0
                             Return 0
                         End Function '----------lwu
        asmOp(217).Exec = Function() As Integer ' sb
                              'sb rt, %i(base)
                              '3-> base, rt, %i, 
                              Dim tAddr As Int32, tRS As Int16

                              StoreAddr = GPr(rc(0)).s32_1 + asmStruct.sImmediate
                              If StoreAddr < -2146959360 Then StoreAddr += 2147483648

                              tAddr = StoreAddr 'PatchMemAddress(StoreAddr, tRS)
                              If tAddr < 0 Or tRS < 0 Then Return EXCEPTION_TLB_Load_InstFetch
                              tAddr \= 4

                              MemData32.u32 = vRAM(tAddr) 'PSMemory(tRS).W(tAddr)

                              Select Case (StoreAddr And 3)
                                  Case 0
                                      MemData32.u8_1 = GPr(rc(1)).u8_1
                                  Case 1
                                      MemData32.u8_2 = GPr(rc(1)).u8_1
                                  Case 2
                                      MemData32.u8_3 = GPr(rc(1)).u8_1
                                  Case 3
                                      MemData32.u8_4 = GPr(rc(1)).u8_1
                              End Select

                              vRAM(tAddr) = MemData32.u32 'PSMemory(tRS).W(tAddr) = MemData32.u32
                              'If PSMemory(tRS).IC(tAddr) <> -5 Then PSMemory(tRS).IC(tAddr) = -1
                              Return 0
                          End Function '----------sb
        asmOp(221).Exec = Function() As Integer ' sh
                              'sh rt, %i(base)
                              '3-> base, rt, %i, 
                              Dim tAddr As Int32, tRS As Int16

                              StoreAddr = GPr(rc(0)).s32_1 + asmStruct.sImmediate
                              If StoreAddr < -2146959360 Then StoreAddr += 2147483648

                              tAddr = StoreAddr  'PatchMemAddress(StoreAddr, tRS)
                              If tAddr < 0 Or tRS < 0 Then Return EXCEPTION_TLB_Load_InstFetch
                              tAddr \= 4

                              MemData32.u32 = vRAM(tAddr) 'PSMemory(tRS).W(tAddr)

                              Select Case (StoreAddr And 3)
                                  Case 0
                                      MemData32.u16_1 = GPr(rc(1)).u16_1
                                  Case 2
                                      MemData32.u16_2 = GPr(rc(1)).u16_1
                                  Case Else
                                      Return EXCEPTION_TLB_Load_InstFetch
                              End Select

                              vRAM(tAddr) = MemData32.u32 'PSMemory(tRS).W(tAddr) = MemData32.u32
                              'If PSMemory(tRS).IC(tAddr) <> -5 Then PSMemory(tRS).IC(tAddr) = -1
                              Return 0
                          End Function '----------sh
        asmOp(238).Exec = Function() As Integer ' sw
                              'sw rt, %i(base)
                              '3-> base, rt, %i, 
                              Dim tAddr As Int32, tRS As Int16

                              StoreAddr = GPr(rc(0)).s32_1 + asmStruct.sImmediate
                              If StoreAddr < -2146959360 Then StoreAddr += 2147483648

                              tAddr = StoreAddr 'PatchMemAddress(StoreAddr, tRS)
                              If tAddr < 0 Or tRS < 0 Then Return EXCEPTION_TLB_Load_InstFetch
                              tAddr \= 4

                              If (StoreAddr And 3) <> 0 Then Return EXCEPTION_TLB_Load_InstFetch

                              vRAM(tAddr) = GPr(rc(1)).u32_1 'PSMemory(tRS).W(tAddr) = GPr(rc(1)).u32_1
                              'If PSMemory(tRS).IC(tAddr) <> -5 Then PSMemory(tRS).IC(tAddr) = -1
                              Return 0
                          End Function '----------sw
        asmOp(218).Exec = Function() As Integer ' sd
                              'sd rt, %i(base)
                              '3-> base, rt, %i, 
                              Dim tAddr As Int32, tRS As Int16

                              StoreAddr = GPr(rc(0)).s32_1 + asmStruct.sImmediate
                              If StoreAddr < -2146959360 Then StoreAddr += 2147483648

                              tAddr = StoreAddr 'PatchMemAddress(StoreAddr, tRS)
                              If tAddr < 0 Or tRS < 0 Then Return EXCEPTION_TLB_Load_InstFetch
                              tAddr \= 4

                              If (StoreAddr And 7) <> 0 Then Return EXCEPTION_TLB_Load_InstFetch


                              vRAM(tAddr) = GPr(rc(1)).u32_1 'PSMemory(tRS).W(tAddr) = GPr(rc(1)).u32_1
                              vRAM(tAddr + 1) = GPr(rc(1)).u32_2 'PSMemory(tRS).W(tAddr + 1) = GPr(rc(1)).u32_2

                              'For i = tAddr To tAddr + 1
                              'If PSMemory(tRS).IC(i) <> -5 Then PSMemory(tRS).IC(i) = -1
                              'Next
                              Return 0
                          End Function '----------sd
        asmOp(228).Exec = Function() As Integer ' sq
                              'sq rt, %i(base)
                              '3-> base, rt, %i, 
                              Dim tAddr As Int32, tRS As Int16, i As Int32

                              StoreAddr = GPr(rc(0)).s32_1 + asmStruct.sImmediate
                              If StoreAddr < -2146959360 Then StoreAddr += 2147483648

                              tAddr = StoreAddr 'PatchMemAddress(StoreAddr, tRS)
                              If tAddr < 0 Or tRS < 0 Then Return EXCEPTION_TLB_Load_InstFetch
                              tAddr \= 4

                              If (StoreAddr And 15) <> 0 Then Return EXCEPTION_TLB_Load_InstFetch

                              vRAM(tAddr) = GPr(rc(1)).u32_1 'PSMemory(tRS).W(tAddr) = GPr(rc(1)).u32_1
                              vRAM(tAddr + 1) = GPr(rc(1)).u32_2 'PSMemory(tRS).W(tAddr + 1) = GPr(rc(1)).u32_2
                              vRAM(tAddr + 2) = GPr(rc(1)).u32_3 'PSMemory(tRS).W(tAddr + 2) = GPr(rc(1)).u32_3
                              vRAM(tAddr + 3) = GPr(rc(1)).u32_4 'PSMemory(tRS).W(tAddr + 3) = GPr(rc(1)).u32_4

                              'For i = tAddr To tAddr + 3
                              'If PSMemory(tRS).IC(i) <> -5 Then PSMemory(tRS).IC(i) = -1
                              'Next
                              Return 0
                          End Function '----------sq
        asmOp(80).Exec = Function() As Integer ' lwc1
                             'lwc1 ft, %i(base)
                             '3-> base, ft, %i, 
                             Dim tAddr As Int32, tRS As Int16

                             LoadAddr = GPr(rc(0)).s32_1 + asmStruct.sImmediate
                             If LoadAddr < -2146959360 Then LoadAddr += 2147483648

                             tAddr = LoadAddr 'PatchMemAddress(LoadAddr, tRS)
                             If tAddr < 0 Or tRS < 0 Then Return EXCEPTION_TLB_Load_InstFetch

                             If (LoadAddr And 3) <> 0 Then Return EXCEPTION_TLB_Load_InstFetch

                             FPr(rc(1)).u32 = vRAM(tAddr \ 4) 'PSMemory(tRS).W(tAddr \ 4)
                             Return 0
                         End Function '----------lwc1
        asmOp(239).Exec = Function() As Integer ' swc1
                              'swc1 ft, %i(base)
                              '3-> base, ft, %i, 
                              Dim tAddr As Int32, tRS As Int16

                              StoreAddr = GPr(rc(0)).s32_1 + asmStruct.sImmediate
                              If StoreAddr < -2146959360 Then StoreAddr += 2147483648

                              tAddr = StoreAddr 'PatchMemAddress(StoreAddr, tRS)
                              If tAddr < 0 Or tRS < 0 Then Return EXCEPTION_TLB_Load_InstFetch

                              If (LoadAddr And 3) <> 0 Then Return EXCEPTION_TLB_Load_InstFetch

                              vRAM(tAddr \ 4) = FPr(rc(1)).u32 'PSMemory(tRS).W(tAddr \ 4) = FPr(rc(1)).u32
                              Return 0
                          End Function '----------swc1

        asmOp(81).Exec = Function() As Integer ' lwl
                             'lwl rt, %i(base)
                             '3-> base, rt, %i, 
                             Return NotImplementedYet
                         End Function '----------lwl
        asmOp(82).Exec = Function() As Integer ' lwr
                             'lwr rt, %i(base)
                             '3-> base, rt, %i, 
                             Return NotImplementedYet
                         End Function '----------lwr
        asmOp(73).Exec = Function() As Integer ' ldl
                             'ldl rt, %i(base)
                             '3-> base, rt, %i, 
                             Return NotImplementedYet
                         End Function '----------ldl
        asmOp(74).Exec = Function() As Integer ' ldr
                             'ldr rt, %i(base)
                             '3-> base, rt, %i, 
                             Return NotImplementedYet
                         End Function '----------ldr
        asmOp(219).Exec = Function() As Integer ' sdl
                              'sdl rt, %i(base)
                              '3-> base, rt, %i, 
                              Return NotImplementedYet
                          End Function '----------sdl
        asmOp(220).Exec = Function() As Integer ' sdr
                              'sdr rt, %i(base)
                              '3-> base, rt, %i, 
                              Return NotImplementedYet
                          End Function '----------sdr
        asmOp(240).Exec = Function() As Integer ' swl
                              'swl rt, %i(base)
                              '3-> base, rt, %i, 
                              Return NotImplementedYet
                          End Function '----------swl
        asmOp(241).Exec = Function() As Integer ' swr
                              'swr rt, %i(base)
                              '3-> base, rt, %i, 
                              Return NotImplementedYet
                          End Function '----------swr

        '==================================================================== COP1
        asmOp(13).Exec = Function() As Integer ' bc1f
                             'bc1f %i
                             '1-> %i, 
                             If (FCr(31).u32 And &H400000) = 0 Then
                                 isBranching = 1
                                 GlobalNewIndex = GlobalIndex + asmStruct.sImmediate + 1
                             End If
                             Return 0
                         End Function '----------bc1f
        asmOp(14).Exec = Function() As Integer ' bc1fl
                             'bc1fl %i
                             '1-> %i, 
                             If (FCr(31).u32 And &H400000) = 0 Then
                                 isBranching = 1
                                 GlobalNewIndex = GlobalIndex + asmStruct.sImmediate + 1
                             Else
                                 GlobalIndex += 1
                             End If
                             Return 0
                         End Function '----------bc1fl
        asmOp(15).Exec = Function() As Integer ' bc1t
                             'bc1t %i
                             '1-> %i, 
                             If (FCr(31).u32 And &H400000) <> 0 Then
                                 isBranching = 1
                                 GlobalNewIndex = GlobalIndex + asmStruct.sImmediate + 1
                             End If
                             Return 0
                         End Function '----------bc1t
        asmOp(16).Exec = Function() As Integer ' bc1tl
                             'bc1tl %i
                             '1-> %i, 
                             If (FCr(31).u32 And &H400000) <> 0 Then
                                 isBranching = 1
                                 GlobalNewIndex = GlobalIndex + asmStruct.sImmediate + 1
                             Else
                                 GlobalIndex += 1
                             End If
                             Return 0
                         End Function '----------bc1tl
        asmOp(0).Exec = Function() As Integer ' abs.s
                            'abs.s fd, fs
                            '2-> fs, fd, 
                            FPr(rc(1)).u32 = FPr(rc(0)).u32
                            If FPr(rc(1)).f32 <> 0 Then FPr(rc(1)).f32 = Math.Abs(FPr(rc(1)).f32)
                            Return 0
                        End Function '----------abs.s
        asmOp(2).Exec = Function() As Integer ' add.s
                            'add.s fd, fs, ft
                            '3-> ft, fs, fd, 
                            FPr(rc(2)).f32 = FPr(rc(1)).f32 + FPr(rc(0)).f32
                            Return 0
                        End Function '----------add.s
        asmOp(235).Exec = Function() As Integer ' sub.s
                              'sub.s fd, fs, ft
                              '3-> ft, fs, fd, 
                              FPr(rc(2)).f32 = FPr(rc(1)).f32 - FPr(rc(0)).f32
                              Return 0
                          End Function '----------sub.s
        asmOp(3).Exec = Function() As Integer ' adda.s
                            'adda.s fs, ft
                            '2-> ft, fs, 
                            FAcc.f32 = FPr(rc(1)).f32 + FPr(rc(0)).f32
                            Return 0
                        End Function '----------adda.s
        asmOp(236).Exec = Function() As Integer ' suba.s
                              'suba.s fs, ft I
                              '2-> ft, fs, 
                              FAcc.f32 = FPr(rc(1)).f32 - FPr(rc(0)).f32
                              Return 0
                          End Function '----------suba.s
        asmOp(99).Exec = Function() As Integer ' mov.s
                             'mov.s fd, fs
                             '2-> fs, fd, 
                             FPr(rc(1)).u32 = FPr(rc(0)).u32
                             Return 0
                         End Function '----------mov.s
        asmOp(229).Exec = Function() As Integer ' sqrt.s
                              'sqrt.s fd, ft
                              '2-> ft, fd, 
                              FPr(rc(1)).f32 = Math.Sqrt(FPr(rc(0)).f32)
                              Return 0
                          End Function '----------sqrt.s
        asmOp(216).Exec = Function() As Integer ' rsqrt.s
                              'rsqrt.s fd, fs, ft
                              '3-> ft, fs, fd, 
                              FPr(rc(2)).f32 = FPr(rc(1)).f32 / Math.Sqrt(FPr(rc(0)).f32)
                              Return 0
                          End Function '----------rsqrt.s
        asmOp(90).Exec = Function() As Integer ' max.s
                             'max.s fd, fs, ft
                             '3-> ft, fs, fd, 
                             If FPr(rc(1)).f32 >= FPr(rc(0)).f32 Then
                                 FPr(rc(2)).f32 = FPr(rc(1)).f32
                             Else
                                 FPr(rc(2)).f32 = FPr(rc(0)).f32
                             End If

                             Return 0
                         End Function '----------max.s
        asmOp(98).Exec = Function() As Integer ' min.s
                             'min.s fd, fs, ft
                             '3-> ft, fs, fd, 
                             If FPr(rc(1)).f32 <= FPr(rc(0)).f32 Then
                                 FPr(rc(2)).f32 = FPr(rc(1)).f32
                             Else
                                 FPr(rc(2)).f32 = FPr(rc(0)).f32
                             End If

                             Return 0
                         End Function '----------min.s
        asmOp(119).Exec = Function() As Integer ' neg.s
                              'neg.s fd, fs
                              '2-> fs, fd, 
                              FPr(rc(1)).f32 = 0 - FPr(rc(0)).f32
                              Return 0
                          End Function '----------neg.s
        asmOp(113).Exec = Function() As Integer ' mul.s
                              'mul.s fd, fs, ft
                              '3-> ft, fs, fd, 
                              FPr(rc(2)).f32 = FPr(rc(1)).f32 * FPr(rc(0)).f32
                              Return 0
                          End Function '----------mul.s
        asmOp(114).Exec = Function() As Integer ' mula.s
                              'mula.s fs, ft
                              '2-> ft, fs, 
                              FAcc.f32 = FPr(rc(1)).f32 * FPr(rc(0)).f32
                              Return 0
                          End Function '----------mula.s
        asmOp(85).Exec = Function() As Integer ' madd.s
                             'madd.s fd, fs, ft
                             '3-> ft, fs, fd, 
                             FPr(rc(2)).f32 = FAcc.f32 + (FPr(rc(0)).f32 * FPr(rc(1)).f32)
                             Return 0
                         End Function '----------madd.s
        asmOp(87).Exec = Function() As Integer ' madda.s
                             'madda.s fs, ft
                             '2-> ft, fs, 
                             FAcc.f32 += (FPr(rc(0)).f32 * FPr(rc(1)).f32)
                             Return 0
                         End Function '----------madda.s
        asmOp(102).Exec = Function() As Integer ' msub.s
                              'msub.s fd, fs, ft
                              '3-> ft, fs, fd, 
                              FPr(rc(2)).f32 = FAcc.f32 - (FPr(rc(0)).f32 * FPr(rc(1)).f32)
                              Return 0
                          End Function '----------msub.s
        asmOp(103).Exec = Function() As Integer ' msuba.s
                              'msuba.s fs, ft
                              '2-> ft, fs, 
                              FAcc.f32 -= (FPr(rc(0)).f32 * FPr(rc(1)).f32)
                              Return 0
                          End Function '----------msuba.s
        asmOp(49).Exec = Function() As Integer ' div.s
                             'div.s fd, fs, ft
                             '3-> ft, fs, fd, 
                             FPr(rc(2)).f32 = FPr(rc(1)).f32 / FPr(rc(0)).f32
                             Return 0
                         End Function '----------div.s
        asmOp(41).Exec = Function() As Integer ' cvt.s.w
                             'cvt.s.w fd, fs
                             '2-> fs, fd, 
                             FPr(rc(1)).f32 = FPr(rc(0)).u32
                             Return 0
                         End Function '----------cvt.s.w
        asmOp(42).Exec = Function() As Integer ' cvt.w.s
                             'cvt.w.s fd, fs
                             '2-> fs, fd, 
                             FPr(rc(1)).u32 = FPr(rc(0)).f32
                             Return 0
                         End Function '----------cvt.w.s
        asmOp(34).Exec = Function() As Integer ' c.eq.s
                             'c.eq.s fs, ft
                             '2-> ft, fs, 
                             FCr(31).u32 = FCr(31).u32 And &HFFBFFFFF
                             If FPr(rc(1)).f32 = FPr(rc(0)).f32 Then FCr(31).u32 = FCr(31).u32 Or &H400000
                             Return 0
                         End Function '----------c.eq.s
        asmOp(36).Exec = Function() As Integer ' c.le.s
                             'c.le.s fs, ft
                             '2-> ft, fs, 
                             FCr(31).u32 = FCr(31).u32 And &HFFBFFFFF
                             If FPr(rc(1)).f32 <= FPr(rc(0)).f32 Then FCr(31).u32 = FCr(31).u32 Or &H400000
                             Return 0
                         End Function '----------c.le.s
        asmOp(37).Exec = Function() As Integer ' c.lt.s
                             'c.lt.s fs, ft
                             '2-> ft, fs, 
                             FCr(31).u32 = FCr(31).u32 And &HFFBFFFFF
                             If FPr(rc(1)).f32 < FPr(rc(0)).f32 Then FCr(31).u32 = FCr(31).u32 Or &H400000
                             Return 0
                         End Function '----------c.lt.s
        asmOp(35).Exec = Function() As Integer ' c.f.s
                             'c.f.s fs, ft
                             '2-> ft, fs, 
                             FCr(31).u32 = FCr(31).u32 And &HFFBFFFFF
                             Return 0
                         End Function '----------c.f.s

        '==================================================================== COP0

        asmOp(9).Exec = Function() As Integer ' bc0f
                            'bc0f %i
                            '1-> %i, 
                            Return NotImplementedYet
                        End Function '----------bc0f
        asmOp(10).Exec = Function() As Integer ' bc0fl
                             'bc0fl %i
                             '1-> %i, 
                             Return NotImplementedYet
                         End Function '----------bc0fl
        asmOp(11).Exec = Function() As Integer ' bc0t
                             'bc0t %i
                             '1-> %i, 
                             Return NotImplementedYet
                         End Function '----------bc0t
        asmOp(12).Exec = Function() As Integer ' bc0tl
                             'bc0tl %i
                             '1-> %i, 
                             Return NotImplementedYet
                         End Function '----------bc0tl

        '==================================================================== System
        asmOp(244).Exec = Function() As Integer ' syscall
                              'syscall (%d)
                              '1-> %d, 
                              GlobalAddress = GlobalIndex * 4 'RevertMemAddress(GlobalIndex * 4, GlobalSpace)
                              Return EXCEPTION_TLB_SysCall
                          End Function '----------syscall

        asmOp(33).Exec = Function() As Integer ' break
                             'break (%d)
                             '1-> %d, 
                             Return NotImplementedYet
                         End Function '----------break
        asmOp(38).Exec = Function() As Integer ' cache
                             'cache cvar, %i(base)
                             '3-> base, cvar, %i, 
                             Return NotImplementedYet
                         End Function '----------cache
        asmOp(47).Exec = Function() As Integer ' di
                             'di 
                             '0-> 
                             Return NotImplementedYet
                         End Function '----------di
        asmOp(64).Exec = Function() As Integer ' ei
                             'ei 
                             '0-> 
                             Return NotImplementedYet
                         End Function '----------ei
        asmOp(65).Exec = Function() As Integer ' eret
                             'eret 
                             '0-> 
                             Return NotImplementedYet
                         End Function '----------eret
        asmOp(242).Exec = Function() As Integer ' sync
                              'sync 
                              '0-> 
                              Return NotImplementedYet
                          End Function '----------sync
        asmOp(243).Exec = Function() As Integer ' sync.p
                              'sync.p 
                              '0-> 
                              Return NotImplementedYet
                          End Function '----------sync.p
        asmOp(193).Exec = Function() As Integer ' pref
                              'pref hint, %i(base)
                              '3-> base, hint, %i, 
                              Return NotImplementedYet
                          End Function '----------pref

        '==================================================================== Parallels
        '------------------------------------------------------------- Basic Math
        asmOp(126).Exec = Function() As Integer ' paddb
                              'paddb rd, rs, rt
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0

                              GPr(rc(2)).Byte1_1 = (GPr(rc(0)).sByte1_1 + GPr(rc(1)).sByte1_1) And 255
                              GPr(rc(2)).Byte1_2 = (GPr(rc(0)).sByte1_2 + GPr(rc(1)).sByte1_2) And 255
                              GPr(rc(2)).Byte1_3 = (GPr(rc(0)).sByte1_3 + GPr(rc(1)).sByte1_3) And 255
                              GPr(rc(2)).Byte1_4 = (GPr(rc(0)).sByte1_4 + GPr(rc(1)).sByte1_4) And 255

                              GPr(rc(2)).Byte2_1 = (GPr(rc(0)).sByte2_1 + GPr(rc(1)).sByte2_1) And 255
                              GPr(rc(2)).Byte2_2 = (GPr(rc(0)).sByte2_2 + GPr(rc(1)).sByte2_2) And 255
                              GPr(rc(2)).Byte2_3 = (GPr(rc(0)).sByte2_3 + GPr(rc(1)).sByte2_3) And 255
                              GPr(rc(2)).Byte2_4 = (GPr(rc(0)).sByte2_4 + GPr(rc(1)).sByte2_4) And 255

                              GPr(rc(2)).Byte3_1 = (GPr(rc(0)).sByte3_1 + GPr(rc(1)).sByte3_1) And 255
                              GPr(rc(2)).Byte3_2 = (GPr(rc(0)).sByte3_2 + GPr(rc(1)).sByte3_2) And 255
                              GPr(rc(2)).Byte3_3 = (GPr(rc(0)).sByte3_3 + GPr(rc(1)).sByte3_3) And 255
                              GPr(rc(2)).Byte3_4 = (GPr(rc(0)).sByte3_4 + GPr(rc(1)).sByte3_4) And 255

                              GPr(rc(2)).Byte4_1 = (GPr(rc(0)).sByte4_1 + GPr(rc(1)).sByte4_1) And 255
                              GPr(rc(2)).Byte4_2 = (GPr(rc(0)).sByte4_2 + GPr(rc(1)).sByte4_2) And 255
                              GPr(rc(2)).Byte4_3 = (GPr(rc(0)).sByte4_3 + GPr(rc(1)).sByte4_3) And 255
                              GPr(rc(2)).Byte4_4 = (GPr(rc(0)).sByte4_4 + GPr(rc(1)).sByte4_4) And 255

                              Return 0
                          End Function '----------paddb
        asmOp(127).Exec = Function() As Integer ' paddh
                              'paddh rd, rs, rt
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0

                              GPr(rc(2)).Half1_1 = (GPr(rc(0)).sHalf1_1 + GPr(rc(1)).sHalf1_1) And 65535
                              GPr(rc(2)).Half1_2 = (GPr(rc(0)).sHalf1_2 + GPr(rc(1)).sHalf1_2) And 65535

                              GPr(rc(2)).Half2_1 = (GPr(rc(0)).sHalf2_1 + GPr(rc(1)).sHalf2_1) And 65535
                              GPr(rc(2)).Half2_2 = (GPr(rc(0)).sHalf2_2 + GPr(rc(1)).sHalf2_2) And 65535

                              GPr(rc(2)).Half3_1 = (GPr(rc(0)).sHalf3_1 + GPr(rc(1)).sHalf3_1) And 65535
                              GPr(rc(2)).Half3_2 = (GPr(rc(0)).sHalf3_2 + GPr(rc(1)).sHalf3_2) And 65535

                              GPr(rc(2)).Half4_1 = (GPr(rc(0)).sHalf4_1 + GPr(rc(1)).sHalf4_1) And 65535
                              GPr(rc(2)).Half4_2 = (GPr(rc(0)).sHalf4_2 + GPr(rc(1)).sHalf4_2) And 65535

                              Return 0
                          End Function '----------paddh
        asmOp(134).Exec = Function() As Integer ' paddw
                              'paddw rd, rs, rt
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0

                              GPr(rc(2)).s64_1 = GPr(rc(0)).s32_1 + GPr(rc(1)).s32_1
                              GPr(rc(2)).s64_2 = GPr(rc(0)).s32_2 + GPr(rc(1)).s32_2
                              GPr(rc(2)).s64_3 = GPr(rc(0)).s32_3 + GPr(rc(1)).s32_3
                              GPr(rc(2)).s64_4 = GPr(rc(0)).s32_4 + GPr(rc(1)).s32_4

                              Return 0
                          End Function '----------paddw
        asmOp(205).Exec = Function() As Integer ' psubb
                              'psubb rd, rs, rt
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0

                              GPr(rc(2)).Byte1_1 = (GPr(rc(0)).sByte1_1 - GPr(rc(1)).sByte1_1) And 255
                              GPr(rc(2)).Byte1_2 = (GPr(rc(0)).sByte1_2 - GPr(rc(1)).sByte1_2) And 255
                              GPr(rc(2)).Byte1_3 = (GPr(rc(0)).sByte1_3 - GPr(rc(1)).sByte1_3) And 255
                              GPr(rc(2)).Byte1_4 = (GPr(rc(0)).sByte1_4 - GPr(rc(1)).sByte1_4) And 255

                              GPr(rc(2)).Byte2_1 = (GPr(rc(0)).sByte2_1 - GPr(rc(1)).sByte2_1) And 255
                              GPr(rc(2)).Byte2_2 = (GPr(rc(0)).sByte2_2 - GPr(rc(1)).sByte2_2) And 255
                              GPr(rc(2)).Byte2_3 = (GPr(rc(0)).sByte2_3 - GPr(rc(1)).sByte2_3) And 255
                              GPr(rc(2)).Byte2_4 = (GPr(rc(0)).sByte2_4 - GPr(rc(1)).sByte2_4) And 255

                              GPr(rc(2)).Byte3_1 = (GPr(rc(0)).sByte3_1 - GPr(rc(1)).sByte3_1) And 255
                              GPr(rc(2)).Byte3_2 = (GPr(rc(0)).sByte3_2 - GPr(rc(1)).sByte3_2) And 255
                              GPr(rc(2)).Byte3_3 = (GPr(rc(0)).sByte3_3 - GPr(rc(1)).sByte3_3) And 255
                              GPr(rc(2)).Byte3_4 = (GPr(rc(0)).sByte3_4 - GPr(rc(1)).sByte3_4) And 255

                              GPr(rc(2)).Byte4_1 = (GPr(rc(0)).sByte4_1 - GPr(rc(1)).sByte4_1) And 255
                              GPr(rc(2)).Byte4_2 = (GPr(rc(0)).sByte4_2 - GPr(rc(1)).sByte4_2) And 255
                              GPr(rc(2)).Byte4_3 = (GPr(rc(0)).sByte4_3 - GPr(rc(1)).sByte4_3) And 255
                              GPr(rc(2)).Byte4_4 = (GPr(rc(0)).sByte4_4 - GPr(rc(1)).sByte4_4) And 255

                              Return 0
                          End Function '----------psubb
        asmOp(206).Exec = Function() As Integer ' psubh
                              'psubh rd, rs, rt
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0

                              GPr(rc(2)).Half1_1 = (GPr(rc(0)).sHalf1_1 - GPr(rc(1)).sHalf1_1) And 65535
                              GPr(rc(2)).Half1_2 = (GPr(rc(0)).sHalf1_2 - GPr(rc(1)).sHalf1_2) And 65535

                              GPr(rc(2)).Half2_1 = (GPr(rc(0)).sHalf2_1 - GPr(rc(1)).sHalf2_1) And 65535
                              GPr(rc(2)).Half2_2 = (GPr(rc(0)).sHalf2_2 - GPr(rc(1)).sHalf2_2) And 65535

                              GPr(rc(2)).Half3_1 = (GPr(rc(0)).sHalf3_1 - GPr(rc(1)).sHalf3_1) And 65535
                              GPr(rc(2)).Half3_2 = (GPr(rc(0)).sHalf3_2 - GPr(rc(1)).sHalf3_2) And 65535

                              GPr(rc(2)).Half4_1 = (GPr(rc(0)).sHalf4_1 - GPr(rc(1)).sHalf4_1) And 65535
                              GPr(rc(2)).Half4_2 = (GPr(rc(0)).sHalf4_2 - GPr(rc(1)).sHalf4_2) And 65535

                              Return 0
                          End Function '----------psubh
        asmOp(213).Exec = Function() As Integer ' psubw
                              'psubw rd, rs, rt
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0

                              GPr(rc(2)).s64_1 = GPr(rc(0)).s32_1 - GPr(rc(1)).s32_1
                              GPr(rc(2)).s64_2 = GPr(rc(0)).s32_2 - GPr(rc(1)).s32_2
                              GPr(rc(2)).s64_3 = GPr(rc(0)).s32_3 - GPr(rc(1)).s32_3
                              GPr(rc(2)).s64_4 = GPr(rc(0)).s32_4 - GPr(rc(1)).s32_4

                              Return 0
                          End Function '----------psubw

        '------------------------------------------------------------- Misc Math
        asmOp(124).Exec = Function() As Integer ' pabsh
                              'pabsh rd, rt
                              '2-> rt, rd, 
                              If rc(1) = 0 Then Return 0

                              GPr(rc(1)).Half1_1 = Math.Abs(GPr(rc(0)).sHalf1_1)
                              GPr(rc(1)).Half1_2 = Math.Abs(GPr(rc(0)).sHalf1_2)

                              GPr(rc(1)).Half2_1 = Math.Abs(GPr(rc(0)).sHalf2_1)
                              GPr(rc(1)).Half2_2 = Math.Abs(GPr(rc(0)).sHalf2_2)

                              GPr(rc(1)).Half3_1 = Math.Abs(GPr(rc(0)).sHalf3_1)
                              GPr(rc(1)).Half3_2 = Math.Abs(GPr(rc(0)).sHalf3_2)

                              GPr(rc(1)).Half4_1 = Math.Abs(GPr(rc(0)).sHalf4_1)
                              GPr(rc(1)).Half4_2 = Math.Abs(GPr(rc(0)).sHalf4_2)

                              Return 0
                          End Function '----------pabsh
        asmOp(125).Exec = Function() As Integer ' pabsw
                              'pabsw rd, rt
                              '2-> rt, rd, 
                              If rc(1) = 0 Then Return 0

                              GPr(rc(1)).u32_1 = Math.Abs(GPr(rc(0)).s32_1)
                              GPr(rc(1)).u32_2 = Math.Abs(GPr(rc(0)).s32_2)
                              GPr(rc(1)).u32_3 = Math.Abs(GPr(rc(0)).s32_3)
                              GPr(rc(1)).u32_4 = Math.Abs(GPr(rc(0)).s32_4)

                              Return 0
                          End Function '----------pabsw

        '------------------------------------------------------------- Hard Math

        '------------------------------------------------------------- Bitwise
        asmOp(136).Exec = Function() As Integer ' pand
                              'pand rd, rs, rt
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0

                              GPr(rc(2)).u32_1 = GPr(rc(0)).u32_1 And GPr(rc(1)).u32_1
                              GPr(rc(2)).u32_2 = GPr(rc(0)).u32_2 And GPr(rc(1)).u32_2
                              GPr(rc(2)).u32_3 = GPr(rc(0)).u32_3 And GPr(rc(1)).u32_3
                              GPr(rc(2)).u32_4 = GPr(rc(0)).u32_4 And GPr(rc(1)).u32_4

                              Return 0
                          End Function '----------pand
        asmOp(188).Exec = Function() As Integer ' por
                              'por rd, rs, rt
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0

                              GPr(rc(2)).u32_1 = GPr(rc(0)).u32_1 Or GPr(rc(1)).u32_1
                              GPr(rc(2)).u32_2 = GPr(rc(0)).u32_2 Or GPr(rc(1)).u32_2
                              GPr(rc(2)).u32_3 = GPr(rc(0)).u32_3 Or GPr(rc(1)).u32_3
                              GPr(rc(2)).u32_4 = GPr(rc(0)).u32_4 Or GPr(rc(1)).u32_4

                              Return 0
                          End Function '----------por
        asmOp(187).Exec = Function() As Integer ' pnor
                              'pnor rd, rs, rt
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0

                              GPr(rc(2)).u32_1 = Not GPr(rc(0)).u32_1 Or GPr(rc(1)).u32_1
                              GPr(rc(2)).u32_2 = Not GPr(rc(0)).u32_2 Or GPr(rc(1)).u32_2
                              GPr(rc(2)).u32_3 = Not GPr(rc(0)).u32_3 Or GPr(rc(1)).u32_3
                              GPr(rc(2)).u32_4 = Not GPr(rc(0)).u32_4 Or GPr(rc(1)).u32_4

                              Return 0
                          End Function '----------pnor
        asmOp(214).Exec = Function() As Integer ' pxor
                              'pxor rd, rs, rt
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0

                              GPr(rc(2)).u32_1 = GPr(rc(0)).u32_1 Xor GPr(rc(1)).u32_1
                              GPr(rc(2)).u32_2 = GPr(rc(0)).u32_2 Xor GPr(rc(1)).u32_2
                              GPr(rc(2)).u32_3 = GPr(rc(0)).u32_3 Xor GPr(rc(1)).u32_3
                              GPr(rc(2)).u32_4 = GPr(rc(0)).u32_4 Xor GPr(rc(1)).u32_4

                              Return 0
                          End Function '----------pxor
        asmOp(140).Exec = Function() As Integer ' pcgtb
                              'pcgtb rd, rs, rt
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0

                              GPr(rc(2)).sByte1_1 = GPr(rc(0)).sByte1_1 > GPr(rc(1)).sByte1_1
                              GPr(rc(2)).sByte1_2 = GPr(rc(0)).sByte1_2 > GPr(rc(1)).sByte1_2
                              GPr(rc(2)).sByte1_3 = GPr(rc(0)).sByte1_3 > GPr(rc(1)).sByte1_3
                              GPr(rc(2)).sByte1_4 = GPr(rc(0)).sByte1_4 > GPr(rc(1)).sByte1_4

                              GPr(rc(2)).sByte2_1 = GPr(rc(0)).sByte2_1 > GPr(rc(1)).sByte2_1
                              GPr(rc(2)).sByte2_2 = GPr(rc(0)).sByte2_2 > GPr(rc(1)).sByte2_2
                              GPr(rc(2)).sByte2_3 = GPr(rc(0)).sByte2_3 > GPr(rc(1)).sByte2_3
                              GPr(rc(2)).sByte2_4 = GPr(rc(0)).sByte2_4 > GPr(rc(1)).sByte2_4

                              GPr(rc(2)).sByte3_1 = GPr(rc(0)).sByte3_1 > GPr(rc(1)).sByte3_1
                              GPr(rc(2)).sByte3_2 = GPr(rc(0)).sByte3_2 > GPr(rc(1)).sByte3_2
                              GPr(rc(2)).sByte3_3 = GPr(rc(0)).sByte3_3 > GPr(rc(1)).sByte3_3
                              GPr(rc(2)).sByte3_4 = GPr(rc(0)).sByte3_4 > GPr(rc(1)).sByte3_4

                              GPr(rc(2)).sByte4_1 = GPr(rc(0)).sByte4_1 > GPr(rc(1)).sByte4_1
                              GPr(rc(2)).sByte4_2 = GPr(rc(0)).sByte4_2 > GPr(rc(1)).sByte4_2
                              GPr(rc(2)).sByte4_3 = GPr(rc(0)).sByte4_3 > GPr(rc(1)).sByte4_3
                              GPr(rc(2)).sByte4_4 = GPr(rc(0)).sByte4_4 > GPr(rc(1)).sByte4_4

                              Return 0
                          End Function '----------pcgtb
        asmOp(141).Exec = Function() As Integer ' pcgth
                              'pcgth rd, rs, rt
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0

                              GPr(rc(2)).sHalf1_1 = GPr(rc(0)).sHalf1_1 > GPr(rc(1)).sHalf1_1
                              GPr(rc(2)).sHalf1_2 = GPr(rc(0)).sHalf1_2 > GPr(rc(1)).sHalf1_2
                              GPr(rc(2)).sHalf2_1 = GPr(rc(0)).sHalf2_1 > GPr(rc(1)).sHalf2_1
                              GPr(rc(2)).sHalf2_2 = GPr(rc(0)).sHalf2_2 > GPr(rc(1)).sHalf2_2
                              GPr(rc(2)).sHalf3_1 = GPr(rc(0)).sHalf3_1 > GPr(rc(1)).sHalf3_1
                              GPr(rc(2)).sHalf3_2 = GPr(rc(0)).sHalf3_2 > GPr(rc(1)).sHalf3_2
                              GPr(rc(2)).sHalf4_1 = GPr(rc(0)).sHalf4_1 > GPr(rc(1)).sHalf4_1
                              GPr(rc(2)).sHalf4_2 = GPr(rc(0)).sHalf4_2 > GPr(rc(1)).sHalf4_2

                              Return 0
                          End Function '----------pcgth
        asmOp(142).Exec = Function() As Integer ' pcgtw
                              'pcgtw rd, rs, rt
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0

                              GPr(rc(2)).s32_1 = GPr(rc(0)).s32_1 > GPr(rc(1)).s32_1
                              GPr(rc(2)).s32_2 = GPr(rc(0)).s32_2 > GPr(rc(1)).s32_2
                              GPr(rc(2)).s32_3 = GPr(rc(0)).s32_3 > GPr(rc(1)).s32_3
                              GPr(rc(2)).s32_4 = GPr(rc(0)).s32_4 > GPr(rc(1)).s32_4

                              Return 0
                          End Function '----------pcgtw

        '------------------------------------------------------------- Shifts
        asmOp(196).Exec = Function() As Integer ' psllh
                              'psllh rd, rt, sa
                              '3-> rt, rd, sa, 
                              If rc(1) = 0 Then Return 0

                              GPr(rc(1)).Half1_1 = (GPr(rc(0)).Half1_1 * (2 ^ rc(2))) And 65535
                              GPr(rc(1)).Half1_2 = (GPr(rc(0)).Half1_2 * (2 ^ rc(2))) And 65535

                              GPr(rc(1)).Half2_1 = (GPr(rc(0)).Half2_1 * (2 ^ rc(2))) And 65535
                              GPr(rc(1)).Half2_2 = (GPr(rc(0)).Half2_2 * (2 ^ rc(2))) And 65535

                              GPr(rc(1)).Half3_1 = (GPr(rc(0)).Half3_1 * (2 ^ rc(2))) And 65535
                              GPr(rc(1)).Half3_2 = (GPr(rc(0)).Half3_2 * (2 ^ rc(2))) And 65535

                              GPr(rc(1)).Half4_1 = (GPr(rc(0)).Half4_1 * (2 ^ rc(2))) And 65535
                              GPr(rc(1)).Half4_2 = (GPr(rc(0)).Half4_2 * (2 ^ rc(2))) And 65535

                              Return 0
                          End Function '----------psllh
        asmOp(198).Exec = Function() As Integer ' psllw
                              'psllw rd, rt, sa
                              '3-> rt, rd, sa, 
                              If rc(1) = 0 Then Return 0

                              GPr(rc(1)).u32_1 = (GPr(rc(0)).u32_1 * (2 ^ rc(2))) And 4294967295
                              GPr(rc(1)).u32_2 = (GPr(rc(0)).u32_2 * (2 ^ rc(2))) And 4294967295
                              GPr(rc(1)).u32_3 = (GPr(rc(0)).u32_3 * (2 ^ rc(2))) And 4294967295
                              GPr(rc(1)).u32_4 = (GPr(rc(0)).u32_4 * (2 ^ rc(2))) And 4294967295

                              Return 0
                          End Function '----------psllw
        asmOp(202).Exec = Function() As Integer ' psrlh
                              'psrlh rd, rt, sa
                              '3-> rt, rd, sa, 
                              If rc(1) = 0 Then Return 0

                              GPr(rc(1)).Half1_1 = (GPr(rc(0)).Half1_1 * (2 ^ rc(2))) And 65535
                              GPr(rc(1)).Half1_2 = (GPr(rc(0)).Half1_2 * (2 ^ rc(2))) And 65535

                              GPr(rc(1)).Half2_1 = (GPr(rc(0)).Half2_1 * (2 ^ rc(2))) And 65535
                              GPr(rc(1)).Half2_2 = (GPr(rc(0)).Half2_2 * (2 ^ rc(2))) And 65535

                              GPr(rc(1)).Half3_1 = (GPr(rc(0)).Half3_1 * (2 ^ rc(2))) And 65535
                              GPr(rc(1)).Half3_2 = (GPr(rc(0)).Half3_2 * (2 ^ rc(2))) And 65535

                              GPr(rc(1)).Half4_1 = (GPr(rc(0)).Half4_1 * (2 ^ rc(2))) And 65535
                              GPr(rc(1)).Half4_2 = (GPr(rc(0)).Half4_2 * (2 ^ rc(2))) And 65535

                              Return 0
                          End Function '----------psrlh
        asmOp(204).Exec = Function() As Integer ' psrlw
                              'psrlw rd, rt, sa
                              '3-> rt, rd, sa, 
                              If rc(1) = 0 Then Return 0

                              GPr(rc(1)).u32_1 = (GPr(rc(0)).u32_1 \ (2 ^ rc(2)))
                              GPr(rc(1)).u32_2 = (GPr(rc(0)).u32_2 \ (2 ^ rc(2)))
                              GPr(rc(1)).u32_3 = (GPr(rc(0)).u32_3 \ (2 ^ rc(2)))
                              GPr(rc(1)).u32_4 = (GPr(rc(0)).u32_4 \ (2 ^ rc(2)))

                              Return 0
                          End Function '----------psrlw

        asmOp(197).Exec = Function() As Integer ' psllvw
                              'psllvw rd, rt, rs
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------psllvw
        asmOp(203).Exec = Function() As Integer ' psrlvw
                              'psrlvw rd, rt, rs
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------psrlvw
        asmOp(199).Exec = Function() As Integer ' psrah
                              'psrah rd, rt, sa
                              '3-> rt, rd, sa, 
                              Return NotImplementedYet
                          End Function '----------psrah
        asmOp(200).Exec = Function() As Integer ' psravw
                              'psravw rd, rt, rs
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------psravw
        asmOp(201).Exec = Function() As Integer ' psraw
                              'psraw rd, rt, sa
                              '3-> rt, rd, sa, 
                              Return NotImplementedYet
                          End Function '----------psraw

        '------------------------------------------------------------- Extends
        asmOp(154).Exec = Function() As Integer ' pextlb
                              'pextlb rd, rs, rt
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0

                              GPr(rc(2)).Byte1_1 = GPr(rc(1)).Byte1_1
                              GPr(rc(2)).Byte1_2 = GPr(rc(0)).Byte1_1

                              GPr(rc(2)).Byte1_3 = GPr(rc(1)).Byte1_2
                              GPr(rc(2)).Byte1_4 = GPr(rc(0)).Byte1_2

                              GPr(rc(2)).Byte2_1 = GPr(rc(1)).Byte1_3
                              GPr(rc(2)).Byte2_2 = GPr(rc(0)).Byte1_3

                              GPr(rc(2)).Byte2_3 = GPr(rc(1)).Byte1_4
                              GPr(rc(2)).Byte2_4 = GPr(rc(0)).Byte1_4

                              GPr(rc(2)).Byte3_1 = GPr(rc(1)).Byte2_1
                              GPr(rc(2)).Byte3_2 = GPr(rc(0)).Byte2_1

                              GPr(rc(2)).Byte3_3 = GPr(rc(1)).Byte2_2
                              GPr(rc(2)).Byte3_4 = GPr(rc(0)).Byte2_2

                              GPr(rc(2)).Byte4_1 = GPr(rc(1)).Byte2_3
                              GPr(rc(2)).Byte4_2 = GPr(rc(0)).Byte2_3

                              GPr(rc(2)).Byte4_3 = GPr(rc(1)).Byte2_4
                              GPr(rc(2)).Byte4_4 = GPr(rc(0)).Byte2_4

                              Return 0
                          End Function '----------pextlb
        asmOp(155).Exec = Function() As Integer ' pextlh
                              'pextlh rd, rs, rt
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0

                              GPr(rc(2)).Half1_1 = GPr(rc(1)).Half1_1
                              GPr(rc(2)).Half1_2 = GPr(rc(0)).Half1_1

                              GPr(rc(2)).Half2_1 = GPr(rc(1)).Half1_2
                              GPr(rc(2)).Half2_2 = GPr(rc(0)).Half1_2

                              GPr(rc(2)).Half3_1 = GPr(rc(1)).Half2_1
                              GPr(rc(2)).Half3_2 = GPr(rc(0)).Half2_1

                              GPr(rc(2)).Half4_1 = GPr(rc(1)).Half2_2
                              GPr(rc(2)).Half4_2 = GPr(rc(0)).Half2_2

                              Return 0
                          End Function '----------pextlh
        asmOp(156).Exec = Function() As Integer ' pextlw
                              'pextlw rd, rs, rt
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0

                              GPr(rc(2)).u32_1 = GPr(rc(1)).u32_1
                              GPr(rc(2)).u32_2 = GPr(rc(0)).u32_1
                              GPr(rc(2)).u32_3 = GPr(rc(1)).u32_2
                              GPr(rc(2)).u32_4 = GPr(rc(0)).u32_2

                              Return 0
                          End Function '----------pextlw
        asmOp(157).Exec = Function() As Integer ' pextub
                              'pextub rd, rs, rt
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0

                              GPr(rc(2)).Byte1_1 = GPr(rc(1)).Byte3_1
                              GPr(rc(2)).Byte1_2 = GPr(rc(0)).Byte3_1

                              GPr(rc(2)).Byte1_3 = GPr(rc(1)).Byte3_2
                              GPr(rc(2)).Byte1_4 = GPr(rc(0)).Byte3_2

                              GPr(rc(2)).Byte2_1 = GPr(rc(1)).Byte3_3
                              GPr(rc(2)).Byte2_2 = GPr(rc(0)).Byte3_3

                              GPr(rc(2)).Byte2_3 = GPr(rc(1)).Byte3_4
                              GPr(rc(2)).Byte2_4 = GPr(rc(0)).Byte3_4

                              GPr(rc(2)).Byte3_1 = GPr(rc(1)).Byte4_1
                              GPr(rc(2)).Byte3_2 = GPr(rc(0)).Byte4_1

                              GPr(rc(2)).Byte3_3 = GPr(rc(1)).Byte4_2
                              GPr(rc(2)).Byte3_4 = GPr(rc(0)).Byte4_2

                              GPr(rc(2)).Byte4_1 = GPr(rc(1)).Byte4_3
                              GPr(rc(2)).Byte4_2 = GPr(rc(0)).Byte4_3

                              GPr(rc(2)).Byte4_3 = GPr(rc(1)).Byte4_4
                              GPr(rc(2)).Byte4_4 = GPr(rc(0)).Byte4_4

                              Return 0
                          End Function '----------pextub
        asmOp(158).Exec = Function() As Integer ' pextuh
                              'pextuh rd, rs, rt
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0

                              GPr(rc(2)).Half1_1 = GPr(rc(1)).Half3_1
                              GPr(rc(2)).Half1_2 = GPr(rc(0)).Half3_1

                              GPr(rc(2)).Half2_1 = GPr(rc(1)).Half3_2
                              GPr(rc(2)).Half2_2 = GPr(rc(0)).Half3_2

                              GPr(rc(2)).Half3_1 = GPr(rc(1)).Half4_1
                              GPr(rc(2)).Half3_2 = GPr(rc(0)).Half4_1

                              GPr(rc(2)).Half4_1 = GPr(rc(1)).Half4_2
                              GPr(rc(2)).Half4_2 = GPr(rc(0)).Half4_2

                              Return 0
                          End Function '----------pextuh
        asmOp(159).Exec = Function() As Integer ' pextuw
                              'pextuw rd, rs, rt
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0

                              GPr(rc(2)).u32_1 = GPr(rc(1)).u32_3
                              GPr(rc(2)).u32_2 = GPr(rc(0)).u32_3
                              GPr(rc(2)).u32_3 = GPr(rc(1)).u32_4
                              GPr(rc(2)).u32_4 = GPr(rc(0)).u32_4

                              Return 0
                          End Function '----------pextuw

        asmOp(153).Exec = Function() As Integer ' pext5
                              'pext5 rd, rt
                              '2-> rt, rd, 
                              Return NotImplementedYet
                          End Function '----------pext5

        '------------------------------------------------------------- Data Rotation
        asmOp(195).Exec = Function() As Integer ' prot3w
                              'prot3w rd, rt
                              '2-> rt, rd, 
                              If rc(1) = 0 Then Return 0

                              GPr(rc(1)).u32_1 = GPr(rc(0)).u32_2
                              GPr(rc(1)).u32_2 = GPr(rc(0)).u32_3
                              GPr(rc(1)).u32_3 = GPr(rc(0)).u32_1
                              GPr(rc(1)).u32_4 = GPr(rc(0)).u32_4

                              Return 0
                          End Function '----------prot3w
        asmOp(194).Exec = Function() As Integer ' prevh
                              'prevh rd, rt
                              '2-> rt, rd, 
                              If rc(1) = 0 Then Return 0

                              GPr(rc(1)).Half1_1 = GPr(rc(0)).Half2_2
                              GPr(rc(1)).Half1_2 = GPr(rc(0)).Half2_1
                              GPr(rc(1)).Half2_1 = GPr(rc(0)).Half1_2
                              GPr(rc(1)).Half2_2 = GPr(rc(0)).Half1_1

                              GPr(rc(1)).Half3_1 = GPr(rc(0)).Half4_2
                              GPr(rc(1)).Half3_2 = GPr(rc(0)).Half4_1
                              GPr(rc(1)).Half4_1 = GPr(rc(0)).Half3_2
                              GPr(rc(1)).Half4_2 = GPr(rc(0)).Half3_1

                              Return 0
                          End Function '----------prevh
        asmOp(143).Exec = Function() As Integer ' pcpyh
                              'pcpyh rd, rt
                              '2-> rt, rd, 
                              If rc(1) = 0 Then Return 0

                              GPr(rc(1)).Half1_1 = GPr(rc(0)).Half1_1
                              GPr(rc(1)).Half1_2 = GPr(rc(0)).Half1_1
                              GPr(rc(1)).Half2_1 = GPr(rc(0)).Half1_1
                              GPr(rc(1)).Half2_2 = GPr(rc(0)).Half1_1

                              GPr(rc(1)).Half3_1 = GPr(rc(0)).Half3_1
                              GPr(rc(1)).Half3_2 = GPr(rc(0)).Half3_1
                              GPr(rc(1)).Half4_1 = GPr(rc(0)).Half3_1
                              GPr(rc(1)).Half4_2 = GPr(rc(0)).Half3_1

                              Return 0
                          End Function '----------pcpyh
        asmOp(144).Exec = Function() As Integer ' pcpyld
                              'pcpyld rd, rs, rt
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0

                              GPr(rc(2)).u32_1 = GPr(rc(1)).u32_1
                              GPr(rc(2)).u32_2 = GPr(rc(1)).u32_2

                              GPr(rc(2)).u32_3 = GPr(rc(0)).u32_1
                              GPr(rc(2)).u32_4 = GPr(rc(0)).u32_2

                              Return 0
                          End Function '----------pcpyld
        asmOp(145).Exec = Function() As Integer ' pcpyud
                              'pcpyud rd, rs, rt
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0

                              GPr(rc(2)).u32_1 = GPr(rc(0)).u32_3
                              GPr(rc(2)).u32_2 = GPr(rc(0)).u32_4

                              GPr(rc(2)).u32_3 = GPr(rc(1)).u32_3
                              GPr(rc(2)).u32_4 = GPr(rc(1)).u32_4

                              Return 0
                          End Function '----------pcpyud

        '------------------------------------------------------------- Data Packing
        asmOp(190).Exec = Function() As Integer ' ppacb
                              'ppacb rd, rs, rt
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0

                              GPr(rc(2)).Byte1_1 = GPr(rc(1)).Byte1_1
                              GPr(rc(2)).Byte1_2 = GPr(rc(1)).Byte1_3
                              GPr(rc(2)).Byte1_3 = GPr(rc(1)).Byte2_1
                              GPr(rc(2)).Byte1_4 = GPr(rc(1)).Byte2_3
                              GPr(rc(2)).Byte2_1 = GPr(rc(1)).Byte3_1
                              GPr(rc(2)).Byte2_2 = GPr(rc(1)).Byte3_3
                              GPr(rc(2)).Byte2_3 = GPr(rc(1)).Byte4_1
                              GPr(rc(2)).Byte2_4 = GPr(rc(1)).Byte4_3

                              GPr(rc(2)).Byte3_1 = GPr(rc(0)).Byte1_1
                              GPr(rc(2)).Byte3_2 = GPr(rc(0)).Byte1_3
                              GPr(rc(2)).Byte3_3 = GPr(rc(0)).Byte2_1
                              GPr(rc(2)).Byte3_4 = GPr(rc(0)).Byte2_3
                              GPr(rc(2)).Byte4_1 = GPr(rc(0)).Byte3_1
                              GPr(rc(2)).Byte4_2 = GPr(rc(0)).Byte3_3
                              GPr(rc(2)).Byte4_3 = GPr(rc(0)).Byte4_1
                              GPr(rc(2)).Byte4_4 = GPr(rc(0)).Byte4_3

                              Return 0
                          End Function '----------ppacb
        asmOp(191).Exec = Function() As Integer ' ppach
                              'ppach rd, rs, rt
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0

                              GPr(rc(2)).Half1_1 = GPr(rc(1)).Half1_1
                              GPr(rc(2)).Half1_2 = GPr(rc(1)).Half2_1
                              GPr(rc(2)).Half2_1 = GPr(rc(1)).Half3_1
                              GPr(rc(2)).Half2_2 = GPr(rc(1)).Half4_1

                              GPr(rc(2)).Half3_1 = GPr(rc(0)).Half1_1
                              GPr(rc(2)).Half3_2 = GPr(rc(0)).Half2_1
                              GPr(rc(2)).Half4_1 = GPr(rc(0)).Half3_1
                              GPr(rc(2)).Half4_2 = GPr(rc(0)).Half4_1

                              Return 0
                          End Function '----------ppach
        asmOp(192).Exec = Function() As Integer ' ppacw
                              'ppacw rd, rs, rt
                              '3-> rs, rt, rd, 
                              If rc(2) = 0 Then Return 0

                              GPr(rc(2)).u32_1 = GPr(rc(1)).u32_1
                              GPr(rc(2)).u32_2 = GPr(rc(1)).u32_3

                              GPr(rc(2)).u32_3 = GPr(rc(0)).u32_1
                              GPr(rc(2)).u32_4 = GPr(rc(0)).u32_3

                              Return 0
                          End Function '----------ppacw

        asmOp(189).Exec = Function() As Integer ' ppac5
                              'ppac5 rd, rt
                              '2-> rt, rd, 
                              Return NotImplementedYet
                          End Function '----------ppac5


        asmOp(128).Exec = Function() As Integer ' paddsb
                              'paddsb rd, rs, rt
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------paddsb
        asmOp(129).Exec = Function() As Integer ' paddsh
                              'paddsh rd, rs, rt
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------paddsh
        asmOp(130).Exec = Function() As Integer ' paddsw
                              'paddsw rd, rs, rt
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------paddsw
        asmOp(131).Exec = Function() As Integer ' paddub
                              'paddub rd, rs, rt
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------paddub
        asmOp(132).Exec = Function() As Integer ' padduh
                              'padduh rd, rs, rt
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------padduh
        asmOp(133).Exec = Function() As Integer ' padduw
                              'padduw rd, rs, rt
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------padduw
        asmOp(135).Exec = Function() As Integer ' padsbh
                              'padsbh rd, rs, rt
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------padsbh
        asmOp(137).Exec = Function() As Integer ' pceqb
                              'pceqb rd, rs, rt
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------pceqb
        asmOp(138).Exec = Function() As Integer ' pceqh
                              'pceqh rd, rs, rt
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------pceqh
        asmOp(139).Exec = Function() As Integer ' pceqw
                              'pceqw rd, rs, rt
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------pceqw
        asmOp(146).Exec = Function() As Integer ' pdivbw
                              'pdivbw rs, rt
                              '2-> rs, rt, 
                              Return NotImplementedYet
                          End Function '----------pdivbw
        asmOp(147).Exec = Function() As Integer ' pdivuw
                              'pdivuw rs, rt
                              '2-> rs, rt, 
                              Return NotImplementedYet
                          End Function '----------pdivuw
        asmOp(148).Exec = Function() As Integer ' pdivw
                              'pdivw rs, rt
                              '2-> rs, rt, 
                              Return NotImplementedYet
                          End Function '----------pdivw
        asmOp(149).Exec = Function() As Integer ' pexch
                              'pexch rd, rt
                              '2-> rt, rd, 
                              Return NotImplementedYet
                          End Function '----------pexch
        asmOp(150).Exec = Function() As Integer ' pexcw
                              'pexcw rd, rt
                              '2-> rt, rd, 
                              Return NotImplementedYet
                          End Function '----------pexcw
        asmOp(151).Exec = Function() As Integer ' pexeh
                              'pexeh rd, rt
                              '2-> rt, rd, 
                              Return NotImplementedYet
                          End Function '----------pexeh
        asmOp(152).Exec = Function() As Integer ' pexew
                              'pexew rd, rt
                              '2-> rt, rd, 
                              Return NotImplementedYet
                          End Function '----------pexew
        asmOp(160).Exec = Function() As Integer ' phmadh
                              'phmadh rd, rs, rt
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------phmadh
        asmOp(161).Exec = Function() As Integer ' phmsbh
                              'phmsbh rd, rs, rt
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------phmsbh
        asmOp(162).Exec = Function() As Integer ' pinteh
                              'pinteh rd, rs, rt
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------pinteh
        asmOp(163).Exec = Function() As Integer ' pinth
                              'pinth rd, rs, rt
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------pinth
        asmOp(164).Exec = Function() As Integer ' plzcw
                              'plzcw rd, rs
                              '2-> rs, rd, 
                              Return NotImplementedYet
                          End Function '----------plzcw
        asmOp(165).Exec = Function() As Integer ' pmaddh
                              'pmaddh rd, rs, rt
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------pmaddh
        asmOp(166).Exec = Function() As Integer ' pmadduw
                              'pmadduw rd, rs, rt
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------pmadduw
        asmOp(167).Exec = Function() As Integer ' pmaddw
                              'pmaddw rd, rs, rt
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------pmaddw
        asmOp(168).Exec = Function() As Integer ' pmaxh
                              'pmaxh rd, rs, rt
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------pmaxh
        asmOp(169).Exec = Function() As Integer ' pmaxw
                              'pmaxw rd, rs, rt
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------pmaxw
        asmOp(170).Exec = Function() As Integer ' pmfhi
                              'pmfhi rd
                              '1-> rd, 
                              Return NotImplementedYet
                          End Function '----------pmfhi
        asmOp(171).Exec = Function() As Integer ' pmfhl.lh
                              'pmfhl.lh rd
                              '1-> rd, 
                              Return NotImplementedYet
                          End Function '----------pmfhl.lh
        asmOp(172).Exec = Function() As Integer ' pmfhl.lw
                              'pmfhl.lw rd
                              '1-> rd, 
                              Return NotImplementedYet
                          End Function '----------pmfhl.lw
        asmOp(173).Exec = Function() As Integer ' pmfhl.sh
                              'pmfhl.sh rd
                              '1-> rd, 
                              Return NotImplementedYet
                          End Function '----------pmfhl.sh
        asmOp(174).Exec = Function() As Integer ' pmfhl.slw
                              'pmfhl.slw rd
                              '1-> rd, 
                              Return NotImplementedYet
                          End Function '----------pmfhl.slw
        asmOp(175).Exec = Function() As Integer ' pmfhl.uw
                              'pmfhl.uw rd
                              '1-> rd, 
                              Return NotImplementedYet
                          End Function '----------pmfhl.uw
        asmOp(176).Exec = Function() As Integer ' pmflo
                              'pmflo rd
                              '1-> rd, 
                              Return NotImplementedYet
                          End Function '----------pmflo
        asmOp(177).Exec = Function() As Integer ' pminh
                              'pminh rd, rs, rt
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------pminh
        asmOp(178).Exec = Function() As Integer ' pminw
                              'pminw rd, rs, rt
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------pminw
        asmOp(179).Exec = Function() As Integer ' pmsubh
                              'pmsubh rd, rs, rt
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------pmsubh
        asmOp(180).Exec = Function() As Integer ' pmsubw
                              'pmsubw rd, rs, rt
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------pmsubw
        asmOp(181).Exec = Function() As Integer ' pmthi
                              'pmthi rs
                              '1-> rs, 
                              Return NotImplementedYet
                          End Function '----------pmthi
        asmOp(182).Exec = Function() As Integer ' pmthl.lw
                              'pmthl.lw rs
                              '1-> rs, 
                              Return NotImplementedYet
                          End Function '----------pmthl.lw
        asmOp(183).Exec = Function() As Integer ' pmtlo
                              'pmtlo rs
                              '1-> rs, 
                              Return NotImplementedYet
                          End Function '----------pmtlo
        asmOp(184).Exec = Function() As Integer ' pmulth
                              'pmulth rd, rs, rt
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------pmulth
        asmOp(185).Exec = Function() As Integer ' pmultuw
                              'pmultuw rd, rs, rt
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------pmultuw
        asmOp(186).Exec = Function() As Integer ' pmultw
                              'pmultw rd, rs, rt
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------pmultw
        asmOp(207).Exec = Function() As Integer ' psubsb
                              'psubsb rd, rs, rt
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------psubsb
        asmOp(208).Exec = Function() As Integer ' psubsh
                              'psubsh rd, rs, rt
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------psubsh
        asmOp(209).Exec = Function() As Integer ' psubsw
                              'psubsw rd, rs, rt
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------psubsw
        asmOp(210).Exec = Function() As Integer ' psubub
                              'psubub rd, rs, rt
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------psubub
        asmOp(211).Exec = Function() As Integer ' psubuh
                              'psubuh rd, rs, rt
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------psubuh
        asmOp(212).Exec = Function() As Integer ' psubuw
                              'psubuw rd, rs, rt
                              '3-> rs, rt, rd, 
                              Return NotImplementedYet
                          End Function '----------psubuw

        '==================================================================== Traps
        asmOp(245).Exec = Function() As Integer ' teq
                              'teq rs, rt (%d)
                              '3-> rs, rt, %d, 
                              Return NotImplementedYet
                          End Function '----------teq
        asmOp(246).Exec = Function() As Integer ' teqi
                              'teqi rs, %i
                              '2-> rs, %i, 
                              Return NotImplementedYet
                          End Function '----------teqi
        asmOp(247).Exec = Function() As Integer ' tge
                              'tge rs, rt (%d)
                              '3-> rs, rt, %d, 
                              Return NotImplementedYet
                          End Function '----------tge
        asmOp(248).Exec = Function() As Integer ' tgei
                              'tgei rs, %i
                              '2-> rs, %i, 
                              Return NotImplementedYet
                          End Function '----------tgei
        asmOp(249).Exec = Function() As Integer ' tgeiu
                              'tgeiu rs, %i
                              '2-> rs, %i, 
                              Return NotImplementedYet
                          End Function '----------tgeiu
        asmOp(250).Exec = Function() As Integer ' tgeu
                              'tgeu rs, rt (%d)
                              '3-> rs, rt, %d, 
                              Return NotImplementedYet
                          End Function '----------tgeu
        asmOp(251).Exec = Function() As Integer ' tlt
                              'tlt rs, rt (%d)
                              '3-> rs, rt, %d, 
                              Return NotImplementedYet
                          End Function '----------tlt
        asmOp(252).Exec = Function() As Integer ' tlti
                              'tlti rs, %i
                              '2-> rs, %i, 
                              Return NotImplementedYet
                          End Function '----------tlti
        asmOp(253).Exec = Function() As Integer ' tltiu
                              'tltiu rs, %i
                              '2-> rs, %i, 
                              Return NotImplementedYet
                          End Function '----------tltiu
        asmOp(254).Exec = Function() As Integer ' tltu
                              'tltu rs, rt (%d)
                              '3-> rs, rt, %d, 
                              Return NotImplementedYet
                          End Function '----------tltu
        asmOp(255).Exec = Function() As Integer ' tne
                              'tne rs, rt (%d)
                              '3-> rs, rt, %d, 
                              Return NotImplementedYet
                          End Function '----------tne
        asmOp(256).Exec = Function() As Integer ' tnei
                              'tnei rs, %i
                              '2-> rs, %i, 
                              Return NotImplementedYet
                          End Function '----------tnei

    End Sub
End Class
