Module RAM
    Public PSMemory() As MemoryBlock

    Public Const EE_Space = 0
    Public Const KSEG0_Space = 1
    Public Const ScratchPad_Space = 2
    Public Const IOP_Space = 3
    Public Const ROM0_Space = 4

    Public Structure RegCache
        Dim r() As Int32
    End Structure
    Public Structure TLBMapping
        Dim StartAddr As Int32
        Dim StopAddr As Int32

        Dim AddrFix As Int32
    End Structure
    Public Structure MemoryBlock
        Dim TLB() As TLBMapping

        Dim W() As UInt32
        Dim IC() As Int16
        Dim RC() As RegCache
    End Structure

    Public Sub InitRAM()
        Dim MemSz As Long
        Dim BitSize As Integer

        BitSize = 4 ' 32 bit words

        ReDim PSMemory(4)

        '-------------------------------------------------- Initialize EE RAM
        MemSz = CDec("&H01F7FFFF")
        With PSMemory(EE_Space)
            ReDim .W(((MemSz + 1) / BitSize) - 1)
            ReDim .IC(((MemSz + 1) / 4) - 1)
            ReDim .RC(((MemSz + 1) / 4) - 1)
            ReDim .TLB(2)
            '-------------------------------------- 00080000 - 02000000
            .TLB(0).StartAddr = Val("&H00080000")
            .TLB(0).StopAddr = Val("&H02000000")
            .TLB(0).AddrFix = Val("&H00080000")
            '-------------------------------------- 20080000 - 22000000
            .TLB(1).StartAddr = Val("&H20080000")
            .TLB(1).StopAddr = Val("&H22000000")
            .TLB(1).AddrFix = Val("&H20080000")
            '-------------------------------------- 30080000 - 32000000
            .TLB(2).StartAddr = Val("&H30080000")
            .TLB(2).StopAddr = Val("&H32000000")
            .TLB(2).AddrFix = Val("&H30080000")
        End With
        FlushMemory(EE_Space)
        FlushCache(EE_Space)

        '-------------------------------------------------- Initialize KSEG0 RAM
        MemSz = CDec("&H0007FFFF")
        With PSMemory(KSEG0_Space)
            ReDim .W(((MemSz + 1) / BitSize) - 1)
            ReDim .IC(((MemSz + 1) / 4) - 1)
            ReDim .RC(((MemSz + 1) / 4) - 1)
            ReDim .TLB(1)
            '-------------------------------------- 80000000 - 80080000
            .TLB(0).StartAddr = Val("&H80000000")
            .TLB(0).StopAddr = Val("&H80080000")
            .TLB(0).AddrFix = Val("&H80000000")
            '-------------------------------------- A0000000 - A0080000
            .TLB(1).StartAddr = Val("&HA0000000")
            .TLB(1).StopAddr = Val("&HA0080000")
            .TLB(1).AddrFix = Val("&HA0000000")
        End With
        FlushMemory(KSEG0_Space)
        FlushCache(KSEG0_Space)

        '-------------------------------------------------- Initialize ScratchPad RAM
        MemSz = CDec("&H00003FFF")
        With PSMemory(ScratchPad_Space)
            ReDim .W(((MemSz + 1) / BitSize) - 1)
            ReDim .IC(((MemSz + 1) / 4) - 1)
            ReDim .RC(((MemSz + 1) / 4) - 1)
            ReDim .TLB(0)
            '-------------------------------------- 70000000 - 70004000
            .TLB(0).StartAddr = Val("&H70000000")
            .TLB(0).StopAddr = Val("&H70004000")
            .TLB(0).AddrFix = Val("&H70000000")
        End With
        FlushMemory(ScratchPad_Space)
        FlushCache(ScratchPad_Space)

        '-------------------------------------------------- Initialize IOP RAM
        MemSz = CDec("&H001FFFFF")
        With PSMemory(IOP_Space)
            ReDim .W(((MemSz + 1) / BitSize) - 1)
            ReDim .IC(((MemSz + 1) / 4) - 1)
            ReDim .RC(((MemSz + 1) / 4) - 1)
            ReDim .TLB(0)
            '-------------------------------------- BC000000 - BC200000
            .TLB(0).StartAddr = Val("&HBC000000")
            .TLB(0).StopAddr = Val("&HBC200000")
            .TLB(0).AddrFix = Val("&HBC000000")
        End With
        FlushMemory(IOP_Space)
        FlushCache(IOP_Space)

        '-------------------------------------------------- Initialize ROM0
        MemSz = CDec("&H003FFFFF")
        With PSMemory(ROM0_Space)
            ReDim .W(((MemSz + 1) / BitSize) - 1)
            ReDim .IC(((MemSz + 1) / 4) - 1)
            ReDim .RC(((MemSz + 1) / 4) - 1)
            ReDim .TLB(2)
            '-------------------------------------- 1FC00000 - 20000000
            .TLB(0).StartAddr = Val("&H1FC00000")
            .TLB(0).StopAddr = Val("&H20000000")
            .TLB(0).AddrFix = Val("&H1FC00000")
            '-------------------------------------- 9FC00000 - A0000000
            .TLB(1).StartAddr = Val("&H9FC00000")
            .TLB(1).StopAddr = Val("&HA0000000")
            .TLB(1).AddrFix = Val("&H9FC00000")
            '-------------------------------------- BFC00000 - C0000000
            .TLB(2).StartAddr = Val("&HBFC00000")
            .TLB(2).StopAddr = Val("&HC0000000")
            .TLB(2).AddrFix = Val("&HBFC00000")
        End With
        FlushMemory(ROM0_Space)
        FlushCache(ROM0_Space)

    End Sub

    Public Sub FlushMemory(RAMSpace As Integer)
        Dim I As Int32
        For I = 0 To PSMemory(RAMSpace).IC.Count() - 1
            PSMemory(RAMSpace).W(I) = 0
        Next
    End Sub

    Public Sub FlushCache(RAMSpace As Integer)
        Dim I As Int32
        For I = 0 To PSMemory(RAMSpace).IC.Count() - 1
            PSMemory(RAMSpace).IC(I) = -1
            ReDim PSMemory(RAMSpace).RC(I).r(2)
            PSMemory(RAMSpace).RC(I).r(0) = 0
            PSMemory(RAMSpace).RC(I).r(1) = 0
            PSMemory(RAMSpace).RC(I).r(2) = 0
        Next
    End Sub



    '==================================================================== TLB Emulation

    Public Function TLB_ToPhysical(VAddr As Int32, ByRef RAMSpace As Int16) As Int32
        Dim I As Integer, i2 As Integer

        For I = 0 To PSMemory.Count - 1
            For i2 = 0 To PSMemory(I).TLB.Count - 1
                With PSMemory(I).TLB(i2)
                    If VAddr >= .StartAddr And VAddr < .StopAddr Then
                        RAMSpace = I
                        Return VAddr - .AddrFix
                    End If
                End With
            Next
        Next

        RAMSpace = -1
        Return -1
    End Function

    '========================================================================== Address Checks
    Public Function IsAddrAlign(memAddr As Double, bitAlign As Byte) As Boolean
        If (memAddr And ((bitAlign / 8) - 1)) = 0 Then Return True
        Return False
    End Function

    Public Function RevertMemAddress(MemAddr As Int32, RAMSpace As Int16) As Int32
        Select Case RAMSpace
            Case EE_Space
                Return MemAddr + &H80000
            Case KSEG0_Space
                Return MemAddr + &H80000000
            Case IOP_Space
                Return MemAddr + &HBC000000
        End Select
    End Function

    Public Function PatchMemIndex(mIndex As Int32, RamSpace As Int16) As Int32
        Select Case RamSpace
            Case EE_Space
                Return mIndex - &H80000
            Case KSEG0_Space
                Return mIndex
            Case IOP_Space
                Return mIndex
        End Select
    End Function

    Public Function PatchMemAddress(MemAddr As Int32, ByRef RAMSpace As Int16) As Int32

        'EE 0  = 00080000 - 01FFFFFF
        'EE 1  = 20080000 - 21FFFFFF (Mirror -> EE 0)
        'EE 2  = 30080000 - 31FFFFFF (Mirror -> EE 0)
        'KSEG0 = 80000000 - 8007FFFF
        'KSEG1 = A0000000 - A007FFFF (Mirror -> KSEG0)
        'IOP   = BC000000 - BC1FFFFF

        'Logical Address Range	Physical Address Range	Description			Size
        '---------------------	----------------------	-----------			-----
        '0x80000000-0x800FFFFF	0x00000000-0x000FFFFF	EE Kernel			  1 MB
        '0x00100000-0x01FFFFFF	0x00100000-0x01FFFFFF	EE RAM (Cached)			 31 MB
        '0x20100000-0x21FFFFFF	0x00100000-0x01FFFFFF	EE RAM (Uncached)		 31 MB
        '0x30100000-0x31FFFFFF	0x00100000-0x01FFFFFF	EE RAM (Uncached&accelerated)	 31 MB
        '0x10000000-0x11FFFFFF	0x10000000-0x11FFFFFF	EE Registers (uncached)		 32 MB
        '0x12000000-0x13FFFFFF	0x12000000-0x13FFFFFF	GS Registers (uncached)		 32 MB
        '0x1FC00000-0x1FFFFFFF	0x1FC00000-0x1FFFFFFF?	Boot ROM0 (uncached)		  4 MB
        '0x9FC00000-0x9FFFFFFF	0x1FC00000-0x1FFFFFFF?	Boot ROM09 (cached)		  4 MB
        '0xBFC00000-0xBFFFFFFF	0x1FC00000-0x1FFFFFFF?	Boot ROM0b (uncached)		  4 MB
        '0xBE000000-0xBE040000	0x1E000000-0x1E03FFFF?	Boot ROM1			256 KB
        '0xBE400000-0xBE440000	0x1E400000-0x1E43FFFF?	Boot ROM2			256 KB
        '0xBC000000-0xBC1FFFFF	0x1C000000-0x1C1FFFFF?	IOP RAM				  2 MB
        '0x70000000-0x70003FFF	----------------------	Scratch Pad			 16 KB

        If (MemAddr >= &H80000) And (MemAddr < &H2000000) Then
            RAMSpace = EE_Space
            Return MemAddr - &H80000
        End If
        If (MemAddr >= &H20080000) And (MemAddr < &H22000000) Then
            RAMSpace = EE_Space
            Return MemAddr - &H20080000
        End If
        If (MemAddr >= &H30080000) And (MemAddr < &H32000000) Then
            RAMSpace = EE_Space
            Return MemAddr - &H30080000
        End If

        If (MemAddr < &H80080000) And (MemAddr >= &H80000000) Then
            RAMSpace = KSEG0_Space
            Return MemAddr - &H80000000
        End If
        If (MemAddr < &HA0080000) And (MemAddr >= &HA0000000) Then
            RAMSpace = KSEG0_Space
            Return MemAddr - &HA0000000
        End If

        If (MemAddr < &HBC200000) And (MemAddr >= &HBC000000) Then
            RAMSpace = IOP_Space
            Return MemAddr - &HBC000000
        End If

        RAMSpace = -1
        Return -1
    End Function

End Module
