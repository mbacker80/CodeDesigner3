Public Class ELF

    Private Header As ElfHeader, ProgHeaders() As ProgramHeader

    Private Structure ElfHeader
        Dim Identity() As Byte '16 bytes
        Dim Type As UInt16
        Dim Machine As UInt16
        Dim Version As UInt32
        Dim Entry As UInt32
        Dim ProgramHeaderOffset As UInt32
        Dim SectionHeaderOffset As UInt32
        Dim Flags As UInt32
        Dim ElfHeaderSize As UInt16
        Dim ProgramHeaderEntrySize As UInt16
        Dim ProgramHeaderCount As UInt16
        Dim SectionHeaderEntrySize As UInt16
        Dim SectionHeaderCount As UInt16
        Dim SectionHeaderStringIndex As UInt16
    End Structure

    Private Structure ProgramHeader
        Dim Type As UInt32
        Dim FileOffset As UInt32
        Dim VirtualAddress As UInt32
        Dim PhysicalAddress As UInt32
        Dim FileSize As UInt32
        Dim MemorySize As UInt32
        Dim Flags As UInt32
        Dim Align As UInt32

        Dim FileData() As Byte 'Not actually part of the header, fyi
    End Structure

    Public Delegate Sub DebugOutput(Str As String)
    Private debugOut As DebugOutput

    Public Sub SetEntry(Entry As UInt32)
        Header.Entry = Entry
    End Sub

    Public Sub Init(ByRef dbgFunc As DebugOutput)
        With Header
            .Identity = {&H7F, &H45, &H4C, &H46, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0}
            .Type = 2
            .Machine = 8
            .Version = 1
            .Entry = 0
            .Flags = 0
            .ElfHeaderSize = &H34

            .ProgramHeaderOffset = .ElfHeaderSize
            .ProgramHeaderCount = 0
            .ProgramHeaderEntrySize = &H20

            .SectionHeaderOffset = 0
            .SectionHeaderCount = 0
            .SectionHeaderEntrySize = 0
            .SectionHeaderStringIndex = 0

            ReDim ProgHeaders(.ProgramHeaderCount)
        End With

        debugOut = dbgFunc
    End Sub

    Public Sub AddFile(Addr As UInt32, fData() As Byte)
        ReDim Preserve ProgHeaders(Header.ProgramHeaderCount)
        Header.ProgramHeaderCount += 1
        With ProgHeaders(Header.ProgramHeaderCount - 1)
            .Align = &H10
            .FileSize = fData.Length - 1

            ReDim .FileData(.FileSize)
            For i As Integer = 0 To .FileSize
                .FileData(i) = fData(i)
            Next

            .Flags = 0
            .MemorySize = .FileSize
            .Type = 1
            .VirtualAddress = Addr
            .PhysicalAddress = Addr
            .FileOffset = 0
        End With
    End Sub

    Public Sub AppendCode(Code As String)
        Dim Lines() As String, sp() As String, I As Integer
        Dim LastAddr As UInt32, NextAddr As UInt32, SetFirst As Boolean
        Dim FileData() As Byte, FileSize As UInt32, Word() As Byte
        Dim Offset As UInt32

        ReDim FileData(0)
        FileSize = 0
        SetFirst = False
        Lines = Split(Code + vbCrLf, vbCrLf)
        For I = 0 To Lines.Count - 1
            If Lines(I) <> "" Then
                sp = Split(Lines(I) + " ", " ")
                If sp(0) <> "" Then
                    If SetFirst = False Then
                        LastAddr = CDec("&H" + Strings.Right(sp(0), 7)) - 4
                        Offset = LastAddr + 4
                        SetFirst = True
                    End If
                    NextAddr = CDec("&H" + Strings.Right(sp(0), 7))

                    If NextAddr = LastAddr + 4 Then
                        FileSize += 4
                        ReDim Preserve FileData(FileSize)
                    Else
                        AddFile(Offset, FileData)
                        FileSize = 4
                        ReDim FileData(FileSize)
                        Offset = NextAddr
                    End If
                    FileData(FileSize - 1) = CDec("&H" + Strings.Mid(sp(1), 1, 2))
                    FileData(FileSize - 2) = CDec("&H" + Strings.Mid(sp(1), 3, 2))
                    FileData(FileSize - 3) = CDec("&H" + Strings.Mid(sp(1), 5, 2))
                    FileData(FileSize - 4) = CDec("&H" + Strings.Mid(sp(1), 7, 2))
                    LastAddr = NextAddr
                End If
            End If
        Next
        If FileSize > 0 Then AddFile(Offset, FileData)
    End Sub

    Private Sub UpdateHeaders()
        Dim I As Integer, Offset As UInt32

        Offset = (Header.ProgramHeaderCount * Header.ProgramHeaderEntrySize) + Header.ElfHeaderSize
        Offset += &H40
        While ((Offset And 15) <> 0)
            Offset += 4
        End While

        For I = 0 To Header.ProgramHeaderCount - 1
            ProgHeaders(I).FileOffset = Offset
            Offset += ProgHeaders(I).FileSize + 1 + &H10
            While ((Offset And 15) <> 0)
                Offset += 1
            End While
        Next
    End Sub

    Public Function SaveELF(fPath As String) As Boolean
        Dim BW As System.IO.BinaryWriter

        UpdateHeaders()
        Try
            If System.IO.File.Exists(fPath) Then System.IO.File.Delete(fPath)
            BW = New System.IO.BinaryWriter(System.IO.File.Open(fPath, System.IO.FileMode.OpenOrCreate))

            With Header
                BW.Write(.Identity)
                BW.Write(.Type)
                BW.Write(.Machine)
                BW.Write(.Version)
                BW.Write(.Entry)
                BW.Write(.ProgramHeaderOffset)
                BW.Write(.SectionHeaderOffset)
                BW.Write(.Flags)
                BW.Write(.ElfHeaderSize)
                BW.Write(.ProgramHeaderEntrySize)
                BW.Write(.ProgramHeaderCount)
                BW.Write(.SectionHeaderEntrySize)
                BW.Write(.SectionHeaderCount)
                BW.Write(.SectionHeaderStringIndex)
            End With

            Dim i As Integer, Offset As UInt32, FileStart As UInt32
            Offset = Header.ElfHeaderSize
            For i = 0 To Header.ProgramHeaderCount - 1
                With ProgHeaders(i)
                    BW.Write(.Type)
                    BW.Write(.FileOffset)
                    BW.Write(.VirtualAddress)
                    BW.Write(.PhysicalAddress)
                    BW.Write(.FileSize)
                    BW.Write(.MemorySize)
                    BW.Write(.Flags)
                    BW.Write(.Align)
                End With
                Offset += Header.ProgramHeaderEntrySize
            Next
            FileStart = Offset + &H40
            While ((FileStart And 15) <> 0)
                FileStart += 4
            End While

            While (Offset < FileStart)
                BW.Write(Convert.ToUInt32(0))
                Offset += 4
            End While

            For i = 0 To Header.ProgramHeaderCount - 1
                BW.Write(ProgHeaders(i).FileData)
                Offset += (ProgHeaders(i).FileSize + 1)
                FileStart = Offset + &H10
                While ((FileStart And 15) <> 0)
                    FileStart += 1
                End While
                While (Offset < FileStart)
                    BW.Write(Convert.ToByte(0))
                    Offset += 1
                End While
            Next

            BW.Close()
        Catch

            debugOut("File I/O Error '" + fPath + "'")
            Return False
        End Try

        Return True
    End Function

End Class
