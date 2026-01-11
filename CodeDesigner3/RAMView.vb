Public Class RAMView
    '======================================================= Draw Buffer
    Private RenderBuffer() As String, RenderStart As Int64, RenderLines As Int64
    Private MarginWidth As Integer, GFX As Graphics, MemoryAddress As Int64
    Private MarginColor As Integer

    '======================================================= Settings
    Private FontName As String, FontSize As Integer, FontColor As Integer
    Private BgColor As Integer
    Private SVFont As Font, SVBrush As Brush

    '======================================================= Current Position
    Private CurLine As Int64, CurLineStr As String, LastLine As Int64
    Private SelectedLine As Int64, SelectedLineColor As Integer, CurSelLineCol As Integer

    '======================================================= Syntax Highlighting
    Private EnableSyntaxHL As Boolean, LineHLColor As Integer
    Private SxCOL() As SyntaxHighlight, HLFontColor As Integer
    Private Reg1Col As Integer, Reg2Col As Integer, Reg3Col As Integer
    Private TargetHLCol As Integer, DrawTarget As Boolean, TargetAddress As Int64

    '======================================================= Breakpoint Selection
    Private BreakOps() As Byte, BreakOpsMax As Byte, BreakOpsColors() As Integer

    '======================================================= Memory Info
    Private MemoryBytes() As Byte, MemoryWords() As UInt32, MemoryType As Integer, MemorySize As Int64
    Private mpAsm As MIPSAssembly, Mem32 As Word32Bit

    Private Structure SyntaxHighlight
        Dim sName As String
        Dim sColor As Integer
        Dim special As String
    End Structure

    Public Property Bullet(index As Int64) As Byte
        Get
            If BreakOps Is Nothing Then ReDim BreakOps(0)
            If index > BreakOps.Count - 1 Then ReDim Preserve BreakOps(index)
            Return BreakOps(index)
        End Get
        Set(newVal As Byte)
            If BreakOps Is Nothing Then ReDim BreakOps(0)
            If index > BreakOps.Count - 1 Then ReDim Preserve BreakOps(index)
            BreakOps(index) = newVal
            Me.Invalidate()
        End Set
    End Property

    Public Property BulletMargin() As Integer
        Get
            Return MarginWidth
        End Get
        Set(newWidth As Integer)
            MarginWidth = newWidth
            Me.Invalidate()
        End Set
    End Property

    Public Property SelLine() As Int64
        Get
            Return SelectedLine
        End Get
        Set(newSel As Int64)
            SelectedLine = newSel
        End Set
    End Property

    Public Property CursorPos() As Int64
        Get
            Return CurLine
        End Get
        Set(newPos As Int64)
            CurLine = newPos
            ReturnToCursor()
        End Set
    End Property


    Public Sub SetBreakOps(ByRef BOp() As Byte)
        ReDim BreakOps(BOp.Count - 1)

        Dim i As Int64
        For i = 0 To BreakOps.Count - 1
            BreakOps(i) = BOp(i)
        Next
        Me.Invalidate()
    End Sub

    Public Function GetBreakOps(ByRef BOp() As Byte) As Int64
        If BreakOps Is Nothing Then Return -1
        Dim i As Int64
        BOp = BreakOps
        Return BOp.Count - 1

        ReDim BOp(BreakOps.Count - 1)
        For i = 0 To BreakOps.Count - 1
            BOp(i) = BreakOps(i)
        Next
    End Function

    Public Sub SetBreakOpsColors(Max As Byte, Colors() As Integer)
        Dim i As Integer
        ReDim BreakOpsColors(Max)
        For i = 0 To Max
            BreakOpsColors(i) = Colors(i)
        Next
        BreakOpsMax = Max
    End Sub

    Public Sub GotoAddress(MemAddr As Int32)
        CurLine = MemAddr \ 4

        If CurLine > RenderStart + RenderLines Then RenderStart = CurLine

        ReturnToCursor()
    End Sub



    Public Sub InitMemory(Optional ByRef mBytes() As Byte = Nothing, Optional ByRef mWords() As UInt32 = Nothing, Optional MemType As Integer = 0, Optional ByRef Asm As MIPSAssembly = Nothing, Optional MemOffset As Int32 = 0)
        Select Case MemType
            Case 0
                MemoryType = 0
            Case 1 'Bytes
                MemoryBytes = mBytes
                MemoryType = 1
                MemorySize = mBytes.Count - 1
                mpAsm = Asm
                MemoryAddress = MemOffset
            Case 2 'Words
                MemoryWords = mWords
                MemoryType = 2
                MemorySize = mWords.Count - 1
                mpAsm = Asm
                MemoryAddress = MemOffset
        End Select
        Me.Invalidate()
    End Sub

    Public Sub SetMemoryOffset(MemOff As Int32)
        MemoryAddress = MemOff
    End Sub

    Public Sub ClearSyntaxColor()
        ReDim SxCOL(0)
    End Sub
    Public Sub SetSyntaxColor(StrArg As String, IntCol As Integer)
        Dim i As Integer

        If SxCOL Is Nothing Then
            ReDim SxCOL(0)
            SxCOL(0).sName = StrArg
            SxCOL(0).sColor = IntCol
            Exit Sub
        End If

        If SxCOL.Count - 1 = 0 Then
            ReDim Preserve SxCOL(1)
            SxCOL(1).sName = StrArg
            SxCOL(1).sColor = IntCol
            Exit Sub
        End If

        For i = 0 To SxCOL.Count - 1
            If LCase(StrArg) = LCase(SxCOL(i).sName) Then
                SxCOL(i).sColor = IntCol
                Exit Sub
            End If
        Next
        ReDim Preserve SxCOL(SxCOL.Count)
        SxCOL(SxCOL.Count - 1).sName = StrArg
        SxCOL(SxCOL.Count - 1).sColor = IntCol

    End Sub
    Private Function GetSyntaxColor(Str As String, ByRef ColOut As Integer) As Boolean
        Dim i As Integer
        If SxCOL Is Nothing Then Return False

        For i = 0 To SxCOL.Count - 1
            If LCase(Str) = LCase(SxCOL(i).sName) Then
                ColOut = SxCOL(i).sColor
                Return True
            End If
        Next

        Return False
    End Function

    Private Sub RAMView_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        If (Me.DesignMode) Then
            Exit Sub
        End If

        Me.DoubleBuffered = True

        EnableSyntaxHL = True

        GFX = Graphics.FromImage(New Bitmap(1, 1))

        FontName = "Courier New"
        FontSize = 10
        FontColor = Val("&HFF8c0064")
        BgColor = Val("&HFFa0d0ff")
        LineHLColor = Val("&HFF000090")
        HLFontColor = Val("&HFFffffff")
        MarginColor = Val("&HFF303030")
        TargetHLCol = Val("&HFFb0b000")
        SelectedLineColor = Val("&HFF808080")
        CurSelLineCol = Val("&HFF408080")

        Reg1Col = Val("&HFFff0000")
        Reg2Col = Val("&HFF0000ff")
        Reg3Col = Val("&HFFffff00")


        SVFont = New Font("Courier New", FontSize, FontStyle.Regular)
        SVBrush = New SolidBrush(Color.FromArgb(FontColor))

        MemoryAddress = 0
        MemorySize = -1
        MemoryType = 0
        RenderStart = 0
        RenderLines = (Me.Height \ SVFont.Height) - 1
        ReDim RenderBuffer(RenderLines)
        ReDim MemoryBytes(0)
        ReDim MemoryWords(0)

        CurLineStr = ""
        CurLine = 0

        SelectedLine = -1
    End Sub

    Private Sub ReturnToCursor()
        If CurLine > RenderStart + RenderLines Then RenderStart = CurLine - RenderLines
        If CurLine < RenderStart Then RenderStart = CurLine
        Me.Invalidate()
    End Sub

    Private Sub WriteRegCols()
        If CurLineStr = "" Then Exit Sub

        Dim sp() As String, tmp As String, i As Integer
        tmp = LCase(CurLineStr)
        tmp = Replace(tmp, ",", " ")
        tmp = Replace(tmp, "(", " ")
        tmp = Replace(tmp, ")", " ")
        tmp = Replace(tmp, ",", " ")
        tmp = stripSpaces(tmp)
        sp = Split(tmp + "      ", " ")

        ClearSyntaxColor()
        If (sp(1) <> "") Then
            If (sp(1) <> "zero") Then SetSyntaxColor(sp(1), Reg1Col)
        End If
        If (sp(2) <> "") Then
            If (sp(2) <> sp(1)) Then
                If (sp(2) <> "zero") Then SetSyntaxColor(sp(2), Reg2Col)
            End If
        End If
        If (sp(3) <> "") Then
            If (sp(3) <> sp(2)) Then
                If (sp(3) <> sp(1)) Then
                    If (sp(3) <> "zero") Then SetSyntaxColor(sp(3), Reg3Col)
                End If
            End If
        End If

    End Sub

    Private Function DoColorReg(Reg As String) As Boolean
        Dim sp() As String, tmp As String, i As Integer
        tmp = CurLineStr
        tmp = Replace(tmp, ",", " ")
        tmp = Replace(tmp, "(", " ")
        tmp = Replace(tmp, ")", " ")
        tmp = Replace(tmp, ",", " ")
        sp = Split(tmp + " ", " ")
        For i = 0 To sp.Count - 1
            If LCase(sp(i)) = LCase(Reg) Then Return True
        Next
        Return False
    End Function

    Private Function readParseChars(strIn As String, startI As Integer) As String
        Dim ret As String, I As Integer, rStop As Boolean

        If Len(strIn) <= 0 Then Return ""
        ret = ""

        I = startI
        Do
            rStop = True
            If I > Len(strIn) Then Return ret
            If Strings.Mid(strIn, I, 1) = " " Then rStop = False
            If Strings.Mid(strIn, I, 1) = "," Then rStop = False
            If Strings.Mid(strIn, I, 1) = ";" Then rStop = False
            If Strings.Mid(strIn, I, 1) = "(" Then rStop = False
            If Strings.Mid(strIn, I, 1) = ")" Then rStop = False
            If Strings.Mid(strIn, I, 1) = "/" Then rStop = False
            If Strings.Mid(strIn, I, 1) = "\" Then rStop = False
            If rStop = False Then
                ret += Strings.Mid(strIn, I, 1)
            End If
            I += 1
        Loop Until rStop = True

        Return ret
    End Function
    Private Function parseRead(strIn As String, startI As Integer) As String
        Dim ret As String, I As Integer, rStop As Boolean

        If Len(strIn) <= 0 Then Return ""
        ret = ""

        I = startI
        Do
            rStop = False
            If I > Len(strIn) Then Return ret
            If Strings.Mid(strIn, I, 1) = " " Then rStop = True
            If Strings.Mid(strIn, I, 1) = "," Then rStop = True
            If Strings.Mid(strIn, I, 1) = ";" Then rStop = True
            If Strings.Mid(strIn, I, 1) = "(" Then rStop = True
            If Strings.Mid(strIn, I, 1) = ")" Then rStop = True
            If Strings.Mid(strIn, I, 1) = "/" Then rStop = True
            If Strings.Mid(strIn, I, 1) = "\" Then rStop = True
            If rStop = False Then
                ret += Strings.Mid(strIn, I, 1)
            End If
            I += 1
        Loop Until rStop = True

        Return ret
    End Function
    Private Function parseSyntax(strIn As String) As String
        Dim ret As String, cmtStrip() As String

        If Len(strIn) <= 0 Then Return ""
        ret = strIn

        cmtStrip = Split(ret + "//", "//")
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
        If Len(strIn) <= 0 Then Return ""
        ret = strIn
        Do
            If ret = "" Then Return ""
            lastLen = ret.Length
            ret = Replace(ret, "  ", " ")
            If Strings.Left(ret, 1).Equals(" ") Then ret = Strings.Right(ret, ret.Length - 1)
            If Strings.Right(ret, 1).Equals(" ") Then ret = Strings.Left(ret, ret.Length - 1)
            If ret = "" Then Return ""
        Loop Until lastLen = ret.Length
        Return ret
    End Function
    Private Function HighlightRegs(str As String) As String
        Dim tStr As String, ret As String, i As Integer, colSet As Integer, rt As Boolean

        If EnableSyntaxHL = False Then Return str

        ret = ""
        i = 1
        Do
            tStr = parseRead(str, i)
            If Strings.Left(tStr, 1) = "$" And Len(tStr) > 4 Then
                ret += tStr '"{c#FFffffd0}{b}" + tStr + "{/c#}"
                i += Len(tStr)
            Else
                If tStr <> "" Then
                    If DoColorReg(tStr) Then
                        rt = GetSyntaxColor(tStr, colSet)
                        If rt Then
                            ret += "{c#" + Strings.Right("00000000" + Hex(colSet), 8) + "}{b}" + tStr + "{/c#}"
                            i += Len(tStr)
                        Else
                            ret += tStr
                            i += Len(tStr)
                        End If
                    Else
                        ret += tStr
                        i += Len(tStr)
                    End If
                Else
                    tStr = readParseChars(str, i)
                    If tStr <> "" Then
                        ret += tStr
                        i += Len(tStr)
                    End If
                End If
            End If

        Loop Until i > Len(str)

        Return ret

    End Function
    Private Function ReFormatJumps(Str As String, bufferAddr As Int64, ByRef Additionals As String) As String
        Dim disStr As String, target As Int64, tarDiff As Int64, sp() As String
        Dim myLineColor As Integer

        myLineColor = LineHLColor
        If (CurLine * 4) = bufferAddr Then myLineColor = HLFontColor

        disStr = Str
        Additionals = ""
        sp = Split(LCase(parseSyntax(disStr)) + "    ", " ")
        Select Case sp(0)
            Case "j"
                target = Val(Replace(sp(1), "$", "&h0"))
                tarDiff = target - bufferAddr
                disStr = Strings.Left(disStr, Len(disStr) - Len(sp(1)))
                Additionals += "$" + LCase(Strings.Right("00000000" + Hex(target), 8))
                Additionals += " {c#" + Strings.Right("00000000" + Hex(myLineColor), 8) + "}{b}("
                If tarDiff >= 0 Then Additionals += "+" + (tarDiff \ 4).ToString + ") v"
                If tarDiff < 0 Then Additionals += (tarDiff \ 4).ToString + ") ^"
                Additionals += "{/c#}"
            Case "jal"
                target = Val(Replace(sp(1), "$", "&h0"))
                tarDiff = target - bufferAddr
                disStr = Strings.Left(disStr, Len(disStr) - Len(sp(1)))
                Additionals += "$" + LCase(Strings.Right("00000000" + Hex(target), 8))
                Additionals += " {c#" + Strings.Right("00000000" + Hex(myLineColor), 8) + "}{b}("
                If tarDiff >= 0 Then Additionals += "+" + (tarDiff \ 4).ToString + ") v"
                If tarDiff < 0 Then Additionals += (tarDiff \ 4).ToString + ") ^"
                Additionals += "{/c#}"
            Case Else
                If Strings.Left(sp(0), 1) = "b" Then
                    If Strings.Left(sp(1), 1) = "$" Then
                        target = Val(Replace(sp(1), "$", "&h0"))
                        disStr = Strings.Left(disStr, Len(disStr) - Len(sp(1)))
                    ElseIf Strings.Left(sp(2), 1) = "$" Then
                        target = Val(Replace(sp(2), "$", "&h0"))
                        disStr = Strings.Left(disStr, Len(disStr) - Len(sp(2)))
                    ElseIf Strings.Left(sp(3), 1) = "$" Then
                        target = Val(Replace(sp(3), "$", "&h0"))
                        disStr = Strings.Left(disStr, Len(disStr) - Len(sp(3)))
                    Else
                        Return disStr
                    End If

                    If target > &H7FFF Then target = 0 - (&H10000 - target)
                    target = bufferAddr + (target * 4) + 4

                    tarDiff = target - bufferAddr

                    Additionals += "$" + LCase(Strings.Right("00000000" + Hex(target), 8))
                    Additionals += " {c#" + Strings.Right("00000000" + Hex(myLineColor), 8) + "}{b}("
                    If tarDiff >= 0 Then Additionals += "+" + (tarDiff \ 4).ToString + ") v"
                    If tarDiff < 0 Then Additionals += (tarDiff \ 4).ToString + ") ^"
                    Additionals += "{/c#}"
                End If
        End Select
        If sp(0) <> "" Then
            disStr = Strings.Right(disStr, Len(disStr) - Len(sp(0)))
            disStr = Strings.Left(sp(0) + "          ", 7) + disStr
        End If
        Return disStr
    End Function
    Private Sub LoadRenderBuffer()
        Dim I As Integer, bufferAddr As Int64, displayAddr As String, displayData As String
        Dim disStr As String, Incr As Integer, target As Int64, tarDiff As Int64, sp() As String
        Dim maxDiff As Integer, preStr As String, postStr As String

        RenderLines = (Me.Height \ SVFont.Height) - 1

        ReDim RenderBuffer(RenderLines)

        bufferAddr = MemoryAddress + (RenderStart * 4)
        If MemoryType = 1 Then Incr = 4
        If MemoryType = 2 Then Incr = 1

        If CurLine * Incr < MemorySize Then
            Select Case MemoryType
                Case 1
                    Mem32.u8_1 = MemoryBytes((CurLine * 4) + 0)
                    Mem32.u8_2 = MemoryBytes((CurLine * 4) + 1)
                    Mem32.u8_3 = MemoryBytes((CurLine * 4) + 2)
                    Mem32.u8_4 = MemoryBytes((CurLine * 4) + 3)
                Case 2
                    Mem32.u32 = MemoryWords(CurLine)
            End Select
            CurLineStr = LCase(mpAsm.DisassembleValue(Mem32.u32))
            If Strings.Left(CurLineStr, 7) = "hexcode" Then CurLineStr = ""

        Else
            CurLineStr = ""
        End If

        DrawTarget = False
        maxDiff = 255
        sp = Split(parseSyntax(CurLineStr) + "      ", " ")
        Select Case sp(0)
            Case "j"
                TargetAddress = Val(Replace(sp(1), "$", "&h0"))
                If CurLine * 4 > target Then tarDiff = (CurLine * 4) - target
                If CurLine * 4 < target Then tarDiff = target - (CurLine * 4)
                If CurLine * 4 = target Then tarDiff = 0
                If tarDiff < maxDiff Then DrawTarget = True
            Case "jal"
                TargetAddress = Val(Replace(sp(1), "$", "&h0"))
                If CurLine * 4 > target Then tarDiff = (CurLine * 4) - target
                If CurLine * 4 < target Then tarDiff = target - (CurLine * 4)
                If CurLine * 4 = target Then tarDiff = 0
                If tarDiff < maxDiff Then DrawTarget = True
            Case Else
                If Strings.Left(sp(0), 1) = "b" Then
                    If Strings.Left(sp(1), 1) = "$" Then target = Val(Replace(sp(1), "$", "&h0"))
                    If Strings.Left(sp(2), 1) = "$" Then target = Val(Replace(sp(2), "$", "&h0"))
                    If Strings.Left(sp(3), 1) = "$" Then target = Val(Replace(sp(3), "$", "&h0"))
                    target += 1
                    If target > &H7FFF Then target = 0 - (&H10000 - target)
                    tarDiff = Math.Abs(target)
                    TargetAddress = (CurLine * 4) + (target * 4)
                    If tarDiff < maxDiff Then DrawTarget = True
                End If
        End Select



        WriteRegCols()

        For I = 0 To RenderLines
            displayAddr = LCase(Strings.Right("00000000" + Hex(bufferAddr), 8))
            preStr = " "
            If DrawTarget And bufferAddr = TargetAddress Then preStr = ">"

            Mem32.u32 = 0
            RenderBuffer(I) = ""
            If MemoryType > 0 Then
                If (RenderStart * Incr) + (I * Incr) < MemorySize Then

                    Select Case MemoryType
                        Case 1
                            Mem32.u8_1 = MemoryBytes((RenderStart * 4) + (I * 4) + 0)
                            Mem32.u8_2 = MemoryBytes((RenderStart * 4) + (I * 4) + 1)
                            Mem32.u8_3 = MemoryBytes((RenderStart * 4) + (I * 4) + 2)
                            Mem32.u8_4 = MemoryBytes((RenderStart * 4) + (I * 4) + 3)
                        Case 2
                            Mem32.u32 = MemoryWords(RenderStart + I)
                    End Select
                    displayData = LCase(Strings.Right("00000000" + Hex(Mem32.u32), 8))

                    If RenderStart + I = CurLine Or RenderStart + I = SelLine Then
                        'Memory Address + Memory Data
                        RenderBuffer(I) = "{c#" + Strings.Right("00000000" + Hex(HLFontColor), 8) + "}" + preStr + displayAddr + " " + displayData

                        'Disassembly
                        disStr = LCase(mpAsm.DisassembleValue(Mem32.u32))
                        disStr = ReFormatJumps(disStr, bufferAddr, postStr)
                        If Strings.Left(disStr, 7) <> "hexcode" Then RenderBuffer(I) += "    " + disStr + postStr

                        RenderBuffer(I) += "{/c#}"
                    Else
                        If EnableSyntaxHL Then
                            'Memory Address + Memory Data
                            RenderBuffer(I) = "{c#" + Strings.Right("00000000" + Hex(FontColor), 8) + "}" + preStr + displayAddr + " " + displayData + "{/c#}"

                            'Disassembly
                            disStr = LCase(mpAsm.DisassembleValue(Mem32.u32))
                            disStr = ReFormatJumps(disStr, bufferAddr, postStr)
                            If Strings.Left(disStr, 7) <> "hexcode" Then RenderBuffer(I) += "    " + HighlightRegs(disStr) + postStr

                        Else
                            'Memory Address
                            RenderBuffer(I) = "{c#" + Strings.Right("00000000" + Hex(FontColor), 8) + "}" + preStr + displayAddr + " " + displayData + "{/c#}"

                            'Disassembly
                            disStr = LCase(mpAsm.DisassembleValue(Mem32.u32))
                            disStr = ReFormatJumps(disStr, bufferAddr, postStr)
                            If Strings.Left(disStr, 7) <> "hexcode" Then RenderBuffer(I) += "    " + disStr + postStr
                        End If
                    End If
                Else
                    If RenderStart + I = CurLine Or RenderStart + I = SelLine Then
                        RenderBuffer(I) = "{c#" + Strings.Right("00000000" + Hex(HLFontColor), 8) + "}" + displayAddr + " {/c#}"
                    Else
                        RenderBuffer(I) = "{c#" + Strings.Right("00000000" + Hex(FontColor), 8) + "}" + displayAddr + " {/c#}"
                    End If
                End If
            Else
                If RenderStart + I = CurLine Or RenderStart + I = SelLine Then
                    RenderBuffer(I) = "{c#" + Strings.Right("00000000" + Hex(HLFontColor), 8) + "}" + displayAddr + " {/c#}"
                Else
                    RenderBuffer(I) = "{c#" + Strings.Right("00000000" + Hex(FontColor), 8) + "}" + displayAddr + " {/c#}"
                End If
            End If

            bufferAddr += 4
        Next
    End Sub

    Private Sub DrawRenderBuffer(ByRef e As PaintEventArgs)
        Dim i As Integer, i2 As Integer, XY As Point, renderStr As String
        Dim RCT As Rectangle, HLWidth As Integer, displayedStr As String
        Dim Tri(2) As PointF

        If BreakOps Is Nothing Then ReDim BreakOps(0)

        If MemoryType > 0 Then
            If BreakOps.Count - 1 < MemorySize Then
                If MemoryType = 1 And BreakOps.Count - 1 < MemorySize \ 4 Then ReDim Preserve BreakOps(MemorySize \ 4)
                If MemoryType = 2 And BreakOps.Count - 1 < MemorySize Then ReDim Preserve BreakOps(MemorySize)
            End If
        End If

        'MarginWidth = StrWidth("__")
        'Draw Margin

        RCT.X = 0
        RCT.Y = 0
        RCT.Width = MarginWidth
        RCT.Height = Me.Height
        e.Graphics.FillRectangle(New SolidBrush(Color.FromArgb(MarginColor)), RCT)

        HLWidth = 8

        If DrawTarget Then
            If TargetAddress >= (CurLine * 4) Then
                RCT.X = MarginWidth
                RCT.Height = ((TargetAddress - (CurLine * 4)) \ 4) * SVFont.Height
                RCT.Width = StrWidth(" 00000000")
                RCT.Y = (((TargetAddress \ 4) - RenderStart) * SVFont.Height) - RCT.Height
                e.Graphics.FillRectangle(New SolidBrush(Color.FromArgb(TargetHLCol)), RCT)
            Else
                RCT.X = MarginWidth
                RCT.Height = (((CurLine * 4) - TargetAddress) \ 4) * SVFont.Height
                RCT.Width = StrWidth(" 00000000")
                RCT.Y = (((CurLine - RenderStart) * SVFont.Height) - RCT.Height) + SVFont.Height
                RCT.Height += SVFont.Height
                e.Graphics.FillRectangle(New SolidBrush(Color.FromArgb(TargetHLCol)), RCT)
            End If
        End If



        XY.X = MarginWidth
        XY.Y = 0
        For i = 0 To RenderLines

            If CurLine <> SelectedLine Then
                If RenderStart + i = CurLine Then
                    RCT.X = 0
                    RCT.Y = XY.Y
                    RCT.Width = Me.Width
                    RCT.Height = SVFont.Height
                    e.Graphics.FillRectangle(New SolidBrush(Color.FromArgb(LineHLColor)), RCT)
                End If
                If RenderStart + i = SelectedLine Then
                    RCT.X = 0
                    RCT.Y = XY.Y
                    RCT.Width = Me.Width
                    RCT.Height = SVFont.Height
                    e.Graphics.FillRectangle(New SolidBrush(Color.FromArgb(SelectedLineColor)), RCT)
                End If
            Else
                If RenderStart + i = CurLine Then
                    RCT.X = 0
                    RCT.Y = XY.Y
                    RCT.Width = Me.Width
                    RCT.Height = SVFont.Height
                    e.Graphics.FillRectangle(New SolidBrush(Color.FromArgb(CurSelLineCol)), RCT)
                End If
            End If

            If RenderBuffer(i) <> "" Then
                i2 = 1
                renderStr = ""
                displayedStr = ""
                Do
                    If Mid(RenderBuffer(i), i2, 3) = "{c#" Then
                        If renderStr <> "" Then
                            displayedStr += renderStr
                            e.Graphics.DrawString(renderStr, SVFont, SVBrush, XY)
                            XY.X += StrWidth(renderStr)
                            renderStr = ""
                        End If
                        i2 += 3
                        SVBrush.Dispose()
                        SVFont.Dispose()
                        SVBrush = New SolidBrush(Color.FromArgb(Val("&H" + Mid(RenderBuffer(i), i2, 8))))
                        i2 += 9
                        SVFont = New Font(FontName, FontSize, FontStyle.Regular)
                    End If

                    If Mid(RenderBuffer(i), i2, 3) = "{b}" Then
                        SVFont.Dispose()
                        SVFont = New Font(FontName, FontSize, FontStyle.Bold)
                        i2 += 3
                    End If
                    If Mid(RenderBuffer(i), i2, 3) = "{i}" Then
                        SVFont.Dispose()
                        SVFont = New Font(FontName, FontSize, FontStyle.Italic)
                        i2 += 3
                    End If
                    If Mid(RenderBuffer(i), i2, 3) = "{u}" Then
                        SVFont.Dispose()
                        SVFont = New Font(FontName, FontSize, FontStyle.Underline)
                        i2 += 3
                    End If
                    If Mid(RenderBuffer(i), i2, 5) = "{/c#}" Then
                        displayedStr += renderStr
                        e.Graphics.DrawString(renderStr, SVFont, SVBrush, XY)
                        XY.X += StrWidth(renderStr)
                        i2 += 5
                        renderStr = ""
                        SVBrush.Dispose()
                        SVFont.Dispose()
                        SVBrush = New SolidBrush(Color.FromArgb(FontColor))
                        SVFont = New Font(FontName, FontSize, FontStyle.Regular)
                    End If

                    If i2 <= Len(RenderBuffer(i)) Then
                        If Mid(RenderBuffer(i), i2, 1) <> "{" Then
                            renderStr += Mid(RenderBuffer(i), i2, 1)
                            i2 += 1
                        End If
                    End If
                Loop Until i2 > Len(RenderBuffer(i))
                If renderStr <> "" Then
                    SVBrush.Dispose()
                    SVFont.Dispose()
                    SVBrush = New SolidBrush(Color.FromArgb(FontColor))
                    SVFont = New Font(FontName, FontSize, FontStyle.Regular)
                    displayedStr += renderStr
                    e.Graphics.DrawString(renderStr, SVFont, SVBrush, XY)
                    renderStr = ""
                End If
            End If

            If Strings.Right(displayedStr, 1) = "v" Then
                displayedStr = Strings.Left(displayedStr, Len(displayedStr) - 1)
                Tri(0).X = (StrWidth(displayedStr) + MarginWidth) - 1
                Tri(0).Y = XY.Y
                Tri(1).X = Tri(0).X + (StrWidth("__") \ 2)
                Tri(1).Y = XY.Y + SVFont.Height
                Tri(2).X = Tri(0).X + StrWidth("__")
                Tri(2).Y = XY.Y
                If RenderStart + i = CurLine Then
                    e.Graphics.FillPolygon(New SolidBrush(Color.FromArgb(HLFontColor)), Tri, Drawing2D.FillMode.Alternate)
                Else
                    e.Graphics.FillPolygon(New SolidBrush(Color.FromArgb(LineHLColor)), Tri, Drawing2D.FillMode.Alternate)
                End If

            ElseIf Strings.Right(displayedStr, 1) = "^" Then
                displayedStr = Strings.Left(displayedStr, Len(displayedStr) - 1)

                Tri(0).X = (StrWidth(displayedStr) + MarginWidth) - 1
                Tri(0).Y = XY.Y + SVFont.Height
                Tri(1).X = Tri(0).X + (StrWidth("__") \ 2)
                Tri(1).Y = XY.Y
                Tri(2).X = Tri(0).X + StrWidth("__")
                Tri(2).Y = XY.Y + SVFont.Height
                If RenderStart + i = CurLine Then
                    e.Graphics.FillPolygon(New SolidBrush(Color.FromArgb(HLFontColor)), Tri, Drawing2D.FillMode.Alternate)
                Else
                    e.Graphics.FillPolygon(New SolidBrush(Color.FromArgb(LineHLColor)), Tri, Drawing2D.FillMode.Alternate)
                End If
            End If
            If Strings.Left(displayedStr, 1) = ">" Then
                Tri(0).X = MarginWidth
                Tri(0).Y = XY.Y
                Tri(1).X = Tri(0).X + StrWidth("_")
                Tri(1).Y = XY.Y + (SVFont.Height \ 2)
                Tri(2).X = Tri(0).X
                Tri(2).Y = XY.Y + SVFont.Height

                e.Graphics.FillPolygon(New SolidBrush(Color.FromArgb(FontColor)), Tri, Drawing2D.FillMode.Alternate)

            End If

            If BreakOps.Count - 1 >= RenderStart + i Then
                If BreakOps(RenderStart + i) > 0 Then
                    RCT.Width = MarginWidth  'StrWidth("_")
                    RCT.Height = SVFont.Height \ 2 'RCT.Width
                    RCT.X = 0
                    RCT.Y = XY.Y + (SVFont.Height \ 4)
                    If BreakOps(RenderStart + i) <= BreakOpsMax Then
                        e.Graphics.FillRectangle(New SolidBrush(Color.FromArgb(BreakOpsColors(BreakOps(RenderStart + i)))), RCT)
                    End If
                End If
            End If
            XY.X = MarginWidth
            XY.Y += SVFont.Height
        Next

    End Sub
    Private Sub ClearScreen(ByRef e As PaintEventArgs)
        e.Graphics.Clear(Color.FromArgb(BgColor))
    End Sub
    Private Sub SyntaxView_Paint(sender As Object, e As PaintEventArgs) Handles Me.Paint
        'On Error Resume Next

        ClearScreen(e)
        LoadRenderBuffer()
        DrawRenderBuffer(e)

        e.Dispose()
        GC.Collect()

        'Err.Clear() 'Stupid random overflow exception
    End Sub
    Private Function StrWidth(Str As String) As Integer
        Dim SzF As SizeF, SzF2 As SizeF

        SzF = GFX.MeasureString(Replace(Replace(Str, " ", "_"), vbTab, "    ") + "I", SVFont)
        SzF2 = GFX.MeasureString("I", SVFont)
        Return SzF.Width - SzF2.Width
    End Function

    Private Sub RAMView_MouseDown(sender As Object, e As MouseEventArgs) Handles Me.MouseDown
        Dim YSel As Integer
        YSel = (e.Y \ SVFont.Height) + RenderStart
        CurLine = YSel

        Me.Invalidate()
    End Sub

    Private Sub RAMView_MouseWheel(sender As Object, e As MouseEventArgs) Handles Me.MouseWheel
        If e.Delta > 0 Then
            If RenderStart > 3 Then
                RenderStart -= 3
                Me.Invalidate()
            Else
                RenderStart = 0
                Me.Invalidate()
            End If
        ElseIf e.Delta < 0 Then
            If RenderStart + RenderLines < 4294967292 Then
                RenderStart += 3
                Me.Invalidate()
            End If
        End If
    End Sub

    Private Sub RAMView_MouseMove(sender As Object, e As MouseEventArgs) Handles Me.MouseMove
        If e.X <= MarginWidth Then
            Me.Cursor = Cursors.Hand
        Else
            Me.Cursor = Cursors.Default
        End If
    End Sub

    Private Sub RAMView_MouseUp(sender As Object, e As MouseEventArgs) Handles Me.MouseUp
        Dim YSel As Integer
        YSel = (e.Y \ SVFont.Height) + RenderStart

        If e.X < MarginWidth Then
            If BreakOps Is Nothing Then ReDim BreakOps(YSel)
            If BreakOps.Count - 1 < YSel Then ReDim Preserve BreakOps(YSel)
            BreakOps(YSel) += 1
            If BreakOps(YSel) > BreakOpsMax Then BreakOps(YSel) = 0
        End If

        Me.Invalidate()
    End Sub

    Protected Overrides Function ProcessCmdKey(ByRef msg As System.Windows.Forms.Message, keyData As System.Windows.Forms.Keys) As Boolean
        Select Case keyData
            Case Keys.Tab
                Me.Invalidate()
                Return True
            Case Keys.Up
                If CurLine > 0 Then CurLine -= 1
                ReturnToCursor()
                Return True
            Case Keys.Down
                If CurLine < 4294967295 Then CurLine += 1
                ReturnToCursor()
                Return True
            Case Keys.Left
                CurLine = LastLine
                ReturnToCursor()
                Return True
            Case Keys.Right
                If Strings.Left(CurLineStr, 1) = "b" Or Strings.Left(CurLineStr, 1) = "j" Then
                    LastLine = CurLine
                    GotoAddress(TargetAddress)
                    ReturnToCursor()
                End If
                Return True
        End Select
        Return MyBase.ProcessCmdKey(msg, keyData)
    End Function

    Private Sub RAMView_KeyUp(sender As Object, e As KeyEventArgs) Handles Me.KeyUp
        If e.KeyCode = Keys.Space Then
            If SelectedLine = CurLine Then
                SelectedLine = -1
            Else
                SelectedLine = CurLine
            End If
            ReturnToCursor()
        End If
    End Sub

    Private Sub RAMView_KeyDown(sender As Object, e As KeyEventArgs) Handles Me.KeyDown
        Select Case e.KeyCode
            Case Keys.PageUp
                If CurLine - RenderLines > 0 Then
                    CurLine -= RenderLines
                Else
                    CurLine = 0
                End If
                ReturnToCursor()
            Case Keys.PageDown
                If CurLine + RenderLines < ((4294967295 \ 4) - RenderLines) Then
                    CurLine += RenderLines
                Else
                    CurLine = 4294967295 \ 4
                End If
                ReturnToCursor()
        End Select
    End Sub
End Class
