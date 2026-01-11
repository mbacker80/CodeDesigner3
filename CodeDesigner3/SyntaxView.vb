Public Class SyntaxView
    '======================================================= Draw Buffer
    Private RenderBuffer() As String, RenderStart As UInt64, RenderLines As UInt64
    Private MarginWidth As Integer, GFX As Graphics

    '======================================================= Blinker
    Private BlinkerONOFF As Boolean, BlinkerPEA As PaintEventArgs
    Private BlinkerXY As Point

    '======================================================= Settings
    Private FontName As String, FontSize As Integer, FontColor As Integer
    Private BgColor As Integer
    Private SVFont As Font, SVBrush As Brush

    '======================================================= Full Document
    Private AllLines() As String, LineCount As UInt64
    Private WasInitialized As Boolean

    '======================================================= Selection
    Private SelLineStart As UInt64, SelLineStop As UInt64
    Private SelStart As UInt64, SelStop As UInt64
    Private IsSelecting As Boolean, SelectionHLColor As Int32

    '======================================================= Current Position
    Private CurLine As UInt64, CurPos As UInt64

    '======================================================= Syntax Highlighting
    Private EnableSyntaxHL As Boolean, EnableLineHL As Boolean, LineHLColor As Integer
    Private SxCMD() As SyntaxHighlight, SxARG() As SyntaxHighlight
    Private MinNameLen As Integer

    '======================================================= Breakpoint Selection
    Private BreakOps() As Byte


    Private Structure SyntaxHighlight
        Dim sName As String
        Dim sColor As Integer
        Dim special As String
    End Structure


    Public Property TxtSource() As String
        Get
            Return Join(AllLines, vbCrLf)
        End Get
        Set(txt As String)
            IsSelecting = False
            AllLines = Split(txt + vbCrLf, vbCrLf)
            ReDim Preserve AllLines(AllLines.Count - 2)
            CurLine = 0
            CurPos = 0
            ReturnToCaret()
            ResizeScrollbars()
            Me.Invalidate()
        End Set
    End Property

    Public Property AllowSyntaxing() As Boolean
        Get
            Return EnableSyntaxHL
        End Get
        Set(value As Boolean)
            EnableSyntaxHL = value
            Me.Invalidate()
        End Set
    End Property

    Public Property HighlightCurrentLine() As Boolean
        Get
            Return EnableLineHL
        End Get
        Set(value As Boolean)
            EnableLineHL = value
            Me.Invalidate()
        End Set
    End Property

    'BgColor = Val("&HFF111111")
    Public Property Back_Color() As Integer
        Get
            Return BgColor
        End Get
        Set(newCol As Integer)
            BgColor = newCol
            Me.Invalidate()
        End Set
    End Property

    'FontName = "Courier New"
    Public Property Font_Name() As String
        Get
            Return FontName
        End Get
        Set(newFont As String)
            FontName = newFont
            Me.Invalidate()
        End Set
    End Property

    'FontSize = 12
    Public Property Font_Size() As Integer
        Get
            Return FontSize
        End Get
        Set(newSize As Integer)
            FontSize = newSize
            Me.Invalidate()
        End Set
    End Property

    'FontColor = Val("&HFFffffff")
    Public Property Font_Color() As Integer
        Get
            Return FontColor
        End Get
        Set(newColor As Integer)
            FontColor = newColor
            Me.Invalidate()
        End Set
    End Property

    'LineHLColor = Val("&HFF222222")
    Public Property CurrentLineHLColor() As Integer
        Get
            Return LineHLColor
        End Get
        Set(newColor As Integer)
            LineHLColor = newColor
            Me.Invalidate()
        End Set
    End Property

    'SelectionHLColor = Val("&HFF6060C0")
    Public Property SelectionHighlightColor() As Integer
        Get
            Return SelectionHLColor
        End Get
        Set(newColor As Integer)
            SelectionHLColor = newColor
            Me.Invalidate()
        End Set
    End Property







    Public Sub GotoLine(LineNum As Int64)
        If LineNum > AllLines.Count - 1 Then Exit Sub
        CurLine = LineNum
        CurPos = 0
        ReturnToCaret()
        Me.Invalidate()
    End Sub

    Public Sub HighlightLine(LineNum As Int64)
        If LineNum > AllLines.Count - 1 Then Exit Sub
        CurLine = LineNum
        CurPos = 0
        SelLineStart = CurLine
        SelLineStop = CurLine
        SelStart = 0
        SelStop = Len(AllLines(LineNum))
        IsSelecting = True
        ReturnToCaret()
        Me.Invalidate()
    End Sub

    Public Sub Delete()
        If IsSelecting Then DeleteSelection()
        Me.Invalidate()
    End Sub
    Public Sub Paste()
        If IsSelecting Then DeleteSelection()
        InsertText(Clipboard.GetText, CurLine, CurPos)
        Me.Invalidate()
    End Sub
    Public Sub Cut()
        If IsSelecting Then
            Clipboard.SetText(CopySelection())
            DeleteSelection()
            Me.Invalidate()
        End If
    End Sub
    Public Sub Copy()
        If IsSelecting Then
            Clipboard.SetText(CopySelection())
        End If
    End Sub
    Public Sub SelectAll()
        IsSelecting = True
        SelStart = 0
        SelLineStart = 0
        SelLineStop = AllLines.Count - 1
        SelStop = Len(AllLines(SelLineStop))
        CurLine = SelLineStop
        CurPos = SelStop
        ReturnToCaret()
        Me.Invalidate()
    End Sub

    Public Function FindNext(StrToFind As String, MatchCase As Boolean) As Integer
        Dim i As Int64, i2 As Integer, didRestart As Boolean
        Dim startPos As Integer
        didRestart = False
startFromTop:
        For i = CurLine To AllLines.Count - 1
            startPos = 1
            If i = CurLine And didRestart = False Then startPos = CurPos + 1
            For i2 = startPos To Len(AllLines(i))
                If MatchCase Then
                    If Strings.Mid(AllLines(i), i2, Len(StrToFind)) = StrToFind Then
                        CurLine = i
                        CurPos = i2 - 1
                        IsSelecting = True
                        SelLineStart = CurLine
                        SelLineStop = CurLine
                        SelStart = CurPos
                        SelStop = CurPos + Len(StrToFind)
                        CurPos = SelStop

                        ReturnToCaret()
                        Me.Invalidate()
                        Return 1
                    End If
                Else
                    If LCase(Strings.Mid(AllLines(i), i2, Len(StrToFind))) = LCase(StrToFind) Then
                        CurLine = i
                        CurPos = i2 - 1
                        IsSelecting = True
                        SelLineStart = CurLine
                        SelLineStop = CurLine
                        SelStart = CurPos
                        SelStop = CurPos + Len(StrToFind)
                        CurPos = SelStop

                        ReturnToCaret()
                        Me.Invalidate()
                        Return 1
                    End If
                End If
            Next
        Next
        If didRestart = False Then
            CurLine = 0
            CurPos = 0
            didRestart = True
            GoTo startFromTop
        End If

        IsSelecting = False
        ReturnToCaret()
        Me.Invalidate()
        didRestart = True
        Return -1
    End Function
    Public Function FindReplace(StrToFind As String, Replacement As String, MatchCase As Boolean) As Integer
        Dim rt As Integer

        rt = FindNext(StrToFind, MatchCase)
        If rt < 0 Then Return rt
        DeleteSelection()
        InsertText(Replacement, CurLine, CurPos)
        CurPos += Len(Replacement)
        rt = FindNext(StrToFind, MatchCase)

        Return rt
    End Function
    Public Function ReplaceAll(StrToFind As String, Replacement As String, MatchCase As Boolean) As Integer
        Dim rt As Integer, rCount As Integer

        rCount = 0
        Do
            rt = FindReplace(StrToFind, Replacement, MatchCase)
            If rt > 0 Then rCount += 1
        Loop Until rt < 0

        Return rCount
    End Function

    Private Sub SortSyntaxing()
        Dim didSwap As Boolean, i As Integer, tmpS As String, tmpI As Integer
        Dim tmpS2 As String

        If SxCMD Is Nothing Then Exit Sub

        Do
            didSwap = False
            For i = 0 To SxCMD.Count - 2
                If String.Compare(SxCMD(i).sName, SxCMD(i + 1).sName) > 0 Then
                    tmpS = SxCMD(i).sName
                    tmpI = SxCMD(i).sColor
                    tmpS2 = SxCMD(i).special

                    SxCMD(i).sName = SxCMD(i + 1).sName
                    SxCMD(i).sColor = SxCMD(i + 1).sColor
                    SxCMD(i).special = SxCMD(i + 1).special

                    SxCMD(i + 1).sName = tmpS
                    SxCMD(i + 1).sColor = tmpI
                    SxCMD(i + 1).special = tmpS2
                    didSwap = True
                End If
            Next
        Loop Until didSwap = False

        If SxARG Is Nothing Then Exit Sub

        Do
            didSwap = False
            For i = 0 To SxARG.Count - 2
                If String.Compare(SxARG(i).sName, SxARG(i + 1).sName) > 0 Then
                    tmpS = SxARG(i).sName
                    tmpI = SxARG(i).sColor
                    SxARG(i).sName = SxARG(i + 1).sName
                    SxARG(i).sColor = SxARG(i + 1).sColor
                    SxARG(i + 1).sName = tmpS
                    SxARG(i + 1).sColor = tmpI
                    didSwap = True
                End If
            Next
        Loop Until didSwap = False
    End Sub
    Public Sub ClearSyntaxConfig()
        ReDim SxCMD(0)
        ReDim SxARG(0)
    End Sub
    Public Sub SetSyntaxCmdConfig(ByVal StrCmd As String, ByVal IntCol As Integer, ByVal Specials As String)
        Dim i As Integer

        If SxCMD Is Nothing Then
            ReDim SxCMD(0)
            SxCMD(0).sName = StrCmd
            SxCMD(0).sColor = IntCol
            SxCMD(0).special = Specials
            Exit Sub
        End If

        If SxCMD.Count - 1 = 0 Then
            ReDim Preserve SxCMD(1)
            SxCMD(1).sName = StrCmd
            SxCMD(1).sColor = IntCol
            SxCMD(1).special = Specials
            Exit Sub
        End If

        For i = 0 To SxCMD.Count - 1
            If LCase(StrCmd) = LCase(SxCMD(i).sName) Then
                SxCMD(i).sColor = IntCol
                SxCMD(i).special = Specials
                Exit Sub
            End If
        Next
        i = SxCMD.Count
        ReDim Preserve SxCMD(i)
        SxCMD(i).sName = StrCmd
        SxCMD(i).sColor = IntCol
        SxCMD(i).special = Specials

        SortSyntaxing()
    End Sub
    Public Sub SetSyntaxArgConfig(StrArg As String, IntCol As Integer)
        Dim i As Integer

        If SxARG Is Nothing Then
            ReDim SxARG(0)
            SxARG(0).sName = StrArg
            SxARG(0).sColor = IntCol
            Exit Sub
        End If

        If SxARG.Count - 1 = 0 Then
            ReDim Preserve SxARG(1)
            SxARG(1).sName = StrArg
            SxARG(1).sColor = IntCol
            Exit Sub
        End If

        For i = 0 To SxARG.Count - 1
            If LCase(StrArg) = LCase(SxARG(i).sName) Then
                SxARG(i).sColor = IntCol
                Exit Sub
            End If
        Next
        ReDim Preserve SxARG(SxARG.Count)
        SxARG(SxARG.Count - 1).sName = StrArg
        SxARG(SxARG.Count - 1).sColor = IntCol

        SortSyntaxing()
    End Sub


    Private Function GetCmdSyntaxColor(StrCmd As String, ByRef ColOut As Integer, ByRef SpecOut As String) As Boolean
        Dim i As Integer
        If SxCMD Is Nothing Then Return False

        Dim high As Integer, low As Integer

        low = 0
        high = SxCMD.Count - 1

        While (low <= high)
            i = (low + high) \ 2
            Select Case Strings.StrComp(LCase(StrCmd), LCase(SxCMD(i).sName))
                Case -1
                    high = i - 1
                Case 0
                    ColOut = SxCMD(i).sColor
                    SpecOut = SxCMD(i).special
                    Return True
                Case 1
                    low = i + 1
            End Select
        End While

        Return False

        'For i = 0 To SxCMD.Count - 1
        '    If LCase(StrCmd) = LCase(SxCMD(i).sName) Then
        '        ColOut = SxCMD(i).sColor
        '        SpecOut = SxCMD(i).special
        '        Return True
        '    End If
        'Next

        'Return False
    End Function
    Private Function GetArgSyntaxColor(StrArg As String, ByRef ColOut As Integer) As Boolean
        Dim i As Integer
        If SxARG Is Nothing Then Return False

        Dim high As Integer, low As Integer

        low = 0
        high = SxARG.Count - 1

        While (low <= high)
            i = (low + high) \ 2
            Select Case Strings.StrComp(LCase(StrArg), LCase(SxARG(i).sName))
                Case -1
                    high = i - 1
                Case 0
                    ColOut = SxARG(i).sColor
                    Return True
                Case 1
                    low = i + 1
            End Select
        End While

        Return False


        'For i = 0 To SxARG.Count - 1
        '    If LCase(StrArg) = LCase(SxARG(i).sName) Then
        '        ColOut = SxARG(i).sColor
        '        Return True
        '    End If
        'Next
        '
        'Return False
    End Function

    Public Sub DeleteSelection()
        Dim FirstLine As Int64, LastLine As Int64, FirstChar As Int64, LastChar As Int64
        Dim i As Int64
        If IsSelecting = False Then Exit Sub

        If SelLineStart <= SelLineStop Then
            FirstLine = SelLineStart
            LastLine = SelLineStop
            FirstChar = SelStart
            LastChar = SelStop
        Else
            FirstLine = SelLineStop
            LastLine = SelLineStart
            FirstChar = SelStop
            LastChar = SelStart
        End If
        If SelLineStart = SelLineStop Then
            If SelStart < SelStop Then
                FirstChar = SelStart
                LastChar = SelStop
            ElseIf SelStart > SelStop Then
                FirstChar = SelStop
                LastChar = SelStart
            Else
                IsSelecting = False
                Exit Sub
            End If
        End If

        If FirstLine = LastLine Then
            If LastChar >= Len(AllLines(LastLine)) Then
                AllLines(FirstLine) = Strings.Left(AllLines(FirstLine), FirstChar)
            Else
                AllLines(FirstLine) = Strings.Left(AllLines(FirstLine), FirstChar) +
                    Strings.Right(AllLines(LastLine), Len(AllLines(LastLine)) - LastChar)
            End If
        ElseIf FirstLine = 0 And LastLine = AllLines.Count - 1 Then
            If SelStart > 0 And SelStop < Len(AllLines(LastLine)) Then
                AllLines(FirstLine) = Strings.Left(AllLines(FirstLine), FirstChar) +
                Strings.Right(AllLines(LastLine), Len(AllLines(LastLine)) - LastChar)
                ReDim Preserve AllLines(0)
            Else
                ReDim AllLines(0)
            End If

        Else
            AllLines(FirstLine) = Strings.Left(AllLines(FirstLine), FirstChar) +
                Strings.Right(AllLines(LastLine), Len(AllLines(LastLine)) - LastChar)

            For i = 0 To (LastLine - FirstLine) - 1
                DeleteLine(FirstLine + 1)
            Next
        End If
        CurLine = FirstLine
        CurPos = FirstChar
        IsSelecting = False

        If AllLines Is Nothing Then ReDim AllLines(0)
    End Sub
    Public Function CopySelection() As String
        Dim FirstLine As Int64, LastLine As Int64, FirstChar As Int64, LastChar As Int64
        Dim ret As String

        If IsSelecting = False Then Return ""

        If SelLineStart <= SelLineStop Then
            FirstLine = SelLineStart
            LastLine = SelLineStop
            FirstChar = SelStart
            LastChar = SelStop
        Else
            FirstLine = SelLineStop
            LastLine = SelLineStart
            FirstChar = SelStop
            LastChar = SelStart
        End If
        If SelLineStart = SelLineStop Then
            If SelStart < SelStop Then
                FirstChar = SelStart
                LastChar = SelStop
            ElseIf SelStart > SelStop Then
                FirstChar = SelStop
                LastChar = SelStart
            Else
                IsSelecting = False
                Return ""
            End If
        End If

        ret = ""
        If FirstLine = LastLine Then
            ret = Strings.Mid(AllLines(FirstLine), FirstChar + 1, LastChar - FirstChar)
        Else
            ret = Strings.Right(AllLines(FirstLine), Len(AllLines(FirstLine)) - FirstChar)
            ret += vbCrLf
            For i = FirstLine + 1 To LastLine
                If i <> LastLine Then
                    ret += AllLines(i) + vbCrLf
                Else
                    ret += Strings.Left(AllLines(i), LastChar)
                End If
            Next
        End If

        Return ret
    End Function
    Public Sub InsertText(Txt As String, LineNumber As Int64, CharPos As Int64)
        Dim I As Int64, Lines() As String, tmp As String

        CurLine = LineNumber
        CurPos = CharPos

        tmp = ""
        If CurPos < Len(AllLines(CurLine)) Then
            tmp = Strings.Right(AllLines(CurLine), Len(AllLines(CurLine)) - CurPos)
            If CurPos > 0 Then
                AllLines(CurLine) = Strings.Left(AllLines(CurLine), CurPos)
            Else
                AllLines(CurLine) = ""
            End If
        End If

        Lines = Split(Replace(Txt, vbCrLf, vbLf) + vbLf, vbLf)
        CurLine = LineNumber
        For I = 1 To Lines.Count - 2
            AddBlankLine(CurLine + 1)
        Next
        For I = 0 To Lines.Count - 2
            AllLines(LineNumber + I) += Lines(I)
        Next
        CurLine = LineNumber + (I - 1)
        CurPos = Len(AllLines(CurLine))
        AllLines(CurLine) += tmp

        ReturnToCaret()
    End Sub

    Private Sub AddChar(chr As Char)
        If CurPos = Len(AllLines(CurLine)) Then
            AllLines(CurLine) += chr
        Else
            If CurPos = 0 Then
                AllLines(CurLine) = chr + AllLines(CurLine)
            Else
                AllLines(CurLine) = Strings.Left(AllLines(CurLine), CurPos) +
                                    chr +
                                    Strings.Right(AllLines(CurLine), Len(AllLines(CurLine)) - CurPos)
            End If
        End If
        CurPos += 1
    End Sub
    Private Sub DelChar(mode As Integer)
        If mode = 0 Then 'Backspace

            If CurPos > 0 Then
                If CurPos < Len(AllLines(CurLine)) Then
                    AllLines(CurLine) = Strings.Left(AllLines(CurLine), CurPos - 1) +
                                       Strings.Right(AllLines(CurLine), Len(AllLines(CurLine)) - CurPos)
                Else
                    AllLines(CurLine) = Strings.Left(AllLines(CurLine), CurPos - 1)
                End If
                CurPos -= 1
            Else
                If CurLine > 0 Then
                    CurLine -= 1
                    CurPos = Len(AllLines(CurLine))
                    AllLines(CurLine) += AllLines(CurLine + 1)
                    DeleteLine(CurLine + 1)
                End If
            End If

        ElseIf mode = 1 Then 'Delete
            If CurPos < Len(AllLines(CurLine)) Then
                If CurPos = 0 Then
                    AllLines(CurLine) = Strings.Right(AllLines(CurLine), Len(AllLines(CurLine)) - 1)
                Else
                    AllLines(CurLine) = Strings.Left(AllLines(CurLine), CurPos) +
                                       Strings.Right(AllLines(CurLine), Len(AllLines(CurLine)) - (CurPos + 1))
                End If
            Else
                If CurLine < AllLines.Count - 1 Then
                    AllLines(CurLine) += AllLines(CurLine + 1)
                    DeleteLine(CurLine + 1)
                End If
            End If
        End If
    End Sub
    Private Sub DeleteLine(LineNum As UInt64)
        Dim I As UInt64, tmp As String

        If LineNum = 0 And AllLines.Count = 1 Then
            AllLines(0) = ""
            Exit Sub
        End If

        For I = LineNum To AllLines.Count - 2
            tmp = AllLines(I + 1)
            AllLines(I) = AllLines(I + 1)
            AllLines(I + 1) = tmp
        Next
        ReDim Preserve AllLines(AllLines.Count - 2)

    End Sub
    Private Sub AddBlankLine(index As Int64)
        Dim I As Int64, tmp As String
        ReDim Preserve AllLines(AllLines.Count)


        For I = AllLines.Count - 1 To index + 1 Step -1
            tmp = AllLines(I - 1)
            AllLines(I - 1) = AllLines(I)
            AllLines(I) = tmp
        Next
    End Sub
    Private Sub AddLineFromEnterKey()
        Dim autoSpaces As String, i As Int64, tmp As String
        Dim presLine As Int64

        If CurLine = AllLines.Count - 1 Then
            ReDim Preserve AllLines(CurLine + 1)
            AllLines(CurLine + 1) = ""

            If CurPos < Len(AllLines(CurLine)) And CurPos > 0 Then
                AllLines(CurLine + 1) = Strings.Right(AllLines(CurLine), Len(AllLines(CurLine)) - CurPos)
                AllLines(CurLine) = Strings.Left(AllLines(CurLine), CurPos)
            ElseIf CurPos = 0 Then
                AllLines(CurLine + 1) = AllLines(CurLine)
                AllLines(CurLine) = ""
            End If

            CurLine += 1
            CurPos = 0

            If AllLines(CurLine - 1) <> "" Then
                autoSpaces = ""
                i = 1
                Do While Mid(AllLines(CurLine - 1), i, 1) = vbTab Or Mid(AllLines(CurLine - 1), i, 1) = " "
                    autoSpaces += Mid(AllLines(CurLine - 1), i, 1)
                    i += 1
                Loop
                If Strings.Right(AllLines(CurLine - 1), 1) = "{" Then
                    presLine = CurLine
                    AllLines(CurLine) = autoSpaces + vbTab
                Else
                    AllLines(CurLine) = autoSpaces
                End If
                CurPos = Len(AllLines(CurLine))

                If Mid(AllLines(CurLine - 1), i, 6) = "switch" Then
                    AllLines(CurLine) += "{"
                    CurPos += 1
                    AddLineFromEnterKey()
                    CurPos = Len(AllLines(CurLine))
                    AddLineFromEnterKey()
                    AllLines(CurLine) = autoSpaces + "}"
                    CurLine -= 1
                    CurPos = Len(AllLines(CurLine))
                End If
                If Mid(AllLines(CurLine - 1), i, 4) = "case" Then
                    AllLines(CurLine) += "{"
                    CurPos += 1
                    AddLineFromEnterKey()
                    CurPos = Len(AllLines(CurLine))
                    AddLineFromEnterKey()
                    AllLines(CurLine) = autoSpaces + "}"
                    CurLine -= 1
                    CurPos = Len(AllLines(CurLine))
                End If
                If Mid(AllLines(CurLine - 1), i, 7) = "default" Then
                    AllLines(CurLine) += "{"
                    CurPos += 1
                    AddLineFromEnterKey()
                    CurPos = Len(AllLines(CurLine))
                    AddLineFromEnterKey()
                    AllLines(CurLine) = autoSpaces + "}"
                    CurLine -= 1
                    CurPos = Len(AllLines(CurLine))
                End If
                If Mid(AllLines(CurLine - 1), i, 3) = "fnc" Then
                    AllLines(CurLine) += "{"
                    CurPos += 1
                    AddLineFromEnterKey()
                    CurPos = Len(AllLines(CurLine))
                    AddLineFromEnterKey()
                    AllLines(CurLine) = autoSpaces + "}"
                    CurLine -= 1
                    CurPos = Len(AllLines(CurLine))
                End If

            End If
        Else
            ReDim Preserve AllLines(AllLines.Count)
            tmp = ""
            AllLines(AllLines.Count - 1) = ""
            For I = AllLines.Count - 1 To CurLine + 2 Step -1
                tmp = AllLines(I)
                AllLines(I) = AllLines(I - 1)
                AllLines(I - 1) = tmp
            Next

            If CurPos < Len(AllLines(CurLine)) And CurPos > 0 Then
                AllLines(CurLine + 1) = Strings.Right(AllLines(CurLine), Len(AllLines(CurLine)) - CurPos)
                AllLines(CurLine) = Strings.Left(AllLines(CurLine), CurPos)
            ElseIf CurPos = 0 Then
                AllLines(CurLine + 1) = AllLines(CurLine)
                AllLines(CurLine) = ""
            End If

            CurLine += 1
            CurPos = 0

            If AllLines(CurLine - 1) <> "" Then
                autoSpaces = ""
                i = 1
                Do While Mid(AllLines(CurLine - 1), i, 1) = vbTab Or Mid(AllLines(CurLine - 1), i, 1) = " "
                    autoSpaces += Mid(AllLines(CurLine - 1), i, 1)
                    i += 1
                Loop
                If Strings.Right(AllLines(CurLine - 1), 1) = "{" Then
                    presLine = CurLine
                    AllLines(CurLine) = autoSpaces + vbTab
                Else
                    AllLines(CurLine) = autoSpaces
                End If
                CurPos = Len(AllLines(CurLine))
                If Mid(AllLines(CurLine - 1), i, 6) = "switch" Then
                    AllLines(CurLine) += "{"
                    CurPos += 1
                    AddLineFromEnterKey()
                    CurPos = Len(AllLines(CurLine))
                    AddLineFromEnterKey()
                    AllLines(CurLine) = autoSpaces + "}"
                    CurLine -= 1
                    CurPos = Len(AllLines(CurLine))
                End If
                If Mid(AllLines(CurLine - 1), i, 4) = "case" Then
                    AllLines(CurLine) += "{"
                    CurPos += 1
                    AddLineFromEnterKey()
                    CurPos = Len(AllLines(CurLine))
                    AddLineFromEnterKey()
                    AllLines(CurLine) = autoSpaces + "}"
                    CurLine -= 1
                    CurPos = Len(AllLines(CurLine))
                End If
                If Mid(AllLines(CurLine - 1), i, 7) = "default" Then
                    AllLines(CurLine) += "{"
                    CurPos += 1
                    AddLineFromEnterKey()
                    CurPos = Len(AllLines(CurLine))
                    AddLineFromEnterKey()
                    AllLines(CurLine) = autoSpaces + "}"
                    CurLine -= 1
                    CurPos = Len(AllLines(CurLine))
                End If
                If Mid(AllLines(CurLine - 1), i, 3) = "fnc" Then
                    AllLines(CurLine) += "{"
                    CurPos += 1
                    AddLineFromEnterKey()
                    CurPos = Len(AllLines(CurLine))
                    AddLineFromEnterKey()
                    AllLines(CurLine) = autoSpaces + "}"
                    CurLine -= 1
                    CurPos = Len(AllLines(CurLine))
                End If

            End If
        End If
    End Sub


    Private Sub tBlinker_Tick(sender As Object, e As EventArgs) Handles tBlinker.Tick
        If CurLine < RenderStart Then Exit Sub
        If CurLine > RenderStart + RenderLines Then Exit Sub

        BlinkerONOFF = Not BlinkerONOFF
        Me.Invalidate()
    End Sub

    Public Sub Init()
        tBlinker.Enabled = True

        WasInitialized = True
    End Sub
    Private Sub SyntaxView_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        WasInitialized = False

        Me.DoubleBuffered = True

        ReDim BreakOps(0)
        ReDim AllLines(0)
        AllLines(0) = ""

        RenderStart = 0
        CurLine = 0
        CurPos = 0

        GFX = Graphics.FromImage(New Bitmap(1, 1))

        FontName = "Courier New"
        FontSize = 12
        FontColor = Val("&HFFffffff")
        BgColor = Val("&HFF111111")
        LineHLColor = Val("&HFF222222")
        SelectionHLColor = Val("&HFF6060C0")

        SVFont = New Font("Courier New", 12, FontStyle.Regular)
        SVBrush = New SolidBrush(Color.FromArgb(FontColor))

        BlinkerXY.X = 4
        BlinkerXY.Y = 2
        BlinkerONOFF = True

        VScroll.Maximum = 0
        VScroll.Value = 0
        VScroll.Visible = False

        HScroll.Minimum = 0
        HScroll.Maximum = 0
        HScroll.Visible = False

        EnableSyntaxHL = True
        EnableLineHL = True


    End Sub

    Private Sub SyntaxView_LostFocus(sender As Object, e As EventArgs) Handles Me.LostFocus
        BlinkerONOFF = False
        tBlinker.Enabled = False
        Me.Invalidate()
    End Sub

    Private Sub SyntaxView_GotFocus(sender As Object, e As EventArgs) Handles Me.GotFocus
        If WasInitialized = False Then Exit Sub
        BlinkerONOFF = True
        tBlinker.Enabled = True
        Me.Invalidate()
    End Sub

    Private Sub SyntaxView_Resize(sender As Object, e As EventArgs) Handles Me.Resize


        VScroll.Left = Me.Width - VScroll.Width
        VScroll.Top = 0
        VScroll.Height = Me.Height

        HScroll.Top = Me.Height - HScroll.Height
        HScroll.Left = 0
        HScroll.Width = Me.Width - VScroll.Width

        If WasInitialized = False Then Exit Sub

        RenderLines = (Me.Height \ SVFont.Height)
        If (AllLines.Count - 1) < RenderLines Then
            RenderStart = 0
        End If
        If RenderStart > 0 Then
            If RenderStart + RenderLines > AllLines.Count - 1 Then
                RenderStart = (AllLines.Count - 1) - RenderLines
                If RenderStart < 0 Then RenderStart = 0
            End If
        End If
        ResizeScrollbars()
        Me.Invalidate()
    End Sub

    Private Sub ResizeScrollbars()
        If WasInitialized = False Then Exit Sub

        RenderLines = (Me.Height \ SVFont.Height)
        If AllLines.Count - 1 > RenderLines Then
            VScroll.Visible = True
        Else
            VScroll.Visible = False
        End If

        If VScroll.Visible = False Then Exit Sub
        VScroll.SmallChange = 1
        VScroll.LargeChange = 1
        VScroll.Minimum = 0

        If (AllLines.Count - 1) - RenderLines > 0 Then
            VScroll.Maximum = (AllLines.Count) - RenderLines
        Else
            VScroll.Maximum = 0
        End If

        If VScroll.Maximum < VScroll.Minimum Then VScroll.Maximum = 0
        VScroll.Value = RenderStart
    End Sub

    Private Sub VScroll_Scroll(sender As Object, e As ScrollEventArgs) Handles VScroll.Scroll
        RenderStart = VScroll.Value
        Me.Invalidate()
    End Sub

    Private Sub ReturnToCaret()
        If RenderStart + RenderLines > AllLines.Count - 1 Then
            If (AllLines.Count - 1) - RenderLines >= 0 Then RenderStart = (AllLines.Count - 1) - RenderLines
            If RenderStart < 0 Then RenderStart = 0
        End If
        If AllLines.Count - 1 < RenderLines Then RenderStart = 0

        If CurLine < RenderStart Then RenderStart = CurLine
        If CurLine > (RenderStart + RenderLines) Then RenderStart = CurLine - RenderLines
    End Sub


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
    Private Function StripWhiteSpace(Str As String) As String
        Dim I As Integer, rt As Boolean, ret As String

        If Len(Str) <= 0 Then Return ""
        ret = Str

        Do
            rt = True
            If Strings.Left(ret, 1) = " " Or Strings.Left(ret, 1) = vbTab Then
                ret = Strings.Right(ret, Len(ret) - 1)
                rt = False
            End If
        Loop Until rt = True

        Return ret
    End Function
    Private Function IsInsideMultiComment(rn As Int64) As Boolean
        Dim i As Int64

        For i = (RenderStart + rn) To 0 Step -1
            If RenderStart - i > 1000 Then Return False
            If Strings.Left(StripWhiteSpace(AllLines(i)), 2) = "*/" Then Return False
            If Strings.Left(StripWhiteSpace(AllLines(i)), 2) = "/*" Then Return True
        Next
    End Function
    Private Function UnsafeChars(StrIn As String) As String
        Dim ret As String
        ret = Replace(StrIn, "%7B", "{")
        ret = Replace(ret, "%7D", "}")
        ret = Replace(ret, "%23", "#")
        ret = Replace(ret, "%25", "%")
        Return ret
    End Function
    Private Function SafeChars(StrIn As String) As String
        Dim ret As String
        ret = Replace(StrIn, "%", "%25")
        ret = Replace(ret, "{", "%7B")
        ret = Replace(ret, "}", "%7D")
        ret = Replace(ret, "#", "%23")
        Return ret
    End Function
    Private Function FormatSyntax(str As String) As String
        Dim tStr As String, sp() As String, COM As String, ARGS As String
        Dim argOut As String, i As Integer, i2 As Integer, colSet As Integer, rt As Boolean
        Dim stopRead As Boolean, whiteSpace As String, cmtStrip As String

        tStr = SafeChars(str)
        If EnableSyntaxHL = False Then Return tStr

        If tStr = "" Then Return "{c#00000000}{/c#}"

        whiteSpace = ""
        Do
            rt = True
            If Strings.Left(tStr, 1) = " " Or Strings.Left(tStr, 1) = vbTab Then
                whiteSpace += Strings.Left(tStr, 1)
                tStr = Strings.Right(tStr, Len(tStr) - 1)
                rt = False
            End If
        Loop Until rt = True

        If Strings.Left(tStr, 2) = "//" Then Return whiteSpace + "{c#FF00FF00}{i}" + tStr + "{/c#}"
        If tStr = "/*" Then Return whiteSpace + "{c#FF00FF00}{i}" + tStr + "{/c#}"
        If tStr = "*/" Then Return whiteSpace + "{c#FF00FF00}{i}" + tStr + "{/c#}"

        sp = Split(parseSyntax(tStr) + " ", " ")
        COM = sp(0)
        If Strings.Right(COM, 1) = ":" Then
            Return whiteSpace + "{c#FFeeeeee}{b}" + tStr + "{/c#}"
        End If
        If Len(tStr) > Len(COM) Then
            ARGS = Strings.Right(tStr, Len(tStr) - Len(COM))
        Else
            ARGS = ""
        End If

        If Len(COM) = 8 And Len(ARGS) = 9 Then
            tStr = Strings.Right(ARGS, 8)
            If LCase(Hex(Val("&H" + COM))) = LCase(COM) And LCase(Hex(Val("&H" + tStr))) = LCase(tStr) Then
                Return whiteSpace + "{c#FFeeeeee}{b}" + COM + ARGS + "{/c#}"
            End If
        End If

        If LCase(COM) = "break" Then
            If Len(Replace(Replace(Replace(Replace(ARGS, " ", ""), "(", ""), ")", ""), ";", "")) < 5 Then
                rt = GetCmdSyntaxColor("%break", colSet, tStr)
                If rt Then
                    COM = "{c#" + Strings.Right("00000000" + Hex(colSet), 8) + "}" + tStr + COM + "{/c#}"
                End If
            Else
                rt = GetCmdSyntaxColor("break", colSet, tStr)
                If rt Then
                    COM = "{c#" + Strings.Right("00000000" + Hex(colSet), 8) + "}" + tStr + COM + "{/c#}"
                End If
            End If
        Else
            'Find out if the command or instruction exists
            rt = GetCmdSyntaxColor(COM, colSet, tStr)
            If rt = False Then
                Return whiteSpace + "{c#FFFF0000}{u}" + tStr + "{/c#}"
            End If
            COM = "{c#" + Strings.Right("00000000" + Hex(colSet), 8) + "}" + tStr + COM + "{/c#}"
        End If

        For i = 1 To Len(ARGS)
            If LCase(Strings.Mid(ARGS, i, 3)) = "%7b" Or LCase(Strings.Mid(ARGS, i, 3)) = "%7d" Then
                Return "{c#FFFF0000}{u}" + SafeChars(str) + "{/c#}"
            End If
        Next

        sp = Split(ARGS + " //", "//")
        ARGS = sp(0)
        cmtStrip = ""
        If sp(1) <> "" Then cmtStrip = "{c#FF00FF00}{i}//" + sp(1) + "{/c#}"

        argOut = ""
        i = 1
        Do
            tStr = parseRead(ARGS, i)
            If Strings.Left(tStr, 1) = ":" Then
                argOut += "{c#FFeeeeee}{b}" + tStr + "{/c#}"
                i += Len(tStr)
            ElseIf Strings.Left(tStr, 1) = """" Then
                tStr = """"
                Do
                    i += 1
                    tStr += Strings.Mid(ARGS, i, 1)
                Loop Until i > Len(ARGS) Or Strings.Mid(ARGS, i, 1) = """"
                argOut += "{c#FFff8080}{b}" + tStr + "{/c#}"
                i += 1
            ElseIf Strings.Left(tStr, 1) = "$" Then
                argOut += "{c#FFffffd0}{b}" + tStr + "{/c#}"
                i += Len(tStr)
            ElseIf LCase(Strings.Left(tStr, 2)) = "0x" Then
                argOut += "{c#FFffffd0}{b}" + tStr + "{/c#}"
                i += Len(tStr)
            Else
                If tStr <> "" Then

                    rt = GetArgSyntaxColor(tStr, colSet)
                    If rt Then
                        argOut += "{c#" + Strings.Right("00000000" + Hex(colSet), 8) + "}{b}" + tStr + "{/c#}"
                        i += Len(tStr)
                    Else
                        argOut += tStr
                        i += Len(tStr)
                    End If
                Else
                    tStr = readParseChars(ARGS, i)
                    If tStr <> "" Then
                        If tStr = "//" Then
                            argOut += "{c#ff00ff00}{i}//" + Strings.Right(ARGS, Len(ARGS) - (i + 1)) + "{/c#}"
                            i = Len(ARGS) + 1
                        Else
                            argOut += tStr
                            i += Len(tStr)
                        End If
                    End If
                End If
            End If

        Loop Until i > Len(ARGS)

        Return whiteSpace + COM + argOut + cmtStrip

    End Function
    Private Sub LoadRenderBuffer()
        Dim I As Integer

        If HScroll.Visible = True Then
            RenderLines = ((Me.Height - HScroll.Height) \ SVFont.Height) - 1
        Else
            RenderLines = (Me.Height \ SVFont.Height) - 1
        End If
        ReDim RenderBuffer(RenderLines)
        If AllLines.Count - 1 <= RenderLines Then RenderStart = 0

        For I = 0 To RenderLines
            RenderBuffer(I) = ""
            If RenderStart + I < AllLines.Count Then
                If EnableSyntaxHL Then
                    'Margin
                    RenderBuffer(I) = "{c#FFd0d0d0}   " +
                                      Strings.Right("          " + (RenderStart + I + 1).ToString, Len((AllLines.Count).ToString)) + " " +
                                      "{/c#}"

                    'Line data
                    If IsInsideMultiComment(I) Then
                        RenderBuffer(I) += "{c#FF00FF00}{i}" + SafeChars(Replace(AllLines(RenderStart + I), vbTab, "    ")) + "{/c#}"
                    Else
                        RenderBuffer(I) += FormatSyntax(Replace(AllLines(RenderStart + I), vbTab, "    "))
                    End If
                Else
                    'Margin
                    RenderBuffer(I) = "   " + Strings.Right("          " + (RenderStart + I + 1).ToString, Len((AllLines.Count).ToString)) + " "
                    'Line Data
                    RenderBuffer(I) += SafeChars(Replace(AllLines(RenderStart + I), vbTab, "    "))
                End If
            End If
        Next
    End Sub

    Private Sub DrawRenderBuffer(ByRef e As PaintEventArgs)
        Dim i As Integer, i2 As Integer, XY As Point, renderStr As String
        Dim RCT As Rectangle, tbrsh As Brush, HLWidth As Integer
        Dim FirstLine As Int64, LastLine As Int64, FirstChar As Int64, LastChar As Int64

        If BreakOps.Count < AllLines.Count Then ReDim Preserve BreakOps(AllLines.Count - 1)

        MarginWidth = StrWidth("   " + AllLines.Count.ToString + " ")

        HLWidth = 8

        XY.X = 0
        XY.Y = 0
        For i = 0 To RenderLines
            If RenderBuffer(i) <> "" Then
                If EnableLineHL And IsSelecting = False Then
                    If RenderStart + i = CurLine Then
                        RCT.X = -2
                        RCT.Y = XY.Y
                        RCT.Height = (SVFont.Height + 2)
                        RCT.Width = Me.Width + 2

                        tbrsh = New SolidBrush(Color.FromArgb(LineHLColor))
                        e.Graphics.FillRectangle(tbrsh, RCT)
                        tbrsh.Dispose()
                    End If
                End If
                If IsSelecting Then
                    If SelLineStart <= SelLineStop Then
                        FirstLine = SelLineStart
                        LastLine = SelLineStop
                        FirstChar = SelStart
                        LastChar = SelStop
                    Else
                        FirstLine = SelLineStop
                        LastLine = SelLineStart
                        FirstChar = SelStop
                        LastChar = SelStart
                    End If
                    If SelLineStart = SelLineStop Then
                        If SelStart < SelStop Then
                            FirstChar = SelStart
                            LastChar = SelStop
                        ElseIf SelStart > SelStop Then
                            FirstChar = SelStop
                            LastChar = SelStart
                        Else
                            IsSelecting = False
                        End If
                    End If

                    If (RenderStart + i) = FirstLine Then
                        If FirstChar = 0 Then
                            RCT.X = MarginWidth
                            RCT.Y = XY.Y
                            RCT.Height = SVFont.Height
                            If LastLine = FirstLine Then
                                RCT.Width = StrWidth(Strings.Left(AllLines(FirstLine), LastChar)) + HLWidth
                                tbrsh = New SolidBrush(Color.FromArgb(SelectionHLColor))
                                e.Graphics.FillRectangle(tbrsh, RCT)
                                tbrsh.Dispose()
                            Else
                                RCT.Width = StrWidth(Mid(AllLines(FirstLine), FirstChar + 1, Len(AllLines(FirstLine)) - FirstChar)) + HLWidth
                                tbrsh = New SolidBrush(Color.FromArgb(SelectionHLColor))
                                e.Graphics.FillRectangle(tbrsh, RCT)
                                tbrsh.Dispose()
                            End If
                        Else
                            RCT.X = MarginWidth + StrWidth(Strings.Left(AllLines(FirstLine), FirstChar))
                            RCT.Y = XY.Y
                            RCT.Height = SVFont.Height
                            If LastLine = FirstLine Then
                                RCT.Width = StrWidth(Mid(AllLines(FirstLine), FirstChar + 1, LastChar - FirstChar)) + HLWidth
                                tbrsh = New SolidBrush(Color.FromArgb(SelectionHLColor))
                                e.Graphics.FillRectangle(tbrsh, RCT)
                                tbrsh.Dispose()
                            Else
                                RCT.Width = StrWidth(Mid(AllLines(FirstLine), FirstChar + 1, Len(AllLines(FirstLine)) - FirstChar)) + HLWidth
                                tbrsh = New SolidBrush(Color.FromArgb(SelectionHLColor))
                                e.Graphics.FillRectangle(tbrsh, RCT)
                                tbrsh.Dispose()
                            End If
                        End If
                    Else
                        If RenderStart + i = LastLine Then
                            RCT.X = MarginWidth
                            RCT.Y = XY.Y
                            RCT.Height = SVFont.Height
                            If LastChar > 0 Then
                                RCT.Width = StrWidth(Strings.Left(AllLines(LastLine), LastChar)) + HLWidth
                                tbrsh = New SolidBrush(Color.FromArgb(SelectionHLColor))
                                e.Graphics.FillRectangle(tbrsh, RCT)
                                tbrsh.Dispose()
                            End If
                        ElseIf (RenderStart + i) > FirstLine And (RenderStart + i) < LastLine Then
                            RCT.X = MarginWidth
                            RCT.Y = XY.Y
                            RCT.Height = SVFont.Height
                            RCT.Width = StrWidth(AllLines(RenderStart + i)) + HLWidth
                            tbrsh = New SolidBrush(Color.FromArgb(SelectionHLColor))
                            e.Graphics.FillRectangle(tbrsh, RCT)
                            tbrsh.Dispose()
                        End If
                    End If


                End If

                i2 = 1
                renderStr = ""
                Do
                    If Mid(RenderBuffer(i), i2, 3) = "{c#" Then
                        If renderStr <> "" Then
                            e.Graphics.DrawString(UnsafeChars(renderStr), SVFont, SVBrush, XY)
                            XY.X += StrWidth(UnsafeChars(renderStr))
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
                        e.Graphics.DrawString(UnsafeChars(renderStr), SVFont, SVBrush, XY)
                        XY.X += StrWidth(UnsafeChars(renderStr))
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
                    e.Graphics.DrawString(UnsafeChars(renderStr), SVFont, SVBrush, XY)
                    renderStr = ""
                End If

                If BreakOps(RenderStart + i) > 0 Then
                    RCT.Width = StrWidth("_")
                    RCT.Height = RCT.Width
                    RCT.X = StrWidth("__") \ 2
                    RCT.Y = XY.Y + (RCT.Height \ 2)
                    Select Case BreakOps(RenderStart + i)
                        Case 1
                            e.Graphics.FillEllipse(Brushes.Red, RCT)
                        Case 2
                            e.Graphics.FillEllipse(Brushes.Yellow, RCT)
                        Case 3
                            e.Graphics.FillEllipse(Brushes.Green, RCT)
                    End Select
                End If

            End If
            XY.X = 0
            XY.Y += SVFont.Height
        Next

    End Sub
    Private Sub DrawCaret(ByRef e As PaintEventArgs)
        Dim XY As Point

        If BlinkerONOFF And IsSelecting = False Then
            Dim W As Integer
            W = StrWidth(Strings.Left(AllLines(CurLine), CurPos))
            W += MarginWidth

            XY.X = W + 0
            XY.Y = (SVFont.Height * (CurLine - RenderStart)) + 2
            BlinkerXY.X = XY.X
            BlinkerXY.Y = XY.Y
            XY.Y += (SVFont.Height - 4)
            e.Graphics.DrawLine(New Pen(Color.FromArgb(FontColor)), BlinkerXY, XY)
        End If
    End Sub
    Private Sub ClearScreen(ByRef e As PaintEventArgs)
        e.Graphics.Clear(Color.FromArgb(BgColor))
    End Sub
    Private Sub SyntaxView_Paint(sender As Object, e As PaintEventArgs) Handles Me.Paint
        On Error Resume Next
        Dim wasBlinking As Boolean
        wasBlinking = tBlinker.Enabled
        tBlinker.Enabled = False

        ClearScreen(e)
        LoadRenderBuffer()
        DrawRenderBuffer(e)
        DrawCaret(e)

        e.Dispose()
        GC.Collect()
        If wasBlinking Then tBlinker.Enabled = True

        Err.Clear() 'Stupid random overflow exception
    End Sub
    Private Function StrWidth(Str As String) As Integer
        Dim SzF As SizeF, SzF2 As SizeF

        SzF = GFX.MeasureString(Replace(Replace(Str, vbTab, "    "), " ", "X") + "X", SVFont)
        'SzF = GFX.MeasureString(New String("X", Len(Str)) + "I", SVFont)
        SzF2 = GFX.MeasureString("I", SVFont)
        Return SzF.Width - SzF2.Width
    End Function


    Private Sub SyntaxView_MouseMove(sender As Object, e As MouseEventArgs) Handles Me.MouseMove
        Dim YSel As Int64, XSel As Int64, SzF As SizeF, SzF2 As SizeF, I As Integer

        Select Case e.Button.ToString
            Case "Left"
                IsSelecting = True

                YSel = (e.Y \ SVFont.Height) + RenderStart
                If YSel < 0 Then YSel = 0
                If YSel <= AllLines.Count - 1 Then
                    SelLineStop = YSel
                Else
                    YSel = AllLines.Count - 1
                    SelLineStop = YSel
                End If

                XSel = -1
                I = 0
                Do
                    SzF = GFX.MeasureString(Strings.Left(AllLines(YSel), I), SVFont)
                    'SzF = GFX.MeasureString(Replace(Replace(Strings.Left(AllLines(YSel), I), " ", "_"), vbTab, "    "), SVFont)
                    SzF2 = GFX.MeasureString(Strings.Left(AllLines(YSel), I + 1), SVFont)
                    'SzF2 = GFX.MeasureString(Replace(Replace(Strings.Left(AllLines(YSel), I + 1), " ", "_"), vbTab, "    "), SVFont)

                    If (SzF.Width + MarginWidth) < e.X Then
                        If SzF2.Width + MarginWidth >= e.X Then
                            XSel = I + 1
                        End If
                    End If

                    I += 1
                Loop Until I >= Len(AllLines(YSel))
                If XSel > -1 Then SelStop = XSel
                If XSel < 0 Then SelStop = Len(AllLines(YSel))
                If e.X <= MarginWidth + 4 Then SelStop = 0

                Me.Invalidate()
            Case "Middle"
            Case "right"
            Case Else
                If e.X < MarginWidth Then
                    Me.Cursor = Cursors.Hand
                Else
                    Me.Cursor = Cursors.IBeam
                End If
        End Select
    End Sub
    Private Sub SyntaxView_MouseDown(sender As Object, e As MouseEventArgs) Handles Me.MouseDown
        Dim YSel As Int64, XSel As Int64, SzF As SizeF, SzF2 As SizeF, I As Integer

        Select Case e.Button.ToString
            Case "Left"
                If e.X < MarginWidth Then
                    YSel = e.Y \ SVFont.Height
                    If BreakOps.Count < AllLines.Count Then ReDim Preserve BreakOps(AllLines.Count - 1)
                    If YSel < 0 Then Exit Sub
                    If YSel <= AllLines.Count - 1 Then
                        BreakOps(RenderStart + YSel) += 1
                        If BreakOps(RenderStart + YSel) > 1 Then BreakOps(RenderStart + YSel) = 0
                    End If
                    Me.Invalidate()
                End If

                IsSelecting = False
                YSel = (e.Y \ SVFont.Height) + RenderStart
                If YSel < 0 Then YSel = 0
                If YSel <= AllLines.Count - 1 Then
                    CurLine = YSel
                Else
                    YSel = AllLines.Count - 1
                    CurLine = YSel
                End If

                XSel = -1
                I = 0
                Do
                    SzF = GFX.MeasureString(Replace(Strings.Left(AllLines(YSel), I), vbTab, "    "), SVFont)
                    SzF2 = GFX.MeasureString(Replace(Strings.Left(AllLines(YSel), I + 1), vbTab, "    "), SVFont)

                    If (SzF.Width + MarginWidth) < e.X Then
                        If SzF2.Width + MarginWidth >= e.X Then
                            XSel = I + 1
                        End If
                    End If

                    I += 1
                Loop Until I >= Len(AllLines(YSel))
                If XSel > -1 Then CurPos = XSel
                If XSel < 0 Then CurPos = Len(AllLines(YSel))
                If e.X <= MarginWidth + 4 Then CurPos = 0

                SelLineStart = CurLine
                SelStart = CurPos
                Me.Invalidate()
            Case "Right"
            Case "Middle"
            Case Else
        End Select
    End Sub

    Private Sub SyntaxView_MouseClick(sender As Object, e As MouseEventArgs) Handles Me.MouseClick

    End Sub

    Private Sub SyntaxView_MouseWheel(sender As Object, e As MouseEventArgs) Handles Me.MouseWheel
        Dim scrollAmmount As Integer

        If VScroll.Visible = False Then Exit Sub

        ResizeScrollbars()

        If e.Delta > 0 Then
            scrollAmmount = (e.Delta \ 120) * 3
            If RenderStart > scrollAmmount Then
                RenderStart -= scrollAmmount
                VScroll.Value = RenderStart
                Me.Invalidate()
            Else
                RenderStart = 0
                VScroll.Value = RenderStart
                Me.Invalidate()
            End If
        ElseIf e.Delta < 0 Then
            scrollAmmount = ((0 - e.Delta) \ 120) * 3
            If RenderStart + RenderLines < AllLines.Count Then
                RenderStart += scrollAmmount
                If RenderStart + RenderLines > AllLines.Count Then
                    RenderStart = (AllLines.Count) - RenderLines
                End If
                VScroll.Value = RenderStart
                Me.Invalidate()
            End If
        End If
    End Sub

    Private Sub SyntaxView_KeyDown(sender As Object, e As KeyEventArgs) Handles Me.KeyDown
        Dim tStr As String

        If e.KeyData.Left = True Then MsgBox("left")
        Select Case e.KeyCode
            Case Keys.Back 'BACKSPACE
                e.Handled = True
                If IsSelecting Then
                    DeleteSelection()
                Else
                    DelChar(0)
                End If
            Case Keys.Delete 'DELETE
                e.Handled = True
                If IsSelecting Then
                    DeleteSelection()
                Else
                    DelChar(1)
                End If
            Case Keys.Enter 'ENTER
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                AddLineFromEnterKey()
            Case Keys.Home 'HOME
                e.Handled = True
                If e.Control Then
                    If e.Shift Then
                        If IsSelecting Then
                            SelLineStop = 0
                            SelStop = 0
                            CurPos = 0
                            CurLine = 0
                        Else
                            IsSelecting = True
                            SelLineStart = CurLine
                            SelStart = CurPos
                            SelLineStop = 0
                            SelStop = 0

                            CurPos = 0
                            CurLine = 0
                        End If
                    Else
                        IsSelecting = False
                        CurLine = 0
                        CurPos = 0
                    End If
                Else
                    If e.Shift Then
                        If IsSelecting Then
                            SelLineStop = CurLine
                            SelStop = 0
                            CurPos = 0
                        Else
                            IsSelecting = True
                            SelLineStart = CurLine
                            SelStart = CurPos
                            SelLineStop = CurLine
                            SelStop = 0
                            CurPos = 0
                        End If
                    Else
                        IsSelecting = False
                        CurPos = 0
                    End If
                End If
            Case Keys.End 'END
                e.Handled = True
                If e.Control Then
                    If e.Shift Then
                        If IsSelecting Then
                            SelLineStop = AllLines.Count - 1
                            SelStop = Len(AllLines(AllLines.Count - 1))
                            CurPos = SelStop
                            CurLine = SelLineStop
                        Else
                            IsSelecting = True
                            SelLineStart = CurLine
                            SelStart = CurPos
                            SelLineStop = AllLines.Count - 1
                            SelStop = Len(AllLines(AllLines.Count - 1))
                            CurPos = SelStop
                            CurLine = SelLineStop
                        End If
                    Else
                        IsSelecting = False
                        CurLine = AllLines.Count - 1
                        CurPos = Len(AllLines(CurLine))
                    End If
                Else
                    If e.Shift Then
                        If IsSelecting Then
                            SelLineStop = CurLine
                            SelStop = Len(AllLines(CurLine))
                            CurPos = SelStop
                        Else
                            IsSelecting = True
                            SelLineStart = CurLine
                            SelStart = CurPos
                            SelLineStop = CurLine
                            SelStop = Len(AllLines(CurLine))
                            CurPos = SelStop
                        End If
                    Else
                        IsSelecting = False
                        CurPos = Len(AllLines(CurLine))
                    End If
                End If
            Case Keys.PageUp 'PAGE UP
                e.Handled = True
                IsSelecting = False
                If e.Control Then
                    CurLine = 0
                Else
                    If (CDec(CurLine) - RenderLines) - 1 > 0 Then
                        CurLine -= (RenderLines + 1)
                    Else
                        CurLine = 0
                    End If
                End If
                If CurPos > Len(AllLines(CurLine)) Then CurPos = Len(AllLines(CurLine))
                ReturnToCaret()
            Case Keys.PageDown 'PAGE DOWN
                e.Handled = True
                IsSelecting = False
                If e.Control Then
                    CurLine = AllLines.Count - 1
                Else
                    CurLine += RenderLines + 1
                    If CurLine > AllLines.Count - 1 Then CurLine = AllLines.Count - 1
                End If
                If CurPos > Len(AllLines(CurLine)) Then CurPos = Len(AllLines(CurLine))
                ReturnToCaret()
            Case Keys.ShiftKey
                e.Handled = True
            Case Keys.CapsLock
                e.Handled = True
            Case Keys.Control
                e.Handled = True
            Case Keys.ControlKey
                e.Handled = True
            Case Keys.Alt
                e.Handled = True
            Case Keys.LWin
                e.Handled = True
            Case Keys.Menu
                e.Handled = True
            Case Keys.Apps
                e.Handled = True
            Case Keys.Insert
                e.Handled = True
            Case Keys.OemQuestion
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                If e.Shift = True Then
                    AddChar("?")
                Else
                    AddChar("/")
                End If
            Case Keys.OemPeriod
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                If e.Shift = True Then
                    AddChar(">")
                Else
                    AddChar(".")
                End If
            Case Keys.Oemcomma
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                If e.Shift = True Then
                    AddChar("<")
                Else
                    AddChar(",")
                End If
            Case Keys.OemSemicolon
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                If e.Shift = True Then
                    AddChar(":")
                Else
                    AddChar(";")
                End If
            Case Keys.OemQuotes
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                If e.Shift = True Then
                    AddChar("""")
                Else
                    AddChar("'")
                End If
            Case Keys.Oem5
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                If e.Shift = True Then
                    AddChar("|")
                Else
                    AddChar("\")
                End If
            Case Keys.OemOpenBrackets
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                If e.Shift = True Then
                    AddChar("{")
                Else
                    AddChar("[")
                End If
            Case Keys.Oem6
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                If e.Shift = True Then
                    AddChar("}")
                Else
                    AddChar("]")
                End If
            Case Keys.OemMinus
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                If e.Shift = True Then
                    AddChar("_")
                Else
                    AddChar("-")
                End If
            Case Keys.Oemplus
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                If e.Shift = True Then
                    AddChar("+")
                Else
                    AddChar("=")
                End If
            Case Keys.Oemtilde
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                If e.Shift = True Then
                    AddChar("`")
                Else
                    AddChar("~")
                End If
            Case Keys.D1
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                If e.Shift = True Then
                    AddChar("!")
                Else
                    AddChar("1")
                End If
            Case Keys.D2
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                If e.Shift = True Then
                    AddChar("@")
                Else
                    AddChar("2")
                End If
            Case Keys.D3
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                If e.Shift = True Then
                    AddChar("#")
                Else
                    AddChar("3")
                End If
            Case Keys.D4
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                If e.Shift = True Then
                    AddChar("$")
                Else
                    AddChar("4")
                End If
            Case Keys.D5
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                If e.Shift = True Then
                    AddChar("%")
                Else
                    AddChar("5")
                End If
            Case Keys.D6
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                If e.Shift = True Then
                    AddChar("^")
                Else
                    AddChar("6")
                End If
            Case Keys.D7
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                If e.Shift = True Then
                    AddChar("&")
                Else
                    AddChar("7")
                End If
            Case Keys.D8
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                If e.Shift = True Then
                    AddChar("*")
                Else
                    AddChar("8")
                End If
            Case Keys.D9
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                If e.Shift = True Then
                    AddChar("(")
                Else
                    AddChar("9")
                End If
            Case Keys.D0
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                If e.Shift = True Then
                    AddChar(")")
                Else
                    AddChar("0")
                End If
            Case Keys.NumLock
                e.Handled = True
            Case Keys.NumPad0
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                AddChar("0")
            Case Keys.NumPad1
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                AddChar("1")
            Case Keys.NumPad2
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                AddChar("2")
            Case Keys.NumPad3
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                AddChar("3")
            Case Keys.NumPad4
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                AddChar("4")
            Case Keys.NumPad5
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                AddChar("5")
            Case Keys.NumPad6
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                AddChar("6")
            Case Keys.NumPad7
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                AddChar("7")
            Case Keys.NumPad8
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                AddChar("8")
            Case Keys.NumPad9
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                AddChar("9")
            Case Keys.Divide
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                AddChar("/")
            Case Keys.Multiply
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                AddChar("*")
            Case Keys.Subtract
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                AddChar("-")
            Case Keys.Add
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                AddChar("+")
            Case Keys.F1
                e.Handled = True
            Case Keys.F2
                e.Handled = True
            Case Keys.F3
                e.Handled = True
            Case Keys.F4
                e.Handled = True
            Case Keys.F5
                e.Handled = True
            Case Keys.F6
                e.Handled = True
            Case Keys.F7
                e.Handled = True
            Case Keys.F9
                e.Handled = True
            Case Keys.F10
                e.Handled = True
            Case Keys.F11
                e.Handled = True
            Case Keys.F12
                e.Handled = True
            Case Keys.A
                e.Handled = True
                If e.Control Then
                    IsSelecting = True
                    SelLineStart = 0
                    SelStart = 0
                    SelLineStop = AllLines.Count - 1
                    SelStop = Len(AllLines(SelLineStart))
                Else
                    If IsSelecting Then DeleteSelection()
                    If e.Shift Then
                        AddChar("A")
                    Else
                        AddChar("a")
                    End If
                End If
            Case Keys.C
                e.Handled = True
                If e.Control Then
                    If IsSelecting Then Clipboard.SetText(CopySelection())
                Else
                    If IsSelecting Then DeleteSelection()
                    If e.Shift Then
                        AddChar("C")
                    Else
                        AddChar("c")
                    End If
                End If
            Case Keys.X
                e.Handled = True
                If e.Control Then
                    If IsSelecting Then
                        Clipboard.SetText(CopySelection())
                        DeleteSelection()
                    End If
                Else
                    If IsSelecting Then DeleteSelection()
                    If e.Shift Then
                        AddChar("X")
                    Else
                        AddChar("x")
                    End If
                End If
            Case Keys.V
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                If e.Control Then
                    tStr = Clipboard.GetText()
                    tStr = Replace(tStr, vbCrLf, vbLf)
                    tStr = Replace(tStr, vbLf, vbCrLf)

                    InsertText(tStr, CurLine, CurPos)
                Else
                    If e.Shift Then
                        AddChar("V")
                    Else
                        AddChar("v")
                    End If
                End If
            Case Else
                e.Handled = True
                If IsSelecting Then DeleteSelection()
                If e.Shift = True Then
                    AddChar(UCase(Chr(e.KeyValue)))
                Else
                    AddChar(LCase(Chr(e.KeyValue)))
                End If
        End Select
        BlinkerONOFF = True
        ReturnToCaret()

        ResizeScrollbars()

        Me.Invalidate()
    End Sub

    'Override for use of non-accessible key presses
    Protected Overrides Function ProcessCmdKey(ByRef msg As System.Windows.Forms.Message, keyData As System.Windows.Forms.Keys) As Boolean

        If keyData.ToString = "Left, Shift" Then
            If IsSelecting = False Then
                IsSelecting = True
                SelLineStart = CurLine
                SelStart = CurPos
            End If

            If CurPos > 0 Then
                CurPos -= 1
            Else
                If CurLine > 0 Then
                    CurLine -= 1
                    CurPos = Len(AllLines(CurLine))
                End If
            End If

            SelLineStop = CurLine
            SelStop = CurPos

            BlinkerONOFF = True
            Me.Invalidate()
            ReturnToCaret()
            Return True
        End If

        If keyData.ToString = "Right, Shift" Then
            If IsSelecting = False Then
                IsSelecting = True
                SelLineStart = CurLine
                SelStart = CurPos
            End If

            If CurPos < Len(AllLines(CurLine)) Then
                CurPos += 1
            Else
                If CurLine < AllLines.Count - 1 Then
                    CurLine += 1
                    CurPos = 0
                End If
            End If

            SelLineStop = CurLine
            SelStop = CurPos

            BlinkerONOFF = True
            Me.Invalidate()
            ReturnToCaret()
            Return True
        End If

        If keyData.ToString = "Up, Shift" Then
            If IsSelecting = False Then
                IsSelecting = True
                SelLineStart = CurLine
                SelStart = CurPos
            End If

            If CurLine > 0 Then
                CurLine -= 1
                If CurPos > Len(AllLines(CurLine)) Then CurPos = Len(AllLines(CurLine))
            End If

            SelLineStop = CurLine
            SelStop = CurPos

            BlinkerONOFF = True
            Me.Invalidate()
            ReturnToCaret()
            Return True
        End If
        If keyData.ToString = "Down, Shift" Then
            If IsSelecting = False Then
                IsSelecting = True
                SelLineStart = CurLine
                SelStart = CurPos
            End If

            If CurLine < AllLines.Count - 1 Then
                CurLine += 1
                If CurPos > Len(AllLines(CurLine)) Then CurPos = Len(AllLines(CurLine))
            End If

            SelLineStop = CurLine
            SelStop = CurPos

            BlinkerONOFF = True
            Me.Invalidate()
            ReturnToCaret()
            Return True
        End If
        Select Case keyData
            Case Keys.Tab
                AddChar(Chr(9))
                BlinkerONOFF = True
                Me.Invalidate()
                ReturnToCaret()
                Return True
            Case Keys.Up
                If CurLine > 0 Then
                    CurLine -= 1
                    If CurPos > Len(AllLines(CurLine)) Then CurPos = Len(AllLines(CurLine))
                End If
                BlinkerONOFF = True
                Me.Invalidate()
                ReturnToCaret()
                Return True
            Case Keys.Down
                If CurLine < AllLines.Count - 1 Then
                    CurLine += 1
                    If CurPos > Len(AllLines(CurLine)) Then CurPos = Len(AllLines(CurLine))
                End If
                BlinkerONOFF = True
                Me.Invalidate()
                ReturnToCaret()
                Return True
            Case Keys.Left
                If CurPos > 0 Then
                    CurPos -= 1
                Else
                    If CurLine > 0 Then
                        CurLine -= 1
                        CurPos = Len(AllLines(CurLine))
                    End If
                End If
                BlinkerONOFF = True
                Me.Invalidate()
                ReturnToCaret()
                Return True
            Case Keys.Right
                If CurPos < Len(AllLines(CurLine)) Then
                    CurPos += 1
                Else
                    If CurLine < AllLines.Count - 1 Then
                        CurLine += 1
                        CurPos = 0
                    End If
                End If
                BlinkerONOFF = True
                Me.Invalidate()
                ReturnToCaret()
                Return True
        End Select
        Return MyBase.ProcessCmdKey(msg, keyData)
    End Function

End Class
