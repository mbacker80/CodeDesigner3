Module GlobalConfiguration
    Private Configs() As ConfigSetting, CfgCount As Integer

    Public Structure ConfigSetting
        Dim SettingType As String
        Dim SettingName As String
        Dim SettingValue As String
        Dim Special As String
    End Structure

    Public Sub LoadConfig(ByRef SV As SyntaxView, ByRef mpAsm As MIPSAssembly)
        Dim ConfigFile As String, Lines() As String, sp() As String, rt As Integer
        Dim i As Integer

        ConfigFile = ""
        CfgCount = -1
        ReDim Configs(0)

        If My.Computer.FileSystem.FileExists(Application.StartupPath + "\CodeDesigner3.ini") Then
            rt = ReadFile(Application.StartupPath + "\CodeDesigner3.ini", ConfigFile)
            If rt < 0 Then GoTo readFailed
            Lines = Split(ConfigFile + vbCrLf, vbCrLf)
            For i = 0 To Lines.Count - 1
                sp = Split(Lines(i) + ";;;;;;", ";")
                If sp(0) <> "" Then
                    SetConfig(sp(0), sp(1), sp(2), sp(3))
                End If
            Next
        Else
readFailed:
            LoadDefault(mpAsm)
        End If
    End Sub

    Public Sub SaveConfig()
        Dim ConfigFile As String, i As Integer, rt As Integer

        ConfigFile = ""
        For i = 0 To CfgCount
            With Configs(i)
                ConfigFile += .SettingName + ";" + .SettingValue + ";" + .SettingType + ";" + .Special + vbCrLf
            End With
        Next

        rt = SaveFile(Application.StartupPath + "\CodeDesigner3.ini", ConfigFile)

    End Sub

    Public Sub SetConfig(SName As String, SVal As String, Optional SType As String = Nothing, Optional SSpecial As String = Nothing)
        Dim i As Integer

        For i = 0 To CfgCount
            With Configs(i)
                If .SettingName = SName Then
                    .SettingValue = SVal
                    If SType IsNot Nothing Then .SettingType = SType
                    If SSpecial IsNot Nothing Then .Special = SSpecial
                    Exit Sub
                End If
            End With
        Next

        CfgCount += 1
        ReDim Preserve Configs(CfgCount)
        With Configs(CfgCount)
            .SettingName = SName
            .SettingValue = SVal
            If SType IsNot Nothing Then .SettingType = SType
            If SSpecial IsNot Nothing Then .Special = SSpecial
        End With
    End Sub

    Public Function GetConfig(SName As String, Optional ByRef SType As String = Nothing, Optional ByRef SSpecial As String = Nothing) As String
        Dim i As Integer, ret As String

        ret = ""
        For i = 0 To CfgCount
            With Configs(i)
                If .SettingName = SName Then
                    If SType IsNot Nothing Then SType = .SettingType
                    If SSpecial IsNot Nothing Then SSpecial = .Special
                    Return .SettingValue
                End If
            End With
        Next

        Return ret
    End Function

    Public Function CopyConfigs(ByRef Cfgs() As ConfigSetting) As Integer
        Dim i As Integer

        If CfgCount < 0 Then Return -1
        ReDim Cfgs(CfgCount)
        For i = 0 To CfgCount
            With Cfgs(i)
                .SettingType = Configs(i).SettingType
                .SettingName = Configs(i).SettingName
                .SettingValue = Configs(i).SettingValue
                .Special = Configs(i).Special
            End With
        Next

        Return CfgCount
    End Function

    Public Sub ApplyChanges(ByRef Cfgs() As ConfigSetting, ByRef SV As SyntaxView, ByRef mpAsm As MIPSAssembly)
        Dim i As Integer

        CfgCount = Cfgs.Count - 1
        ReDim Configs(Cfgs.Count - 1)
        For i = 0 To CfgCount
            With Cfgs(i)
                Configs(i).SettingType = .SettingType
                Configs(i).SettingName = .SettingName
                Configs(i).SettingValue = .SettingValue
                Configs(i).Special = .Special
            End With
        Next

        SVResetConfig(SV, mpAsm)
    End Sub

    Public Sub LoadDefault(ByRef mpAsm As MIPSAssembly)
        CfgCount = -1
        ReDim Configs(0)

        SetConfig("ASM_Language", "MIPS32", "PS2")
        SetConfig("ASM_OutputFormat", "2%07X %08X")

        SetConfig("SVFont", "Courier New")
        SetConfig("SVFontSize", "12")
        SetConfig("SVFontColor", "FFffffff")
        SetConfig("SVBackColor", "FF111111")
        SetConfig("SVLineColor", "FF222222")
        SetConfig("SVSelColor", "FF6060C0")

        SetConfig("SVMultiComments", "FF00ff00",, "{i}")
        SetConfig("SVSingleComments", "FF00ff00",, "{i}")
        SetConfig("SVInvalid", "FFff0000",, "{u}")

        SetConfig("SVCOM:hexcode", "FFff00ff",, "{b}")
        SetConfig("SVCOM:hexfloat", "FFff00ff",, "{b}")
        SetConfig("SVCOM:code", "FFff00ff",, "{b}")
        SetConfig("SVCOM:setreg", "FFff00ff",, "{b}")
        SetConfig("SVCOM:setfpr", "FFff00ff",, "{b}")
        SetConfig("SVCOM:setfloat", "FFff00ff",, "{b}")
        SetConfig("SVCOM:address", "FFff00ff",, "{b}")
        SetConfig("SVCOM:addradd", "FFff00ff",, "{b}")
        SetConfig("SVCOM:memalign", "FFff00ff",, "{b}")
        SetConfig("SVCOM:alloc", "FFff00ff",, "{b}")
        SetConfig("SVCOM:malloc", "FFff00ff",, "{b}")
        SetConfig("SVCOM:string", "FFff00ff",, "{b}")
        SetConfig("SVCOM:print", "FFff00ff",, "{b}")
        SetConfig("SVCOM:padding", "FFff00ff",, "{b}")
        SetConfig("SVCOM:call", "FFff00ff",, "{b}")
        SetConfig("SVCOM:goto", "FFff00ff",, "{b}")
        SetConfig("SVCOM:define", "FFff00ff",, "{b}")

        SetConfig("SVCOM:prochook", "FFff00ff",, "{b}")
        SetConfig("SVCOM:hook", "FFff00ff",, "{b}")
        SetConfig("SVCOM:thread", "FFff00ff",, "{b}")
        SetConfig("SVCOM:thread.start", "FFff00ff",, "{b}")
        SetConfig("SVCOM:thread.stop", "FFff00ff",, "{b}")
        SetConfig("SVCOM:thread.sleep", "FFff00ff",, "{b}")
        SetConfig("SVCOM:thread.wakeup", "FFff00ff",, "{b}")
        SetConfig("SVCOM:event", "FFff00ff",, "{b}")

        SetConfig("SVCOM:elf.entry", "FFff00ff",, "{b}")
        SetConfig("SVCOM:resource", "FFff00ff",, "{b}")
        SetConfig("SVCOM:resource.path", "FFff00ff",, "{b}")

        SetConfig("SVCOM:import", "FFff00ff",, "{b}")

        SetConfig("SVCOM:if", "FFff00ff",, "{b}")
        SetConfig("SVCOM:else", "FFff00ff",, "{b}")
        SetConfig("SVARG:if", "FFff00ff")

        SetConfig("SVCOM:switch", "FFff00ff",, "{b}")
        SetConfig("SVCOM:case", "FFff00ff",, "{b}")
        SetConfig("SVCOM:default", "FFff00ff",, "{b}")
        SetConfig("SVCOM:%break", "FFff00ff",, "{b}")

        SetConfig("SVCOM:fnc", "FFff00ff",, "{b}")
        SetConfig("SVCOM:return", "FFff00ff",, "{b}")
        SetConfig("SVCOM:extern", "FFff00ff",, "{b}")

        SetConfig("SVCOM:for", "FFff00ff",, "{b}")
        SetConfig("SVCOM:while", "FFff00ff",, "{b}")

        SetConfig("SVCOM:%7b", "FFffff00",, "{b}")
        SetConfig("SVCOM:%7d", "FFffff00",, "{b}")



        SetConfig("SVARG:zero", "FFaaaaaa")
        SetConfig("SVARG:at", "FF00a070")
        SetConfig("SVARG:v0", "FFff8000")
        SetConfig("SVARG:v1", "FFff8000")
        SetConfig("SVARG:a0", "FF0080ff")
        SetConfig("SVARG:a1", "FF0080ff")
        SetConfig("SVARG:a2", "FF0080ff")
        SetConfig("SVARG:a3", "FF0080ff")
        SetConfig("SVARG:t0", "FF904090")
        SetConfig("SVARG:t1", "FF904090")
        SetConfig("SVARG:t2", "FF904090")
        SetConfig("SVARG:t3", "FF904090")
        SetConfig("SVARG:t4", "FF904090")
        SetConfig("SVARG:t5", "FF904090")
        SetConfig("SVARG:t6", "FF904090")
        SetConfig("SVARG:t7", "FF904090")
        SetConfig("SVARG:t8", "FF904090")
        SetConfig("SVARG:t9", "FF904090")
        SetConfig("SVARG:s0", "FFc0c040")
        SetConfig("SVARG:s1", "FFc0c040")
        SetConfig("SVARG:s2", "FFc0c040")
        SetConfig("SVARG:s3", "FFc0c040")
        SetConfig("SVARG:s4", "FFc0c040")
        SetConfig("SVARG:s5", "FFc0c040")
        SetConfig("SVARG:s6", "FFc0c040")
        SetConfig("SVARG:s7", "FFc0c040")
        SetConfig("SVARG:fp", "FFc0ffc0")
        SetConfig("SVARG:gp", "FF80ff00")
        SetConfig("SVARG:sp", "FFffff00")
        SetConfig("SVARG:k0", "FF808000")
        SetConfig("SVARG:k1", "FF808000")
        SetConfig("SVARG:ra", "FF900000")


        SetConfig("SVARG:ee", "FFff8080")
        SetConfig("SVARG:cop1", "FFff8080")
        SetConfig("SVARG:void", "FFff8080")



        Dim i As Integer
        For i = 0 To 258
            SetConfig("SVCOM:" + LCase(mpAsm.GetInstrName(i)), "FFffffff",, "{b}")
        Next


        SetConfig("SVCOM:at", "FF00a070",, "{b}")
        SetConfig("SVCOM:v0", "FFff8000",, "{b}")
        SetConfig("SVCOM:v1", "FFff8000",, "{b}")
        SetConfig("SVCOM:a0", "FF0080ff",, "{b}")
        SetConfig("SVCOM:a1", "FF0080ff",, "{b}")
        SetConfig("SVCOM:a2", "FF0080ff",, "{b}")
        SetConfig("SVCOM:a3", "FF0080ff",, "{b}")
        SetConfig("SVCOM:t0", "FF904090",, "{b}")
        SetConfig("SVCOM:t1", "FF904090",, "{b}")
        SetConfig("SVCOM:t2", "FF904090",, "{b}")
        SetConfig("SVCOM:t3", "FF904090",, "{b}")
        SetConfig("SVCOM:t4", "FF904090",, "{b}")
        SetConfig("SVCOM:t5", "FF904090",, "{b}")
        SetConfig("SVCOM:t6", "FF904090",, "{b}")
        SetConfig("SVCOM:t7", "FF904090",, "{b}")
        SetConfig("SVCOM:t8", "FF904090",, "{b}")
        SetConfig("SVCOM:t9", "FF904090",, "{b}")
        SetConfig("SVCOM:s0", "FFc0c040",, "{b}")
        SetConfig("SVCOM:s1", "FFc0c040",, "{b}")
        SetConfig("SVCOM:s2", "FFc0c040",, "{b}")
        SetConfig("SVCOM:s3", "FFc0c040",, "{b}")
        SetConfig("SVCOM:s4", "FFc0c040",, "{b}")
        SetConfig("SVCOM:s5", "FFc0c040",, "{b}")
        SetConfig("SVCOM:s6", "FFc0c040",, "{b}")
        SetConfig("SVCOM:s7", "FFc0c040",, "{b}")
        SetConfig("SVCOM:fp", "FFc0ffc0",, "{b}")
        SetConfig("SVCOM:gp", "FF80ff00",, "{b}")
        SetConfig("SVCOM:sp", "FFffff00",, "{b}")
        SetConfig("SVCOM:k0", "FF808000",, "{b}")
        SetConfig("SVCOM:k1", "FF808000",, "{b}")
        SetConfig("SVCOM:ra", "FF900000",, "{b}")

    End Sub




    Public Sub SVResetConfig(ByRef SV As SyntaxView, ByRef mpAsm As MIPSAssembly)
        Dim i As Integer, sp() As String

        SV.ClearSyntaxConfig()
        For i = 0 To CfgCount
            With Configs(i)
                If Strings.Left(.SettingName, 5) = "SVCOM" Then
                    sp = Split(.SettingName, ":")
                    SV.SetSyntaxCmdConfig(sp(1), Val("&H" + .SettingValue), .Special)
                ElseIf Strings.Left(.SettingName, 5) = "SVARG" Then
                    sp = Split(.SettingName, ":")
                    SV.SetSyntaxArgConfig(sp(1), Val("&H" + .SettingValue))
                Else
                    Select Case .SettingName
                        Case "SVFont"
                            SV.Font_Name = .SettingValue
                        Case "SVFontSize"
                            SV.Font_Size = Val(.SettingValue)
                        Case "SVFontColor"
                            SV.Font_Color = Val("&H" + .SettingValue)
                        Case "SVBackColor"
                            SV.Back_Color = Val("&H" + .SettingValue)
                        Case "SVLineColor"
                            SV.CurrentLineHLColor = Val("&H" + .SettingValue)
                        Case "SVSelColor"
                            SV.SelectionHighlightColor = Val("&H" + .SettingValue)
                    End Select
                End If
            End With
        Next
    End Sub

    Public Sub SVUseDefault(ByRef SV As SyntaxView, ByRef mpAsm As MIPSAssembly)

        SV.SetSyntaxCmdConfig("hexcode", Val("&HFFff00ff"), "{b}")
        SV.SetSyntaxCmdConfig("code", Val("&HFFff00ff"), "{b}")
        SV.SetSyntaxCmdConfig("setreg", Val("&HFFff00ff"), "{b}")
        SV.SetSyntaxCmdConfig("setfpr", Val("&HFFff00ff"), "{b}")
        SV.SetSyntaxCmdConfig("setfloat", Val("&HFFff00ff"), "{b}")
        SV.SetSyntaxCmdConfig("address", Val("&HFFff00ff"), "{b}")
        SV.SetSyntaxCmdConfig("alloc", Val("&HFFff00ff"), "{b}")
        SV.SetSyntaxCmdConfig("string", Val("&HFFff00ff"), "{b}")
        SV.SetSyntaxCmdConfig("print", Val("&HFFff00ff"), "{b}")
        SV.SetSyntaxCmdConfig("padding", Val("&HFFff00ff"), "{b}")
        SV.SetSyntaxCmdConfig("call", Val("&HFFff00ff"), "{b}")
        SV.SetSyntaxCmdConfig("goto", Val("&HFFff00ff"), "{b}")
        SV.SetSyntaxCmdConfig("define", Val("&HFFff00ff"), "{b}")

        SV.SetSyntaxCmdConfig("prochook", Val("&HFFff00ff"), "{b}")
        SV.SetSyntaxCmdConfig("hook", Val("&HFFff00ff"), "{b}")
        SV.SetSyntaxCmdConfig("thread", Val("&HFFff00ff"), "{b}")
        SV.SetSyntaxCmdConfig("thread.start", Val("&HFFff00ff"), "{b}")
        SV.SetSyntaxCmdConfig("thread.stop", Val("&HFFff00ff"), "{b}")
        SV.SetSyntaxCmdConfig("thread.sleep", Val("&HFFff00ff"), "{b}")
        SV.SetSyntaxCmdConfig("thread.wakeup", Val("&HFFff00ff"), "{b}")
        SV.SetSyntaxCmdConfig("event", Val("&HFFff00ff"), "{b}")

        SV.SetSyntaxCmdConfig("import", Val("&HFFff00ff"), "{b}")

        SV.SetSyntaxCmdConfig("if", Val("&HFFff00ff"), "{b}")
        SV.SetSyntaxCmdConfig("else", Val("&HFFff00ff"), "{b}")
        SV.SetSyntaxArgConfig("if", Val("&HFFff00ff"))

        SV.SetSyntaxCmdConfig("switch", Val("&HFFff00ff"), "{b}")
        SV.SetSyntaxCmdConfig("case", Val("&HFFff00ff"), "{b}")
        SV.SetSyntaxCmdConfig("default", Val("&HFFff00ff"), "{b}")
        SV.SetSyntaxCmdConfig("%break", Val("&HFFff00ff"), "{b}")

        SV.SetSyntaxCmdConfig("fnc", Val("&HFFff00ff"), "{b}")
        SV.SetSyntaxCmdConfig("return", Val("&HFFff00ff"), "{b}")
        SV.SetSyntaxCmdConfig("extern", Val("&HFFff00ff"), "{b}")

        SV.SetSyntaxCmdConfig("for", Val("&HFFff00ff"), "{b}")
        SV.SetSyntaxCmdConfig("while", Val("&HFFff00ff"), "{b}")

        SV.SetSyntaxCmdConfig("%7b", Val("&HFFffff00"), "{b}")
        SV.SetSyntaxCmdConfig("%7d", Val("&HFFffff00"), "{b}")

        SV.SetSyntaxArgConfig("zero", Val("&HFFaaaaaa"))
        SV.SetSyntaxArgConfig("at", Val("&HFF00a070"))
        SV.SetSyntaxArgConfig("v0", Val("&HFFff8000"))
        SV.SetSyntaxArgConfig("v1", Val("&HFFff8000"))
        SV.SetSyntaxArgConfig("a0", Val("&HFF0080ff"))
        SV.SetSyntaxArgConfig("a1", Val("&HFF0080ff"))
        SV.SetSyntaxArgConfig("a2", Val("&HFF0080ff"))
        SV.SetSyntaxArgConfig("a3", Val("&HFF0080ff"))
        SV.SetSyntaxArgConfig("t0", Val("&HFF904090"))
        SV.SetSyntaxArgConfig("t1", Val("&HFF904090"))
        SV.SetSyntaxArgConfig("t2", Val("&HFF904090"))
        SV.SetSyntaxArgConfig("t3", Val("&HFF904090"))
        SV.SetSyntaxArgConfig("t4", Val("&HFF904090"))
        SV.SetSyntaxArgConfig("t5", Val("&HFF904090"))
        SV.SetSyntaxArgConfig("t6", Val("&HFF904090"))
        SV.SetSyntaxArgConfig("t7", Val("&HFF904090"))
        SV.SetSyntaxArgConfig("t8", Val("&HFF904090"))
        SV.SetSyntaxArgConfig("t9", Val("&HFF904090"))
        SV.SetSyntaxArgConfig("s0", Val("&HFFc0c040"))
        SV.SetSyntaxArgConfig("s1", Val("&HFFc0c040"))
        SV.SetSyntaxArgConfig("s2", Val("&HFFc0c040"))
        SV.SetSyntaxArgConfig("s3", Val("&HFFc0c040"))
        SV.SetSyntaxArgConfig("s4", Val("&HFFc0c040"))
        SV.SetSyntaxArgConfig("s5", Val("&HFFc0c040"))
        SV.SetSyntaxArgConfig("s6", Val("&HFFc0c040"))
        SV.SetSyntaxArgConfig("s7", Val("&HFFc0c040"))
        SV.SetSyntaxArgConfig("fp", Val("&HFFc0ffc0"))
        SV.SetSyntaxArgConfig("gp", Val("&HFF80ff00"))
        SV.SetSyntaxArgConfig("sp", Val("&HFFffff00"))
        SV.SetSyntaxArgConfig("k0", Val("&HFF808000"))
        SV.SetSyntaxArgConfig("k1", Val("&HFF808000"))
        SV.SetSyntaxArgConfig("ra", Val("&HFF900000"))


        SV.SetSyntaxArgConfig("ee", Val("&HFFff8080"))
        SV.SetSyntaxArgConfig("cop1", Val("&HFFff8080"))
        SV.SetSyntaxArgConfig("void", Val("&HFFff8080"))

        Dim i As Integer
        For i = 0 To 258
            SV.SetSyntaxCmdConfig(mpAsm.GetInstrName(i), Val("&HFFffffff"), "{b}")
        Next
        SV.SetSyntaxCmdConfig("at", Val("&HFF00a070"), "{b}")
        SV.SetSyntaxCmdConfig("v0", Val("&HFFff8000"), "{b}")
        SV.SetSyntaxCmdConfig("v1", Val("&HFFff8000"), "{b}")
        SV.SetSyntaxCmdConfig("a0", Val("&HFF0080ff"), "{b}")
        SV.SetSyntaxCmdConfig("a1", Val("&HFF0080ff"), "{b}")
        SV.SetSyntaxCmdConfig("a2", Val("&HFF0080ff"), "{b}")
        SV.SetSyntaxCmdConfig("a3", Val("&HFF0080ff"), "{b}")
        SV.SetSyntaxCmdConfig("t0", Val("&HFF904090"), "{b}")
        SV.SetSyntaxCmdConfig("t1", Val("&HFF904090"), "{b}")
        SV.SetSyntaxCmdConfig("t2", Val("&HFF904090"), "{b}")
        SV.SetSyntaxCmdConfig("t3", Val("&HFF904090"), "{b}")
        SV.SetSyntaxCmdConfig("t4", Val("&HFF904090"), "{b}")
        SV.SetSyntaxCmdConfig("t5", Val("&HFF904090"), "{b}")
        SV.SetSyntaxCmdConfig("t6", Val("&HFF904090"), "{b}")
        SV.SetSyntaxCmdConfig("t7", Val("&HFF904090"), "{b}")
        SV.SetSyntaxCmdConfig("t8", Val("&HFF904090"), "{b}")
        SV.SetSyntaxCmdConfig("t9", Val("&HFF904090"), "{b}")
        SV.SetSyntaxCmdConfig("s0", Val("&HFFc0c040"), "{b}")
        SV.SetSyntaxCmdConfig("s1", Val("&HFFc0c040"), "{b}")
        SV.SetSyntaxCmdConfig("s2", Val("&HFFc0c040"), "{b}")
        SV.SetSyntaxCmdConfig("s3", Val("&HFFc0c040"), "{b}")
        SV.SetSyntaxCmdConfig("s4", Val("&HFFc0c040"), "{b}")
        SV.SetSyntaxCmdConfig("s5", Val("&HFFc0c040"), "{b}")
        SV.SetSyntaxCmdConfig("s6", Val("&HFFc0c040"), "{b}")
        SV.SetSyntaxCmdConfig("s7", Val("&HFFc0c040"), "{b}")
        SV.SetSyntaxCmdConfig("fp", Val("&HFFc0ffc0"), "{b}")
        SV.SetSyntaxCmdConfig("gp", Val("&HFF80ff00"), "{b}")
        SV.SetSyntaxCmdConfig("sp", Val("&HFFffff00"), "{b}")
        SV.SetSyntaxCmdConfig("k0", Val("&HFF808000"), "{b}")
        SV.SetSyntaxCmdConfig("k1", Val("&HFF808000"), "{b}")
        SV.SetSyntaxCmdConfig("ra", Val("&HFF900000"), "{b}")
    End Sub

    Public Function ReadFile(fName As String, ByRef fData As String) As Integer
        Dim F As System.IO.FileStream, fBytes() As Byte, rt As Integer

        Try
            If System.IO.File.Exists(fName) Then

                F = System.IO.File.Open(fName, System.IO.FileMode.Open)
                ReDim fBytes(F.Length)
                rt = F.Read(fBytes, 0, F.Length)
                F.Close()

                F.Dispose()

                'fData = System.Text.Encoding.UTF8.GetString(fBytes)
                fData = BytesToStr(fBytes)
                If Strings.Right(fData, 1) = Chr(0) Then fData = Strings.Left(fData, Len(fData) - 1)

                GC.Collect()
                Return 0
            Else
                Return -2
            End If
        Catch Ex As Exception
            frmMain.DebugOut(Ex.ToString)
            Return -1
        End Try

    End Function

    Public Function SaveFile(fName As String, fData As String) As Integer
        Dim F As System.IO.FileStream, fBytes() As Byte

        Try
            If System.IO.File.Exists(fName) Then
                Try
                    System.IO.File.Delete(fName)

                    F = System.IO.File.Create(fName)
                    'fBytes = New System.Text.UTF8Encoding(False).GetBytes(fData)
                    StrToBytes(fData, fBytes)
                    F.Write(fBytes, 0, fBytes.Length)
                    F.Close()

                    F.Dispose()
                    fBytes = Nothing
                Catch Ex As Exception
                    frmMain.DebugOut(Ex.ToString)
                    Return -1
                End Try
            Else

                Try
                    F = System.IO.File.Create(fName)
                    'fBytes = New System.Text.UTF8Encoding(False).GetBytes(fData)
                    StrToBytes(fData, fBytes)
                    F.Write(fBytes, 0, fBytes.Length)
                    F.Close()

                    F.Dispose()
                    fBytes = Nothing
                Catch Ex As Exception
                    frmMain.DebugOut(Ex.ToString)
                    Return -1
                End Try

            End If
        Catch Ex As Exception
            frmMain.DebugOut(Ex.ToString)
            Return -1
        End Try

        GC.Collect()
        Return 0
    End Function

    Public Function HexToStr(ByRef HexStr As String) As String
        Dim i As Integer, ret As String

        ret = ""
        For i = 1 To Len(HexStr)
            ret += Chr(Val("&H" + Strings.Mid(HexStr, i, 2)))
            i += 1
        Next

        Return ret
    End Function

    Public Function HexReverse(ByRef HexStr As String) As String
        Dim i As Integer, ret As String

        ret = ""
        For i = 1 To Len(HexStr)
            ret = Strings.Mid(HexStr, i, 2) + ret
            i += 1
        Next

        Return ret
    End Function

    Public Function StrToHex(ByRef Str As String) As String
        Dim i As Integer, ret As String

        ret = ""
        For i = 1 To Len(Str)
            ret += Strings.Right("00" + Hex(Asc(Strings.Mid(Str, i, 1))), 2)
        Next

        Return ret
    End Function

    Public Function StrToBytes(Str As String, ByRef Bytes() As Byte) As Integer
        Dim i As Integer, n As Integer

        n = Len(Str) - 1
        ReDim Bytes(n)
        For i = 1 To Len(Str)
            Bytes(i - 1) = Asc(Strings.Mid(Str, i, 1))
        Next

        Return n
    End Function

    Public Function BytesToStr(ByRef Bytes() As Byte) As String
        Dim i As Integer, ret As String

        ret = ""
        For i = 0 To Bytes.Count - 1
            ret += Chr(Bytes(i))
        Next

        Return ret
    End Function


    Public Function StringToCode(StartAddr As Int32, Bin As String) As String
        Dim i As Integer, ret As String, tStr As String, MemAddr As Int32

        MemAddr = StartAddr

        ret = ""
        For i = 1 To Len(Bin)
            tStr = HexReverse(StrToHex(Strings.Mid(Bin, i, 4)))
            ret += Strings.Right("00000000" + Hex(MemAddr), 8) + " " + tStr
            MemAddr += 4
        Next

        Return ret
    End Function

    Public Function CodeToBytes(Code As String, ByRef Bytes() As Byte) As Integer
        Dim i As Integer, Lines() As String, sp() As String, ret As String
        Dim n As Integer

        ret = ""

        n = -1
        Lines = Split(Code + vbCrLf, vbCrLf)
        For i = 0 To Lines.Count - 1
            sp = Split(Lines(i) + " ", " ")
            If sp(1) <> "" Then
                sp(1) = Strings.Right("00000000" + sp(1), 8)
                ret = HexToStr(HexReverse(sp(1)))
                n += 4
                ReDim Preserve Bytes(n)
                Bytes(n - 3) = Asc(Strings.Mid(ret, 1, 1))
                Bytes(n - 2) = Asc(Strings.Mid(ret, 2, 1))
                Bytes(n - 1) = Asc(Strings.Mid(ret, 3, 1))
                Bytes(n - 0) = Asc(Strings.Mid(ret, 4, 1))
            End If
        Next

        Return n
    End Function

    Public Function CodeToString(Code As String) As String
        Dim i As Integer, Lines() As String, sp() As String, ret As String

        ret = ""

        Lines = Split(Code + vbCrLf, vbCrLf)
        For i = 0 To Lines.Count - 1
            sp = Split(Lines(i) + " ", " ")
            If sp(1) <> "" Then
                sp(1) = Strings.Right("00000000" + sp(1), 8)
                ret += HexToStr(HexReverse(sp(1)))
            End If
        Next

        Return ret
    End Function

End Module
