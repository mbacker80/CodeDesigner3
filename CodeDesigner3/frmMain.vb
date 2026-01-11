
Imports System.ComponentModel

Public Class frmMain
    Private mpAsm As MIPSAssembly, mpCom As Compiler, btSav1 As Integer, btSav2 As Integer
    Private frmGoto As frmFindReplaceGoto, ps2Joker As frmPS2Controller

    Private IsProject As Boolean, MyProject As CDSProject, MyFile As String, LastFile As String
    Private frmNewItem_Mode As String, MyCurrentFile As String, wasSaved As Boolean

    Private Structure CDSFile
        Dim FileName As String
        Dim FileData As String
    End Structure
    Private Structure CDSProject
        Dim CDS() As CDSFile
        Dim CDSCount As Integer
        Dim ProjectPath As String
        Dim ProjectName As String

        Dim NewCDS As Func(Of String, Integer)
        Dim AddCDS As Func(Of String, String, Integer)
        Dim SetCDS As Func(Of String, String, Integer)
        Dim GetCDS As Func(Of String, String)
        Dim DelCDS As Func(Of String, Integer)
    End Structure

    Private Sub InitProject()
        With MyProject
            ReDim .CDS(0)
            .CDS(0).FileName = ""
            .CDS(0).FileData = ""
            .CDSCount = -1

            .NewCDS = Function(strName As String) As Integer
                          .CDSCount += 1
                          ReDim Preserve .CDS(.CDSCount)
                          .CDS(.CDSCount).FileName = strName
                          .CDS(.CDSCount).FileData = ""
                          Return .CDSCount
                      End Function

            .AddCDS = Function(strName As String, strData As String) As Integer
                          Dim i As Integer
                          For i = 0 To .CDSCount
                              If LCase(.CDS(i).FileName) = LCase(strName) Then Return -1
                          Next
                          i = .NewCDS(strName)
                          .CDS(i).FileData = strData
                          Return i
                      End Function

            .SetCDS = Function(strName As String, strData As String) As Integer
                          Dim i As Integer
                          For i = 0 To .CDSCount
                              If LCase(.CDS(i).FileName) = LCase(strName) Then
                                  .CDS(i).FileData = strData
                                  Return i
                              End If
                          Next
                          Return -1
                      End Function

            .GetCDS = Function(strName As String) As String
                          Dim i As Integer
                          For i = 0 To .CDSCount
                              If LCase(.CDS(i).FileName) = LCase(strName) Then
                                  Return .CDS(i).FileData
                              End If
                          Next

                          Return ""
                      End Function

            .DelCDS = Function(strName As String) As Integer
                          Dim i As Integer, i2 As Integer, t1 As String, t2 As String
                          For i = 0 To .CDSCount
                              If LCase(.CDS(i).FileName) = LCase(strName) Then
                                  .CDS(i).FileName = ""
                                  .CDS(i).FileData = ""
                                  For i2 = i To .CDSCount - 1
                                      t1 = .CDS(i2).FileName
                                      t2 = .CDS(i2).FileData
                                      .CDS(i2).FileName = .CDS(i2 + 1).FileName
                                      .CDS(i2).FileData = .CDS(i2 + 1).FileData
                                      .CDS(i2 + 1).FileName = t1
                                      .CDS(i2 + 1).FileData = t2
                                  Next
                                  ReDim Preserve .CDS(.CDSCount - 1)
                                  .CDSCount -= 1
                                  Return 0
                              End If
                          Next
                          Return -1
                      End Function
        End With
    End Sub

    Private Sub LeaveProject()
        IsProject = False
        ReDim MyProject.CDS(0)
        MyProject.CDSCount = -1
        MyProject.ProjectName = ""
        MyProject.ProjectPath = ""
        LastFile = ""
    End Sub

    Private Sub NewProject()
        LeaveProject()
        IsProject = True

        ReDim MyProject.CDS(0)
        MyProject.CDSCount = 0
        MyProject.ProjectName = "New Project"
        MyProject.ProjectPath = ""
        MyProject.CDS(0).FileName = "Main"
        MyProject.CDS(0).FileData = ""
        LastFile = ""

        RefreshProjectList()
    End Sub

    Private Sub RefreshProjectList()
        Dim CurIndex As Integer, I As Integer
        CurIndex = lstFiles.SelectedIndex

        lstFiles.Items.Clear()

        For I = 0 To MyProject.CDSCount
            lstFiles.Items.Add(MyProject.CDS(I).FileName)
        Next

        If CurIndex > lstFiles.Items.Count - 1 Then
            lstFiles.SelectedIndex = 0
        Else
            lstFiles.SelectedIndex = CurIndex
        End If
    End Sub

    Private Sub frmMain_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        InitProject()
        LeaveProject()
        LastFile = ""
        ProjectToolStripMenuItem.Enabled = False
        SaveToolStripMenuItem.Enabled = True
        SaveAsToolStripMenuItem.Enabled = True

        btSav1 = 6
        btSav2 = 6

        mpAsm = New MIPSAssembly
        mpCom = New Compiler

        mpAsm.init()
        mpCom.Init(mpAsm, AddressOf DebugOut)

        SyntaxView1.Init()
        lstFiles.Items.Clear()
        lstFiles.Items.Add("New File")
        lstFiles.SelectedIndex = 0

        LoadConfig(SyntaxView1, mpAsm)
        SVResetConfig(SyntaxView1, mpAsm)

        'SVUseDefault(SyntaxView1, mpAsm)
        GC.Collect()
        frmMain_Resize(sender, e)

        SyntaxView1.TxtSource = "/*" + vbCrLf +
            "" + vbTab + "CodeDesigner v3 ~ Created by: Gtlcpimp" + vbCrLf +
            "*/" + vbCrLf + vbCrLf

        Me.Text = "Code Designer 3 (Classic Mode) ~ Created by: Gtlcpimp"

    End Sub

    Private Sub frmMain_Resize(sender As Object, e As EventArgs) Handles Me.Resize

        If lstFiles.Visible Then
            lstFiles.Left = 0
            lstFiles.Top = MenuStrip1.Height

            If txtDebug.Visible = True Then
                lstFiles.Height = Me.ClientRectangle.Height - (txtDebug.Height + MenuStrip1.Height)
            Else
                lstFiles.Height = Me.ClientRectangle.Height - MenuStrip1.Height
            End If
            SyntaxView1.Height = lstFiles.Height

            txtDebug.Left = 0
            txtDebug.Top = lstFiles.Top + lstFiles.Height
            txtDebug.Width = Me.ClientRectangle.Width - txtCodeOutput.Width

            SyntaxView1.Left = lstFiles.Width
            SyntaxView1.Top = lstFiles.Top
            SyntaxView1.Width = Me.ClientRectangle.Width - (lstFiles.Width + txtCodeOutput.Width)
            txtCodeOutput.Top = MenuStrip1.Height
            txtCodeOutput.Left = txtDebug.Left + txtDebug.Width
            txtCodeOutput.Height = Me.ClientRectangle.Height - MenuStrip1.Height

        Else

            SyntaxView1.Left = 0
            SyntaxView1.Top = MenuStrip1.Height
            SyntaxView1.Width = Me.ClientRectangle.Width - txtCodeOutput.Width
            If txtDebug.Visible = True Then
                SyntaxView1.Height = Me.ClientRectangle.Height - (MenuStrip1.Height + txtDebug.Height)
                txtDebug.Top = SyntaxView1.Top + SyntaxView1.Height
                txtDebug.Left = 0
                txtDebug.Width = Me.ClientRectangle.Width - txtCodeOutput.Width
            Else
                SyntaxView1.Height = Me.ClientRectangle.Height - MenuStrip1.Height
            End If

            txtCodeOutput.Top = MenuStrip1.Height
            txtCodeOutput.Left = SyntaxView1.Left + SyntaxView1.Width
            txtCodeOutput.Height = Me.ClientRectangle.Height - (MenuStrip1.Height + (Button2.Height * 4) + (btSav2 * 5))
            Button1.Top = txtCodeOutput.Top + txtCodeOutput.Height + btSav2
            Button3.Top = Button1.Top + Button1.Height + btSav2
            Button5.Top = Button3.Top + Button3.Height + btSav2
            Button7.Top = Button5.Top + Button5.Height + btSav2
            Button1.Left = txtCodeOutput.Left
            Button3.Left = Button1.Left
            Button5.Left = Button3.Left
            Button7.Left = Button5.Left

            Button2.Top = Button1.Top
            Button4.Top = Button3.Top
            Button6.Top = Button5.Top
            Button8.Top = Button7.Top

            Button2.Left = Button1.Left + Button1.Width + btSav1
            Button4.Left = Button3.Left + Button3.Width + btSav1
            Button6.Left = Button5.Left + Button5.Width + btSav1
            Button8.Left = Button7.Left + Button7.Width + btSav1

        End If
    End Sub

    Private Sub ExitToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ExitToolStripMenuItem.Click
        Dim rt As Integer

        rt = MsgBox("Any unsaved changes will" + vbCrLf +
                    "be lost! Continue?", vbOKCancel, "Save Before Quit")
        If rt = 2 Then Exit Sub

        End
    End Sub


    Private Sub HandleError(CompilerType As Integer)
        Dim PageName As String, LineNumber As Integer, LineData As String
        Dim ErrorNumber As Integer, ErrorString As String, ExtraDetails As String

        ErrorString = mpCom.RetreiveError(PageName, ErrorNumber, LineNumber, LineData, ExtraDetails)

        If CompilerType = 0 Then 'Selected File

            DebugOut("[Syntax Error] Line (" + (LineNumber + 1).ToString + ") " + ErrorString)
            If ExtraDetails <> "" Then DebugOut("[Notes] " + ExtraDetails)
            DebugOut("[Line Contents] " + LineData)
            SyntaxView1.GotoLine(LineNumber)
            If txtDebug.Visible = False Then
                txtCodeOutput.Text = "[Syntax Error] Line (" + (LineNumber + 1).ToString + ") " + vbCrLf +
                                     ErrorString + vbCrLf
            End If
        ElseIf CompilerType = 1 Then 'Entire Project
            Dim i As Integer

            DebugOut("[Syntax Error] File '" + PageName + "' on line (" + (LineNumber + 1).ToString + ") " + ErrorString)
            If ExtraDetails <> "" Then DebugOut("[Notes] " + ExtraDetails)
            DebugOut("[Line Contents] " + LineData)

            If txtDebug.Visible = False Then
                txtCodeOutput.Text = "[Syntax Error] Line (" + (LineNumber + 1).ToString + ") " + vbCrLf +
                                     ErrorString + vbCrLf
            End If

            For i = 0 To lstFiles.Items.Count - 1
                If LCase(lstFiles.Items(i).ToString) = LCase(PageName) Then
                    lstFiles.SelectedIndex = i

                    SyntaxView1.TxtSource = MyProject.GetCDS(PageName)
                    SyntaxView1.GotoLine(LineNumber)
                    Exit Sub
                End If
            Next

        End If

    End Sub


    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Dim txtData As String, ret As String, rt As Integer

        txtCodeOutput.Text = ""
        DebugOut("Compiling file...")

        txtData = SyntaxView1.TxtSource

        If IsProject Then
            rt = MyProject.SetCDS(MyFile, txtData)
        End If

        rt = mpCom.CompileSingle(txtData, ret, False)
        If rt < 0 Then
            DebugOut("Compiler Error!")
            HandleError(0)
        Else
            txtCodeOutput.Text = ret
            DebugOut("Compile Complete")
        End If

        GC.Collect()
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        If txtCodeOutput.Text = "" Then Exit Sub
        Clipboard.SetText(txtCodeOutput.Text)
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        Dim rt As Integer
        rt = MsgBox("Disassembling code will erase" + vbCrLf +
                    "unsaved changes! Continue?", vbOKCancel, "Decompile")

        If rt = 2 Then Exit Sub

        SyntaxView1.TxtSource = mpCom.Decompile(txtCodeOutput.Text)
        SyntaxView1.Refresh()
    End Sub

    Private Sub BlankProjectToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles BlankProjectToolStripMenuItem.Click
        Dim rt As Integer
        If IsProject = False Then
            MsgBox("Not currently in project mode")
            Exit Sub
        End If

        rt = MsgBox("Any unsaved changes will" + vbCrLf +
                    "be lost! Continue?", vbOKCancel, "New Project")
        If rt = 2 Then Exit Sub

        NewProject()

    End Sub

    Private Sub BlankFileToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles BlankFileToolStripMenuItem.Click
        If IsProject = False Then
            MsgBox("No project is currently open", vbOKOnly, "Add Project File")
            Exit Sub
        End If

        frmNewItem_Mode = "BlankFile"

        Dim fNewBlankFile As New frmNewItem
        fNewBlankFile.SetMode(frmNewItem_Mode, "New File")
        fNewBlankFile.ShowDialog()
        GC.Collect()

    End Sub

    Private Sub Button6_Click(sender As Object, e As EventArgs) Handles Button6.Click
        Dim rt As Integer

        If MyCurrentFile = "" Then
            Button8_Click(sender, e)
        Else
            rt = SaveFile(MyCurrentFile, SyntaxView1.TxtSource)
            If rt = 0 Then DebugOut("File '" + MyCurrentFile + "' saved successfully!")
            If rt < 0 Then DebugOut("File '" + MyCurrentFile + "' save failed!")
            If rt = 0 Then wasSaved = True
        End If
    End Sub

    Private Sub Button8_Click(sender As Object, e As EventArgs) Handles Button8.Click
        Dim rt As Integer

        With SaveFileDialog1
            .FileName = ""
            .Filter = "CodeDesigner Source|*.cds"

            rt = .ShowDialog()

            If rt = 2 Then Exit Sub
            MyCurrentFile = .FileName

            rt = SaveFile(MyCurrentFile, SyntaxView1.TxtSource)
            If rt = 0 Then DebugOut("File '" + MyCurrentFile + "' saved successfully!")
            If rt < 0 Then DebugOut("File '" + MyCurrentFile + "' save failed!")
            If rt = 0 Then wasSaved = True
        End With
    End Sub

    Private Sub EntireProjectToolStripMenuItem1_Click(sender As Object, e As EventArgs) Handles EntireProjectToolStripMenuItem1.Click
        If IsProject = False Then
            MsgBox("Not currently in project mode")
            Exit Sub
        End If

        Dim i As Integer, fileDatas() As String, fileNames() As String, rt As Integer, ret As String

        txtCodeOutput.Text = ""
        DebugOut("Compiling project '" + MyProject.ProjectName + "'...")

        rt = MyProject.SetCDS(MyFile, SyntaxView1.TxtSource)
        If rt < 0 Then DebugOut("Error: Project file '" + MyFile + "' doesn't exist")

        ReDim fileDatas(MyProject.CDSCount)
        ReDim fileNames(MyProject.CDSCount)

        For i = 0 To MyProject.CDSCount
            fileNames(i) = MyProject.CDS(i).FileName
            fileDatas(i) = MyProject.CDS(i).FileData
        Next

        rt = mpCom.CompileProject(fileNames, fileDatas, ret)
        If rt < 0 Then
            DebugOut("Compiler Error!")
            HandleError(1)
        Else
            txtCodeOutput.Text = ret
            DebugOut("Compile Complete")
        End If

        GC.Collect()
    End Sub

    Private Sub SelectedFileToolStripMenuItem2_Click(sender As Object, e As EventArgs) Handles SelectedFileToolStripMenuItem2.Click
        If IsProject = False Then
            MsgBox("Not currently in project mode")
            Exit Sub
        End If

        Dim txtData As String, ret As String, rt As Integer


        DebugOut("Compiling file '" + MyFile + "'...")


        txtData = SyntaxView1.TxtSource

        rt = MyProject.SetCDS(MyFile, txtData)
        If rt < 0 Then DebugOut("Error: Project file '" + MyFile + "' doesn't exist")

        rt = mpCom.CompileSingle(txtData, ret, False)
        If rt < 0 Then
            DebugOut("Compiler Error!")
            HandleError(0)
        Else
            txtCodeOutput.Text = ret
            DebugOut("Compile Complete")
        End If

        GC.Collect()

    End Sub

    Private Sub lstFiles_SelectedIndexChanged(sender As Object, e As EventArgs) Handles lstFiles.SelectedIndexChanged
        If IsProject = False Then Exit Sub

        If LastFile <> "" Then MyProject.SetCDS(LastFile, SyntaxView1.TxtSource)
        MyFile = lstFiles.SelectedItem.ToString
        LastFile = MyFile
        SyntaxView1.TxtSource = MyProject.GetCDS(MyFile)

    End Sub
    Private Sub lstFiles_MouseUp(sender As Object, e As MouseEventArgs) Handles lstFiles.MouseUp
        If e.Button.ToString = "Right" Then
            ContextMenuStrip1.Show(lstFiles, e.X, e.Y)
        End If

    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        Dim rt As Integer, fData As String

        With OpenFileDialog1
            .FileName = ""
            .Filter = "CodeDesigner Source|*.cds"

            rt = .ShowDialog
            If rt = 2 Then Exit Sub

            rt = ReadFile(.FileName, fData)
            If rt < 0 Then
                If rt = -2 Then DebugOut("File '" + .FileName + "' doesn't exist.")
                Exit Sub
            End If

            MyCurrentFile = .FileName
            SyntaxView1.TxtSource = fData
        End With
    End Sub

    Private Sub NewToolStripMenuItem1_Click(sender As Object, e As EventArgs) Handles NewToolStripMenuItem1.Click
        If IsProject Then Exit Sub

        Dim rt As Integer

        rt = MsgBox("Any unsaved changes will" + vbCrLf +
                    "be lost! Continue?", vbOKCancel, "New")
        If rt = 2 Then Exit Sub

        MyCurrentFile = ""
        SyntaxView1.TxtSource = ""

    End Sub

    Private Sub OpenToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles OpenToolStripMenuItem.Click
        If IsProject Then Exit Sub

        Button4_Click(sender, e)
    End Sub

    Private Sub SaveToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles SaveToolStripMenuItem.Click
        If IsProject Then
            MsgBox("Not in classic mode")
            Exit Sub
        Else
            Button6_Click(sender, e)
        End If
    End Sub

    Private Sub SaveAsToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles SaveAsToolStripMenuItem.Click
        If IsProject Then
            MsgBox("Not in classic mode")
            Exit Sub
        Else
            Button8_Click(sender, e)
        End If
    End Sub

    Private Sub SelectedFileToolStripMenuItem1_Click(sender As Object, e As EventArgs) Handles SelectedFileToolStripMenuItem1.Click
        If lstFiles.SelectedIndex < 0 Then Exit Sub

        Dim rt As Integer
        rt = MsgBox("Are you sure you wish to" + vbCrLf +
                    "remove the selected file?", vbOKCancel, "Remove File")
        If rt = 2 Then Exit Sub

        If lstFiles.SelectedIndex = 0 And MyProject.CDSCount = 0 Then
            MsgBox("Cannot remove the only 1 file of the project", vbOKCancel, "Remove File")
            Exit Sub
        End If

        MyProject.DelCDS(lstFiles.Items(lstFiles.SelectedIndex).ToString)
        RefreshProjectList()

    End Sub

    Private Sub OpenProjectToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles OpenProjectToolStripMenuItem.Click
        If IsProject = False Then
            MsgBox("Not currently in project mode")
            Exit Sub
        End If
        Dim rt As Integer, i As Integer, sp() As String, projData As String

        rt = MsgBox("Any unsaved changes will" + vbCrLf +
                    "be lost! Continue?", vbOKCancel, "New Project")
        If rt = 2 Then Exit Sub

        With OpenFileDialog1
            .InitialDirectory = Application.StartupPath + "\Projects\"
            .FileName = ""
            .Filter = "CodeDesigner Projects|*.cdp"
            rt = .ShowDialog
            If rt = 2 Then Exit Sub

            sp = Split("\" + .FileName, "\")

            LeaveProject()
            IsProject = True

            MyProject.ProjectPath = Strings.Left(.FileName, Len(.FileName) - Len(sp(sp.Count - 1)))
            MyProject.ProjectName = sp(sp.Count - 1)
            If LCase(Strings.Right(MyProject.ProjectName, 4)) = ".cdp" Then MyProject.ProjectName = Strings.Left(MyProject.ProjectName, Len(MyProject.ProjectName) - 4)

            Do Until Strings.Right(MyProject.ProjectPath, 1) <> "\"
                MyProject.ProjectPath = Strings.Left(MyProject.ProjectPath, Len(MyProject.ProjectPath) - 1)
            Loop

            Try
                DebugOut("Loading '" + MyProject.ProjectPath + "\" + MyProject.ProjectName + "'...")
                rt = ReadFile(MyProject.ProjectPath + "\" + MyProject.ProjectName + ".cdp", projData)
                If rt < 0 Then
                    DebugOut("Failed reading project file")
                Else
                    sp = Split(projData + vbCrLf, vbCrLf)
                    For i = 0 To sp.Count - 1
                        If LCase(Strings.Right(sp(i), 4)) = ".cds" Then sp(i) = Strings.Left(sp(i), Len(sp(i)) - 4)

                        If sp(i) <> "" Then
                            rt = ReadFile(MyProject.ProjectPath + "\" + sp(i) + ".cds", projData)
                            If rt < 0 Then
                                DebugOut("Failed to load '" + MyProject.ProjectPath + "\" + sp(i) + ".cds'")
                            Else
                                rt = MyProject.AddCDS(sp(i), projData)
                                If rt < 0 Then
                                    DebugOut("Failed to loaded '" + MyProject.ProjectPath + "\" + sp(i) + ".cds'")
                                Else
                                    DebugOut("Loaded '" + sp(i) + ".cds' successfully!")
                                End If
                            End If
                        End If
                    Next
                End If
                RefreshProjectList()
            Catch ex As Exception
                MyProject.ProjectPath = ""
                MyProject.ProjectName = ""
                DebugOut("Error loading project '" + .FileName + "'")
            End Try

        End With

    End Sub

    Private Sub SaveProjectToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles SaveProjectToolStripMenuItem.Click
        If IsProject = False Then
            MsgBox("Not currently in proejct mode")
            Exit Sub
        End If
        Dim i As Integer, sp() As String, saveFolder As String, projSave As String, rt As Integer

        If lstFiles.SelectedIndex > -1 Then
            MyProject.SetCDS(lstFiles.Items(lstFiles.SelectedIndex).ToString, SyntaxView1.TxtSource)
        End If

        If MyProject.ProjectPath = "" Then
            With SaveFileDialog1
                Try
                    If My.Computer.FileSystem.DirectoryExists(Application.StartupPath + "\Projects") = False Then
                        My.Computer.FileSystem.CreateDirectory(Application.StartupPath + "\Projects")
                    End If
                    .InitialDirectory = Application.StartupPath + "\Projects"
                    .FileName = "New Project"
                    .Filter = "CodeDesigner Project|*.cdp"
                    i = .ShowDialog
                    If i = 2 Then Exit Sub
                    If LCase(Strings.Right(.FileName, 4)) = ".cdp" Then .FileName = Strings.Left(.FileName, Len(.FileName) - 4)

                    sp = Split("\" + .FileName, "\")
                    If LCase(Strings.Right(sp(sp.Count - 1), 4)) = ".cdp" Then sp(sp.Count - 1) = Strings.Left(sp(sp.Count - 1), Len(sp(sp.Count - 1)) - 4)

                    saveFolder = Strings.Left(.FileName, Len(.FileName) - Len(sp(sp.Count - 1)))

                    If LCase(.FileName) = LCase(Application.StartupPath + "\Projects\" + sp(sp.Count - 1) + ".cdp") Then
                        If My.Computer.FileSystem.DirectoryExists(Application.StartupPath + "\Projects\" + sp(sp.Count - 1)) Then
                            i = MsgBox("A project directory already exists" + vbCrLf +
                                       "with this name. Save to it anyway?", vbOKCancel, "Save Project")
                            If i = 2 Then Exit Sub

                            saveFolder = Application.StartupPath + "\Projects\" + sp(sp.Count - 1)
                        Else
                            My.Computer.FileSystem.CreateDirectory(Application.StartupPath + "\Projects\" + sp(sp.Count - 1))
                            saveFolder = Application.StartupPath + "\Projects\" + sp(sp.Count - 1)

                        End If
                    End If

                    MyProject.ProjectPath = saveFolder
                    MyProject.ProjectName = sp(sp.Count - 1) ' + ".cdp"

                    projSave = ""
                    For i = 0 To MyProject.CDSCount
                        projSave += MyProject.CDS(i).FileName + vbCrLf
                        If LCase(Strings.Right(MyProject.CDS(i).FileName, 4)) = ".cds" Then MyProject.CDS(i).FileName = Strings.Left(MyProject.CDS(i).FileName, Len(MyProject.CDS(i).FileName) - 4)
                        rt = SaveFile(saveFolder + "\" + MyProject.CDS(i).FileName + ".cds", MyProject.CDS(i).FileData)
                        If rt < 0 Then
                            DebugOut("Save '" + saveFolder + "\" + MyProject.CDS(i).FileName + ".cds' failed!")
                        Else
                            DebugOut("Saved '" + MyProject.CDS(i).FileName + ".cds' successfully!")
                        End If
                    Next
                    rt = SaveFile(saveFolder + "\" + MyProject.ProjectName + ".cdp", projSave)
                    If rt < 0 Then
                        MyProject.ProjectPath = ""
                        MyProject.ProjectName = ""
                        DebugOut("Project file '" + MyProject.ProjectPath + "\" + MyProject.ProjectName + ".cdp' save failed!")
                    Else
                        DebugOut("Project file '" + MyProject.ProjectName + ".cdp' saved successfully!")
                        wasSaved = True
                    End If

                Catch Ex As Exception
                    MyProject.ProjectPath = ""
                    MyProject.ProjectName = ""
                    DebugOut(Ex.ToString)
                    DebugOut("Error occurred during saving.")
                End Try
            End With
        Else
            Try
                projSave = ""
                For i = 0 To MyProject.CDSCount
                    If LCase(Strings.Right(MyProject.CDS(i).FileName, 4)) = ".cds" Then MyProject.CDS(i).FileName = Strings.Left(MyProject.CDS(i).FileName, Len(MyProject.CDS(i).FileName) - 4)

                    projSave += MyProject.CDS(i).FileName + vbCrLf
                    rt = SaveFile(MyProject.ProjectPath + "\" + MyProject.CDS(i).FileName + ".cds", MyProject.CDS(i).FileData)
                    If rt < 0 Then
                        DebugOut("Save '" + MyProject.ProjectPath + "\" + MyProject.CDS(i).FileName + ".cds' failed!")
                    Else
                        DebugOut("Saved '" + MyProject.CDS(i).FileName + ".cds' successfully!")
                    End If
                Next

                If LCase(Strings.Left(MyProject.ProjectName, 4)) = ".cdp" Then MyProject.ProjectName = Strings.Left(MyProject.ProjectName, Len(MyProject.ProjectName) - 4)
                rt = SaveFile(MyProject.ProjectPath + "\" + MyProject.ProjectName + ".cdp", projSave)
                If rt < 0 Then
                    MyProject.ProjectPath = ""
                    MyProject.ProjectName = ""
                    DebugOut("Project file '" + MyProject.ProjectPath + "\" + MyProject.ProjectName + ".cdp' save failed!")
                Else
                    DebugOut("Project file '" + MyProject.ProjectName + ".cdp' saved successfully!")
                    wasSaved = True
                End If
            Catch ex As Exception
                MyProject.ProjectPath = ""
                MyProject.ProjectName = ""
                DebugOut(Ex.ToString)
                DebugOut("Error occurred during saving.")
            End Try
        End If

    End Sub

    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click
        Dim txtData As String, ret As String, rt As Integer, Lines() As String, sp() As String

        txtCodeOutput.Text = ""
        DebugOut("Compiling file...")

        txtData = SyntaxView1.TxtSource

        If IsProject Then
            rt = MyProject.SetCDS(MyFile, txtData)
        End If

        rt = mpCom.CompileSingle(txtData, ret, False)
        If rt < 0 Then
            DebugOut("Compiler Error!")
            HandleError(0)
        Else
            Dim Bytes() As Byte, i As Integer, b As Integer, FS As System.IO.FileStream

            DebugOut("Compile completed.")
            If ret = "" Then Exit Sub
            If ret = vbCrLf Then Exit Sub

            ReDim Bytes(0)
            b = -1
            Lines = Split(ret + vbCrLf, vbCrLf)
            For i = 0 To Lines.Count - 1
                sp = Split(Lines(i) + " ", " ")
                If sp(1) <> "" Then
                    b += 4
                    ReDim Preserve Bytes(b)
                    Bytes(b - 0) = Val("&H" + Strings.Mid(sp(1), 1, 2))
                    Bytes(b - 1) = Val("&H" + Strings.Mid(sp(1), 3, 2))
                    Bytes(b - 2) = Val("&H" + Strings.Mid(sp(1), 5, 2))
                    Bytes(b - 3) = Val("&H" + Strings.Mid(sp(1), 7, 2))
                End If
            Next

            With SaveFileDialog1
                .FileName = ""
                .Filter = "RAW Files|*.raw|All Files|*.*"
                rt = .ShowDialog
                If rt = 2 Then Exit Sub

                Try
                    If My.Computer.FileSystem.FileExists(.FileName) Then My.Computer.FileSystem.DeleteFile(.FileName)

                    FS = System.IO.File.OpenWrite(.FileName)
                    FS.Write(Bytes, 0, Bytes.Length)
                    FS.Close()

                    DebugOut("RAW Exported successfully!")
                Catch Ex As Exception
                    DebugOut(Ex.ToString)
                    DebugOut("RAW Export failed!")
                End Try
            End With
        End If

        GC.Collect()
    End Sub

    Private Sub Button7_Click(sender As Object, e As EventArgs) Handles Button7.Click
        Dim rt As Integer, i As Integer, FS As System.IO.FileStream, Bytes() As Byte
        Dim ret As String, tStr As String, fakeAddr As String

        rt = MsgBox("Importing a RAW will erase" + vbCrLf +
                    "unsaved changes! Continue?", vbOKCancel, "Import")
        If rt = 2 Then Exit Sub

        With OpenFileDialog1
            .FileName = ""
            .Filter = "RAW Files|*.raw|All Files|*.*"
            rt = .ShowDialog
            If rt = 2 Then Exit Sub

            Try
                FS = System.IO.File.OpenRead(.FileName)
                ReDim Bytes(FS.Length - 1)
                FS.Read(Bytes, 0, FS.Length)
                FS.Close()

                If Bytes.Count < 4 Then
                    DebugOut("File too small")
                    Exit Sub
                End If

                If Bytes.Count > 40000 Then
                    DebugOut("File too large")
                    Exit Sub
                End If

                Do Until (Bytes.Count / 4) = (Bytes.Count \ 4)
                    ReDim Preserve Bytes(Bytes.Count)
                Loop

                ret = ""
                For i = 0 To Bytes.Count - 1 Step 4
                    tStr = Strings.Right("00" + Hex(Bytes(i + 3)), 2)
                    tStr += Strings.Right("00" + Hex(Bytes(i + 2)), 2)
                    tStr += Strings.Right("00" + Hex(Bytes(i + 1)), 2)
                    tStr += Strings.Right("00" + Hex(Bytes(i + 0)), 2)

                    fakeAddr = Strings.Right("00000000" + Hex(i), 8)

                    ret += fakeAddr + " " + tStr + vbCrLf
                Next
                SyntaxView1.TxtSource = mpCom.Decompile(ret)

            Catch ex As Exception
                DebugOut(ex.ToString)
                DebugOut("Import failed.")
            End Try

        End With

    End Sub


    Private Sub CreateFromProjectToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CreateFromProjectToolStripMenuItem.Click
        If IsProject = False Then
            MsgBox("Not in project mode")
            Exit Sub
        End If


        Dim i As Integer, fileDatas() As String, fileNames() As String, rt As Integer, ret As String
        Dim Names() As String, Src() As String, fBA As New frmBuildArchive

        txtCodeOutput.Text = ""

        rt = MyProject.SetCDS(MyFile, SyntaxView1.TxtSource)
        If rt < 0 Then DebugOut("Error: Project file '" + MyFile + "' doesn't exist")

        ReDim fileDatas(MyProject.CDSCount)
        ReDim fileNames(MyProject.CDSCount)

        For i = 0 To MyProject.CDSCount
            fileNames(i) = MyProject.CDS(i).FileName
            fileDatas(i) = MyProject.CDS(i).FileData
        Next

        rt = mpCom.CompileProject(fileNames, fileDatas, ret)
        If rt < 0 Then
            MsgBox("Cannot build libraries until" + vbCrLf +
                   "all errors are corrected!", vbOKOnly, "Create Lib")

            DebugOut("Compiler Error!")
            HandleError(1)
        Else
            ReDim Names(MyProject.CDSCount)
            ReDim Src(MyProject.CDSCount)
            For i = 0 To MyProject.CDSCount
                Names(i) = MyProject.CDS(i).FileName
                Src(i) = MyProject.CDS(i).FileData
            Next

            fBA.SetData(Names, Src, mpCom)
            fBA.ShowDialog()
        End If

        GC.Collect()
    End Sub

    Private Sub ExistingFileToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ExistingFileToolStripMenuItem.Click
        If IsProject = False Then
            MsgBox("Not in project mode")
            Exit Sub
        End If


        frmNewItem_Mode = "ExistingFile"

        Dim fNewBlankFile As New frmNewItem
        fNewBlankFile.SetMode(frmNewItem_Mode, "Existing File")
        fNewBlankFile.ShowDialog()
        GC.Collect()

    End Sub

    Private Sub PasteToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles PasteToolStripMenuItem.Click
        SyntaxView1.Paste()
    End Sub

    Private Sub CutToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CutToolStripMenuItem.Click
        SyntaxView1.Cut()
    End Sub

    Private Sub CopyToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CopyToolStripMenuItem.Click
        SyntaxView1.Copy()
    End Sub

    Private Sub DeleteToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles DeleteToolStripMenuItem.Click
        SyntaxView1.Delete()
    End Sub

    Private Sub SelectAllToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles SelectAllToolStripMenuItem.Click
        SyntaxView1.SelectAll()
    End Sub

    Private Sub SaveSelectedFileToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles SaveSelectedFileToolStripMenuItem.Click
        Dim rt As Integer

        If MyProject.ProjectPath = "" Then
            MsgBox("Project hasn't been saved yet!")
            Exit Sub
        End If

        MyProject.SetCDS(lstFiles.SelectedIndex, SyntaxView1.TxtSource)
        rt = SaveFile(MyProject.ProjectPath + "\" + lstFiles.Items(lstFiles.SelectedIndex), MyProject.GetCDS(lstFiles.Items(lstFiles.SelectedIndex)))
        If rt < 0 Then
            DebugOut("Error saving '" + MyProject.ProjectPath + "\" + lstFiles.Items(lstFiles.SelectedIndex) + "'")
        Else
            DebugOut("File '" + MyProject.ProjectPath + "\" + lstFiles.Items(lstFiles.SelectedIndex) + "' saved successfully!")
        End If
    End Sub

    Private Sub AddNewToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles AddNewToolStripMenuItem.Click
        BlankFileToolStripMenuItem_Click(sender, e)
    End Sub

    Private Sub AddExistingToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles AddExistingToolStripMenuItem.Click
        ExistingFileToolStripMenuItem_Click(sender, e)
    End Sub

    Private Sub RemoveToolStripMenuItem2_Click(sender As Object, e As EventArgs) Handles RemoveToolStripMenuItem2.Click
        SelectedFileToolStripMenuItem1_Click(sender, e)
    End Sub

    Private Sub SaveToolStripMenuItem3_Click(sender As Object, e As EventArgs) Handles SaveToolStripMenuItem3.Click
        SaveSelectedFileToolStripMenuItem_Click(sender, e)
    End Sub

    Private Sub CreateFromDocumentToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CreateFromDocumentToolStripMenuItem.Click
        Dim Src As String, SrcCode As String, rt As Integer, fBL As New frmBuildLibrary

        Src = SyntaxView1.TxtSource
        SrcCode = ""

        rt = mpCom.CompileSingle(Src, SrcCode, False)
        If rt < 0 Then
            MsgBox("Cannot create library until" + vbCrLf +
                   "all errors are corrected!", vbOKOnly, "Create Lib")
            HandleError(0)
            Exit Sub
        End If

        fBL.SetData(Src, mpCom)
        fBL.ShowDialog()
    End Sub

    Private Sub FindReplaceToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles FindReplaceToolStripMenuItem.Click
        If frmGoto Is Nothing Then frmGoto = New frmFindReplaceGoto
        If frmGoto.IsDisposed Then
            GC.Collect()
            frmGoto = New frmFindReplaceGoto
        End If
        frmGoto.Show()

    End Sub

    Private Sub GotoToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles GotoToolStripMenuItem.Click
        If frmGoto Is Nothing Then frmGoto = New frmFindReplaceGoto
        If frmGoto.IsDisposed Then
            GC.Collect()
            frmGoto = New frmFindReplaceGoto
        End If
        frmGoto.Show()

    End Sub

    Private Sub FindToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles FindToolStripMenuItem.Click
        If frmGoto Is Nothing Then frmGoto = New frmFindReplaceGoto
        If frmGoto.IsDisposed Then
            GC.Collect()
            frmGoto = New frmFindReplaceGoto
        End If
        frmGoto.Show()

    End Sub

    Private Sub CurrentDocumentToolStripMenuItem1_Click(sender As Object, e As EventArgs) Handles CurrentDocumentToolStripMenuItem1.Click
        Button5_Click(sender, e)
    End Sub

    Private Sub EntireProjectToolStripMenuItem4_Click(sender As Object, e As EventArgs) Handles EntireProjectToolStripMenuItem4.Click
        If IsProject = False Then
            MsgBox("Not currently in project mode")
            Exit Sub
        End If
        Dim Code As String, rt As Integer, FS As System.IO.FileStream, Bytes() As Byte

        If CompileProject(Code) < 0 Then
            MsgBox("Cannot export raw until all" + vbCrLf +
                   "errors are corrected.", vbOKOnly, "RAW Export")
        Else
            With SaveFileDialog1
                .FileName = ""
                .Filter = "RAW Files|*.raw|All Files|*.*"
                rt = .ShowDialog
                If rt = 2 Then Exit Sub

                Try
                    If My.Computer.FileSystem.FileExists(.FileName) Then My.Computer.FileSystem.DeleteFile(.FileName)

                    rt = CodeToBytes(Code, Bytes)

                    FS = System.IO.File.OpenWrite(.FileName)
                    FS.Write(Bytes, 0, Bytes.Length)
                    FS.Close()

                    DebugOut("RAW Exported successfully!")
                Catch Ex As Exception
                    DebugOut(Ex.ToString)
                    DebugOut("RAW Export failed!")
                End Try
            End With
        End If
    End Sub

    Private Sub RAWToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles RAWToolStripMenuItem.Click
        Button7_Click(sender, e)
    End Sub

    Private Sub EntireProjectToolStripMenuItem6_Click(sender As Object, e As EventArgs) Handles EntireProjectToolStripMenuItem6.Click
        If IsProject = False Then
            MsgBox("Not currently in project mode")
            Exit Sub
        End If

        Dim Code As String, fI As New frmInject

        If CompileProject(Code) < 0 Then
            MsgBox("Cannot inject file until all" + vbCrLf +
                   "errors are corrected.", vbOKOnly, "File Inject")
        Else
            If Code = "" Then Exit Sub
            If Code = vbCrLf Then Exit Sub

            fI.SetData(Code)
            fI.ShowDialog()
        End If
    End Sub

    Private Sub CurrentDocumentToolStripMenuItem3_Click(sender As Object, e As EventArgs) Handles CurrentDocumentToolStripMenuItem3.Click
        Dim rt As Integer, ret As String, fI As New frmInject


        rt = mpCom.CompileSingle(SyntaxView1.TxtSource, ret, False)
        If rt < 0 Then
            MsgBox("Cannot inject file until all" + vbCrLf +
                   "errors are corrected.", vbOKOnly, "File Inject")

            DebugOut("Compile Error!")
            HandleError(0)
        Else
            If ret = "" Then Exit Sub
            If ret = vbCrLf Then Exit Sub
            fI.SetData(ret)
            fI.ShowDialog()
        End If
    End Sub

    Private Sub DebugWindowToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles DebugWindowToolStripMenuItem.Click
        DebugWindowToolStripMenuItem.Checked = Not DebugWindowToolStripMenuItem.Checked
        If DebugWindowToolStripMenuItem.Checked = True Then
            txtDebug.Visible = True
        Else
            txtDebug.Visible = False
        End If
        frmMain_Resize(sender, e)
    End Sub

    Private Sub FileExtractToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles FileExtractToolStripMenuItem.Click
        Dim fE As New frmExtract, rt As Integer

        rt = MsgBox("Extracting from a file will erase" + vbCrLf +
                    "any unsaved changes! Continue?", vbOKCancel, "Extract")
        If rt = 2 Then Exit Sub

        fE.SetAsm(mpAsm, mpCom)
        fE.ShowDialog()
        GC.Collect()

    End Sub

    Private Sub SettingsToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles SettingsToolStripMenuItem.Click
        Dim fs As New frmSettings
        fs.Init(SyntaxView1, mpAsm)
        fs.ShowDialog()
        GC.Collect()
    End Sub

    Private Sub HexTextConverterToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles HexTextConverterToolStripMenuItem.Click
        Dim HexTools As New frmHexTools
        HexTools.Show()
    End Sub

    Private Sub ValueConverterToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ValueConverterToolStripMenuItem.Click
        Dim vc As New frmValueConverter
        vc.Show()
    End Sub

    Private Sub CurrentDocumentToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CurrentDocumentToolStripMenuItem.Click

        Dim txtData As String, ret As String, rt As Integer

        txtCodeOutput.Text = ""
        DebugOut("Compiling file...")

        txtData = SyntaxView1.TxtSource

        If IsProject Then
            rt = MyProject.SetCDS(MyFile, txtData)
        End If

        rt = mpCom.CompileSingle(txtData, ret, False)
        If rt < 0 Then
            DebugOut("Compiler Error! Cannot export ELF until all errors are fixed.")
            HandleError(0)
        Else
            'txtCodeOutput.Text = ret
            DebugOut("Compile complete, preparing ELF...")

            Dim CDElf As New ELF, i As Integer
            CDElf.Init(AddressOf DebugOut)
            CDElf.SetEntry(mpCom.ElfCfg.Entry)
            CDElf.AppendCode(ret)
            For i = 0 To mpCom.ElfCfg.ResourceCount
                CDElf.AddFile(mpCom.ElfCfg.Resources(i).Offset, mpCom.ElfCfg.Resources(i).Data)
            Next

            With SaveFileDialog1
                .FileName = ""
                .Filter = "ELF File|*.elf"
                rt = .ShowDialog
                If rt = 2 Then Exit Sub

                If CDElf.SaveELF(.FileName) Then
                    DebugOut("Exported ELF to '" + .FileName + "' successfully!")
                Else
                    DebugOut("ELF Export Failed")
                End If
            End With

        End If

        GC.Collect()
    End Sub

    Private Sub EntireProjectToolStripMenuItem3_Click(sender As Object, e As EventArgs) Handles EntireProjectToolStripMenuItem3.Click
        If IsProject = False Then
            MsgBox("Not currently in project mode")
            Exit Sub
        End If
        Dim Code As String, rt As Integer, FS As System.IO.FileStream, Bytes() As Byte

        If CompileProject(Code) < 0 Then
            MsgBox("Cannot export ELF until all" + vbCrLf +
                   "errors are corrected.", vbOKOnly, "RAW Export")
        Else
            DebugOut("Compile complete, preparing ELF...")

            Dim CDElf As New ELF, i As Integer
            CDElf.Init(AddressOf DebugOut)
            CDElf.SetEntry(mpCom.ElfCfg.Entry)
            CDElf.AppendCode(Code)
            For i = 0 To mpCom.ElfCfg.ResourceCount
                CDElf.AddFile(mpCom.ElfCfg.Resources(i).Offset, mpCom.ElfCfg.Resources(i).Data)
            Next

            With SaveFileDialog1
                .FileName = ""
                .Filter = "ELF File|*.elf"
                rt = .ShowDialog
                If rt = 2 Then Exit Sub

                If CDElf.SaveELF(.FileName) Then
                    DebugOut("Exported ELF to '" + .FileName + "' successfully!")
                Else
                    DebugOut("ELF Export Failed")
                End If
            End With

        End If
    End Sub

    Private Sub SelectedFileToolStripMenuItem3_Click(sender As Object, e As EventArgs) Handles SelectedFileToolStripMenuItem3.Click
        Dim txtData As String, ret As String, rt As Integer, Lines() As String, sp() As String

        txtCodeOutput.Text = ""
        DebugOut("Compiling file...")

        txtData = SyntaxView1.TxtSource

        If IsProject Then
            rt = MyProject.SetCDS(MyFile, txtData)
        End If

        rt = mpCom.CompileSingle(txtData, ret, False)
        If rt < 0 Then
            DebugOut("Compiler Error!")
            HandleError(0)
        Else
            Dim Addrs() As UInt32, Datas() As UInt32, i As Integer, b As Integer

            DebugOut("Compile completed.")
            If ret = "" Then Exit Sub
            If ret = vbCrLf Then Exit Sub

            ReDim Addrs(0)
            ReDim Datas(0)
            b = -1
            Lines = Split(ret + vbCrLf, vbCrLf)
            For i = 0 To Lines.Count - 1
                sp = Split(Lines(i) + " ", " ")
                If sp(1) <> "" Then
                    b += 1
                    ReDim Preserve Addrs(b)
                    ReDim Preserve Datas(b)
                    Addrs(b) = Convert.ToUInt32(Strings.Right(sp(0), 7), 16)
                    Datas(b) = Convert.ToUInt32(sp(1), 16)
                End If
            Next


            Dim EMU As New frmEmulator
            EMU.Show()
            EMU.SetAsm(mpAsm, mpCom)

            EMU.SetCode(Addrs, Datas)
        End If

        GC.Collect()
    End Sub

    Private Sub EntireProjectToolStripMenuItem2_Click(sender As Object, e As EventArgs) Handles EntireProjectToolStripMenuItem2.Click
        If IsProject = False Then
            MsgBox("Not currently in project mode")
            Exit Sub
        End If

        Dim i As Integer, fileDatas() As String, fileNames() As String, rt As Integer, ret As String

        txtCodeOutput.Text = ""
        DebugOut("Compiling project '" + MyProject.ProjectName + "'...")

        rt = MyProject.SetCDS(MyFile, SyntaxView1.TxtSource)
        If rt < 0 Then DebugOut("Error: Project file '" + MyFile + "' doesn't exist")

        ReDim fileDatas(MyProject.CDSCount)
        ReDim fileNames(MyProject.CDSCount)

        For i = 0 To MyProject.CDSCount
            fileNames(i) = MyProject.CDS(i).FileName
            fileDatas(i) = MyProject.CDS(i).FileData
        Next

        rt = mpCom.CompileProject(fileNames, fileDatas, ret)
        If rt < 0 Then
            DebugOut("Compiler Error!")
            HandleError(1)
        Else
            'txtCodeOutput.Text = ret


            Dim Addrs() As UInt32, Datas() As UInt32, sp() As String, Lines() As String, b As Integer

            DebugOut("Compile completed.")
            If ret = "" Then Exit Sub
            If ret = vbCrLf Then Exit Sub

            ReDim Addrs(0)
            ReDim Datas(0)
            b = -1
            Lines = Split(ret + vbCrLf, vbCrLf)
            For i = 0 To Lines.Count - 1
                sp = Split(Lines(i) + " ", " ")
                If sp(1) <> "" Then
                    b += 1
                    ReDim Preserve Addrs(b)
                    ReDim Preserve Datas(b)
                    Addrs(b) = Convert.ToUInt32(Strings.Right(sp(0), 7), 16)
                    Datas(b) = Convert.ToUInt32(sp(1), 16)
                End If
            Next


            Dim EMU As New frmEmulator
            EMU.Show()
            EMU.SetAsm(mpAsm, mpCom)

            EMU.SetCode(Addrs, Datas)
        End If

        GC.Collect()
    End Sub

    Private Sub SyntaxView1_Load(sender As Object, e As EventArgs) Handles SyntaxView1.Load

    End Sub

    Private Sub ManageToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ManageToolStripMenuItem.Click

    End Sub

    Private Sub PS2ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles PS2ToolStripMenuItem.Click
        If ps2Joker Is Nothing Then ps2Joker = New frmPS2Controller
        If ps2Joker.IsDisposed Then
            GC.Collect()
            ps2Joker = New frmPS2Controller
        End If
        ps2Joker.Show()

    End Sub

    Private Sub ProjectStyleToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ProjectStyleToolStripMenuItem.Click
        If IsProject = False Then
            Dim rt As Integer
            rt = MsgBox("Switching to project mode will lose" + vbCrLf +
                        "any unsaved changes! Continue?", vbOKCancel, "Project Mode")
            If rt = 2 Then Exit Sub
            NewProject()

            ProjectToolStripMenuItem.Enabled = True
            NewToolStripMenuItem1.Enabled = False
            OpenToolStripMenuItem.Enabled = False
            SaveToolStripMenuItem.Enabled = False
            SaveAsToolStripMenuItem.Enabled = False

            ClassicToolStripMenuItem.Checked = False
            ProjectStyleToolStripMenuItem.Checked = True
            lstFiles.Visible = True
            Button1.Visible = False
            Button2.Visible = False
            Button3.Visible = False
            Button4.Visible = False
            Button5.Visible = False
            Button6.Visible = False
            Button7.Visible = False
            Button8.Visible = False
            frmMain_Resize(sender, e)

            Me.Text = "Code Designer 3 (Project Mode) ~ Created by: Gtlcpimp"
        End If
    End Sub

    Private Sub ClassicToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ClassicToolStripMenuItem.Click
        If IsProject Then
            Dim rt As Integer
            rt = MsgBox("Switching to classic mode will lose" + vbCrLf +
                        "any unsaved changes! Continue?", vbOKCancel, "Classic Mode")
            If rt = 2 Then Exit Sub
            LeaveProject()

            SyntaxView1.TxtSource = ""
            ProjectToolStripMenuItem.Enabled = False
            NewToolStripMenuItem1.Enabled = True
            OpenToolStripMenuItem.Enabled = True
            SaveToolStripMenuItem.Enabled = True
            SaveAsToolStripMenuItem.Enabled = True

            ClassicToolStripMenuItem.Checked = True
            ProjectStyleToolStripMenuItem.Checked = False
            lstFiles.Visible = False
            Button1.Visible = True
            Button2.Visible = True
            Button3.Visible = True
            Button4.Visible = True
            Button5.Visible = True
            Button6.Visible = True
            Button7.Visible = True
            Button8.Visible = True
            frmMain_Resize(sender, e)

            Me.Text = "Code Designer 3 (Classic Mode) ~ Created by: Gtlcpimp"
        End If
    End Sub

    Private Function CompileProject(ByRef Output As String) As Integer
        If IsProject = False Then
            MsgBox("Not currently in project mode")
            Return -1
        End If

        Dim i As Integer, fileDatas() As String, fileNames() As String, rt As Integer, ret As String

        txtCodeOutput.Text = ""
        DebugOut("Compiling project '" + MyProject.ProjectName + "'...")

        rt = MyProject.SetCDS(MyFile, SyntaxView1.TxtSource)
        If rt < 0 Then DebugOut("Error: Project file '" + MyFile + "' doesn't exist")

        ReDim fileDatas(MyProject.CDSCount)
        ReDim fileNames(MyProject.CDSCount)

        For i = 0 To MyProject.CDSCount
            fileNames(i) = MyProject.CDS(i).FileName
            fileDatas(i) = MyProject.CDS(i).FileData
        Next

        rt = mpCom.CompileProject(fileNames, fileDatas, ret)
        GC.Collect()

        If rt < 0 Then
            DebugOut("Compiler Error!")
            HandleError(1)
        Else
            Output = ret
            DebugOut("Compile Complete!")
            Return 0
        End If

        Return -2
    End Function




    Public Sub formNewItemReturn(args() As String, okCancel As Integer)
        Dim rt As Integer, fNewRetry As New frmNewItem
        If okCancel = 0 Then Exit Sub

        Select Case frmNewItem_Mode
            Case "BlankFile"
                If LCase(Strings.Right(args(1), 4)) = ".cds" Then args(1) = Strings.Left(args(1), Len(args(1)) - 4)
                rt = MyProject.AddCDS(args(1), "")
                If rt < 0 Then
                    rt = MsgBox("File name already exists in project!", vbOKCancel, "Add Project File")
                    If rt = 2 Then Exit Sub
                    fNewRetry.SetMode(frmNewItem_Mode, "New File")
                    fNewRetry.ShowDialog()
                    GC.Collect()
                Else
                    RefreshProjectList()
                End If
            Case "ExistingFile"
                If LCase(Strings.Right(args(1), 4)) = ".cds" Then args(1) = Strings.Left(args(1), Len(args(1) - 4))
                rt = MyProject.AddCDS(args(1), args(2))
                If rt < 0 Then
                    rt = MsgBox("File name already exists in project!", vbOKCancel, "Add Project File")
                    If rt = 2 Then Exit Sub
                    fNewRetry.SetMode(frmNewItem_Mode, "")
                    fNewRetry.ShowDialog()
                    GC.Collect()
                Else
                    RefreshProjectList()
                End If
        End Select
    End Sub

    Public Sub formSettingsReturn()

    End Sub

    Public Sub ClearDebugs()
        txtDebug.Text = ""
    End Sub

    Public Sub DebugOut(Str As String)
        Dim LineLimiter() As String
        LineLimiter = Split(vbCrLf + txtDebug.Text, vbCrLf)
        LineLimiter(0) = Str
        If LineLimiter.Count - 1 > 100 Then
            ReDim Preserve LineLimiter(100)
        End If
        txtDebug.Text = Join(LineLimiter, vbCrLf)
        txtDebug.SelectionStart = 0
    End Sub

    Private Sub frmMain_Closing(sender As Object, e As CancelEventArgs) Handles Me.Closing
        Dim rt As Integer
        rt = MsgBox("Would you like a chance to" + vbCrLf +
                    "save data before closing?", vbYesNo, "Save Before Quit")
        If rt = 6 Then
            wasSaved = False
            If IsProject Then
                SaveProjectToolStripMenuItem_Click(sender, e)
                If wasSaved = False Then e.Cancel = True
            Else
                Button6_Click(sender, e)
                If wasSaved = False Then e.Cancel = True
            End If
        End If
    End Sub
End Class
