Public Class frmEmulator
    Private lblBoxes(188) As Label, txtEE(128) As TextBox, txtCOP0(32) As TextBox, txtCOP1(32) As TextBox

    Private mpAsm As MIPSAssembly, mpCom As Compiler, RAM(&H2000000) As Byte
    Private EE As EEProcessor

    Private LoadedCodes As fncCode

    Private Structure fncCode
        Public Addr() As UInt32
        Public Data() As UInt32
    End Structure

    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click
        RamView1.GotoAddress(Convert.ToInt32(txtGoto.Text, 16))
        EE.Position = RamView1.CursorPos * 4
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Dim Rt As Integer
        Rt = EE.Exec1()
        RamView1.CursorPos = EE.Position \ 4
        RefreshRegs()
        If Rt < 0 Then
            TabControl1.SelectTab(3)
            Output("Exception At " + EE.Position.ToString("X8") + ": " + ErrRetString(Rt))
        End If
    End Sub

    Private Sub FrmEmulator_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        InitTabs()
    End Sub

    Private Sub InitTabs()
        Dim I As Integer, i2 As Integer, index As Integer, X As Single, Y As Single
        Dim lblIndex As Integer

        X = 5
        Y = 5
        lblIndex = 0
        For I = 0 To 31
            lblBoxes(lblIndex) = New Label With {
                .Font = New Font("Courier New", 9),
                .BorderStyle = BorderStyle.FixedSingle,
                .TextAlign = 4,
                .AutoSize = False,
                .Width = 70,
                .Height = 21,
                .Text = GetEERegStr(I)
            }

            tabEE.Controls.Add(lblBoxes(lblIndex))
            lblBoxes(lblIndex).Left = X
            lblBoxes(lblIndex).Top = Y

            X = X + lblBoxes(lblIndex).Width + 5
            For i2 = 0 To 3
                index = (I * 4) + i2
                txtEE(index) = New TextBox With {
                    .Font = New Font("Courier New", 9),
                    .TextAlign = HorizontalAlignment.Center,
                    .MaxLength = 8,
                    .Text = "00000000",
                    .Width = 70,
                    .Height = 21
                }

                tabEE.Controls.Add(txtEE(index))
                txtEE(index).Left = X
                txtEE(index).Top = Y
                X = X + txtEE(index).Width + 5
            Next


            X = 5
            Y += lblBoxes(lblIndex).Height
            lblIndex += 1
        Next

        Y = 5
        index = 0
        For I = 0 To 31
            X = 5

            lblBoxes(lblIndex) = New Label With {
                .Font = New Font("Courier New", 9),
                .BorderStyle = BorderStyle.FixedSingle,
                .TextAlign = 4,
                .AutoSize = False,
                .Width = 70,
                .Height = 21,
                .Text = GetCOP0RegStr(I)
            }
            tabCOP0.Controls.Add(lblBoxes(lblIndex))
            lblBoxes(lblIndex).Top = Y
            lblBoxes(lblIndex).Left = X

            X = X + lblBoxes(lblIndex).Width + 5
            txtCOP0(index) = New TextBox With {
                    .Font = New Font("Courier New", 9),
                    .TextAlign = HorizontalAlignment.Center,
                    .MaxLength = 8,
                    .Text = "00000000",
                    .Width = 70,
                    .Height = 21
            }
            tabCOP0.Controls.Add(txtCOP0(index))
            txtCOP0(index).Left = X
            txtCOP0(index).Top = Y

            index += 1
            Y += lblBoxes(lblIndex).Height
            lblIndex += 1
        Next

        Y = 5
        index = 0
        For I = 0 To 31
            X = 5

            lblBoxes(lblIndex) = New Label With {
                .Font = New Font("Courier New", 9),
                .BorderStyle = BorderStyle.FixedSingle,
                .TextAlign = 4,
                .AutoSize = False,
                .Width = 70,
                .Height = 21,
                .Text = GetCOP1RegStr(I)
            }
            tabCOP1.Controls.Add(lblBoxes(lblIndex))
            lblBoxes(lblIndex).Top = Y
            lblBoxes(lblIndex).Left = X

            X = X + lblBoxes(lblIndex).Width + 5
            txtCOP1(index) = New TextBox With {
                    .Font = New Font("Courier New", 9),
                    .TextAlign = HorizontalAlignment.Center,
                    .MaxLength = 8,
                    .Text = "00000000",
                    .Width = 70,
                    .Height = 21
            }
            tabCOP1.Controls.Add(txtCOP1(index))
            txtCOP1(index).Left = X
            txtCOP1(index).Top = Y

            index += 1
            Y += lblBoxes(lblIndex).Height
            lblIndex += 1
        Next
    End Sub
    Public Sub SetAsm(ByRef Asm As MIPSAssembly, ByRef mCom As Compiler)
        mpAsm = Asm
        mpCom = mCom

        EE = New EEProcessor
        EE.Init(mpAsm)
        RamView1.InitMemory(Nothing, EE.vRAM, 2, mpAsm)
        EE.Position = 0

        GC.Collect()
    End Sub

    Private Sub RamView1_Load(sender As Object, e As EventArgs)

    End Sub

    Public Sub SetCode(ByRef Addrs() As UInt32, ByRef Datas() As UInt32)
        ReDim LoadedCodes.Addr(Addrs.Count - 1)
        ReDim LoadedCodes.Data(Addrs.Count - 1)
        Dim i As Integer
        For i = 0 To Addrs.Count - 1
            LoadedCodes.Addr(i) = Addrs(i)
            LoadedCodes.Data(i) = Datas(i)
        Next
        EE.InjectCode(Addrs, Datas)
        RamView1.Invalidate()
        GC.Collect()
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        RamView1.CursorPos += 1
        EE.Position = RamView1.CursorPos * 4
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        Dim i As Integer
        i = CDec(txtCycleLimit.Text)
        While (i > 0)
            If ExecLine() < 0 Then
                Exit While
            End If
            i -= 1
        End While
        RefreshRegs()
    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        Dim i As Integer, watch As New Stopwatch

        i = CDec(txtTimeLimit.Text)
        watch.Start()
        While ((watch.ElapsedMilliseconds \ 1000) < i)
            If ExecLine() < 0 Then
                Exit While
            End If
        End While
        watch.Stop()


        RefreshRegs()
    End Sub

    Private Sub Button7_Click(sender As Object, e As EventArgs) Handles Button7.Click
        Dim rt As Integer, FS As System.IO.FileStream, sp() As String, fData() As Byte
        With OpenFileDialog1
            .FileName = ""
            .Filter = "All Files|*.*"

            rt = .ShowDialog
            If rt = 2 Then Exit Sub

            Try
                sp = Split("\" + .FileName, "\")

                FS = System.IO.File.Open(.FileName, System.IO.FileMode.Open)
                ReDim fData(FS.Length)
                rt = FS.Read(fData, 0, FS.Length)
                FS.Close()
                FS.Dispose()

                EE.UploadDump(fData)
                EE.InjectCode(LoadedCodes.Addr, LoadedCodes.Data)
                RamView1.Invalidate()
                GC.Collect()
            Catch ex As Exception
                MsgBox("Error.")
            End Try

        End With
    End Sub

    Private Function ExecLine() As Integer
        Dim Rt As Integer
        Rt = EE.Exec1()
        RamView1.CursorPos = EE.Position \ 4
        If Rt < 0 Then
            RefreshRegs()
            TabControl1.SelectTab(3)
            Output("Exception At " + EE.Position.ToString("X8") + ": " + ErrRetString(Rt))
            Return -1
        End If
        Return 0
    End Function
    Private Sub RefreshRegs()
        Dim i As Integer, i2 As Integer, sp() As String

        For i = 0 To 31
            sp = Split(EE.DumpGPR(i), " ")
            i2 = (i * 4)
            txtEE(i2 + 0).Text = sp(0)
            txtEE(i2 + 1).Text = sp(1)
            txtEE(i2 + 2).Text = sp(2)
            txtEE(i2 + 3).Text = sp(3)

            txtCOP0(i).Text = EE.DumpCPR(i)
            txtCOP1(i).Text = EE.DumpFPR(i)
        Next
    End Sub

    Private Sub RamView1_MouseUp(sender As Object, e As MouseEventArgs)
        EE.Position = RamView1.CursorPos * 4
    End Sub

    Private Sub RamView1_KeyUp(sender As Object, e As KeyEventArgs)
        EE.Position = RamView1.CursorPos * 4
    End Sub

    Public Sub Output(Str As String)
        txtOutput.Text = Str + vbCrLf + txtOutput.Text
    End Sub
End Class