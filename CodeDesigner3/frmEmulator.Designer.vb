<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class frmEmulator
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()>
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()>
    Private Sub InitializeComponent()
        Me.TabControl1 = New System.Windows.Forms.TabControl()
        Me.tabEE = New System.Windows.Forms.TabPage()
        Me.tabCOP0 = New System.Windows.Forms.TabPage()
        Me.tabCOP1 = New System.Windows.Forms.TabPage()
        Me.tabOut = New System.Windows.Forms.TabPage()
        Me.txtOutput = New System.Windows.Forms.TextBox()
        Me.Button1 = New System.Windows.Forms.Button()
        Me.Button2 = New System.Windows.Forms.Button()
        Me.Button3 = New System.Windows.Forms.Button()
        Me.txtCycleLimit = New System.Windows.Forms.TextBox()
        Me.txtTimeLimit = New System.Windows.Forms.TextBox()
        Me.Button4 = New System.Windows.Forms.Button()
        Me.txtGoto = New System.Windows.Forms.TextBox()
        Me.Button5 = New System.Windows.Forms.Button()
        Me.Button6 = New System.Windows.Forms.Button()
        Me.Button7 = New System.Windows.Forms.Button()
        Me.OpenFileDialog1 = New System.Windows.Forms.OpenFileDialog()
        Me.RamView1 = New CodeDesigner3.RAMView()
        Me.TabControl1.SuspendLayout()
        Me.tabOut.SuspendLayout()
        Me.SuspendLayout()
        '
        'TabControl1
        '
        Me.TabControl1.Controls.Add(Me.tabEE)
        Me.TabControl1.Controls.Add(Me.tabCOP0)
        Me.TabControl1.Controls.Add(Me.tabCOP1)
        Me.TabControl1.Controls.Add(Me.tabOut)
        Me.TabControl1.Location = New System.Drawing.Point(648, 12)
        Me.TabControl1.Name = "TabControl1"
        Me.TabControl1.SelectedIndex = 0
        Me.TabControl1.Size = New System.Drawing.Size(443, 711)
        Me.TabControl1.SizeMode = System.Windows.Forms.TabSizeMode.Fixed
        Me.TabControl1.TabIndex = 1
        '
        'tabEE
        '
        Me.tabEE.Location = New System.Drawing.Point(4, 22)
        Me.tabEE.Name = "tabEE"
        Me.tabEE.Padding = New System.Windows.Forms.Padding(3)
        Me.tabEE.Size = New System.Drawing.Size(435, 685)
        Me.tabEE.TabIndex = 0
        Me.tabEE.Text = "EE"
        Me.tabEE.UseVisualStyleBackColor = True
        '
        'tabCOP0
        '
        Me.tabCOP0.Location = New System.Drawing.Point(4, 22)
        Me.tabCOP0.Name = "tabCOP0"
        Me.tabCOP0.Padding = New System.Windows.Forms.Padding(3)
        Me.tabCOP0.Size = New System.Drawing.Size(435, 685)
        Me.tabCOP0.TabIndex = 1
        Me.tabCOP0.Text = "COP0"
        Me.tabCOP0.UseVisualStyleBackColor = True
        '
        'tabCOP1
        '
        Me.tabCOP1.Location = New System.Drawing.Point(4, 22)
        Me.tabCOP1.Name = "tabCOP1"
        Me.tabCOP1.Size = New System.Drawing.Size(435, 685)
        Me.tabCOP1.TabIndex = 2
        Me.tabCOP1.Text = "COP1"
        Me.tabCOP1.UseVisualStyleBackColor = True
        '
        'tabOut
        '
        Me.tabOut.Controls.Add(Me.txtOutput)
        Me.tabOut.Location = New System.Drawing.Point(4, 22)
        Me.tabOut.Name = "tabOut"
        Me.tabOut.Size = New System.Drawing.Size(435, 685)
        Me.tabOut.TabIndex = 3
        Me.tabOut.Text = "Output"
        Me.tabOut.UseVisualStyleBackColor = True
        '
        'txtOutput
        '
        Me.txtOutput.Font = New System.Drawing.Font("Courier New", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.txtOutput.Location = New System.Drawing.Point(3, 3)
        Me.txtOutput.Multiline = True
        Me.txtOutput.Name = "txtOutput"
        Me.txtOutput.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.txtOutput.Size = New System.Drawing.Size(429, 679)
        Me.txtOutput.TabIndex = 0
        '
        'Button1
        '
        Me.Button1.Location = New System.Drawing.Point(542, 220)
        Me.Button1.Name = "Button1"
        Me.Button1.Size = New System.Drawing.Size(100, 21)
        Me.Button1.TabIndex = 2
        Me.Button1.Text = "Execute"
        Me.Button1.UseVisualStyleBackColor = True
        '
        'Button2
        '
        Me.Button2.Location = New System.Drawing.Point(542, 247)
        Me.Button2.Name = "Button2"
        Me.Button2.Size = New System.Drawing.Size(100, 21)
        Me.Button2.TabIndex = 3
        Me.Button2.Text = "Skip"
        Me.Button2.UseVisualStyleBackColor = True
        '
        'Button3
        '
        Me.Button3.Location = New System.Drawing.Point(542, 330)
        Me.Button3.Name = "Button3"
        Me.Button3.Size = New System.Drawing.Size(100, 21)
        Me.Button3.TabIndex = 4
        Me.Button3.Text = "Cycle Run"
        Me.Button3.UseVisualStyleBackColor = True
        '
        'txtCycleLimit
        '
        Me.txtCycleLimit.Font = New System.Drawing.Font("Courier New", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.txtCycleLimit.Location = New System.Drawing.Point(542, 304)
        Me.txtCycleLimit.Name = "txtCycleLimit"
        Me.txtCycleLimit.Size = New System.Drawing.Size(100, 21)
        Me.txtCycleLimit.TabIndex = 5
        Me.txtCycleLimit.Text = "1000"
        Me.txtCycleLimit.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
        '
        'txtTimeLimit
        '
        Me.txtTimeLimit.Font = New System.Drawing.Font("Courier New", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.txtTimeLimit.Location = New System.Drawing.Point(542, 371)
        Me.txtTimeLimit.Name = "txtTimeLimit"
        Me.txtTimeLimit.Size = New System.Drawing.Size(100, 21)
        Me.txtTimeLimit.TabIndex = 7
        Me.txtTimeLimit.Text = "10"
        Me.txtTimeLimit.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
        '
        'Button4
        '
        Me.Button4.Location = New System.Drawing.Point(542, 397)
        Me.Button4.Name = "Button4"
        Me.Button4.Size = New System.Drawing.Size(100, 21)
        Me.Button4.TabIndex = 6
        Me.Button4.Text = "Timed Run"
        Me.Button4.UseVisualStyleBackColor = True
        '
        'txtGoto
        '
        Me.txtGoto.Font = New System.Drawing.Font("Courier New", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.txtGoto.Location = New System.Drawing.Point(542, 34)
        Me.txtGoto.MaxLength = 8
        Me.txtGoto.Name = "txtGoto"
        Me.txtGoto.Size = New System.Drawing.Size(100, 21)
        Me.txtGoto.TabIndex = 9
        Me.txtGoto.Text = "00000000"
        Me.txtGoto.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
        '
        'Button5
        '
        Me.Button5.Location = New System.Drawing.Point(542, 60)
        Me.Button5.Name = "Button5"
        Me.Button5.Size = New System.Drawing.Size(100, 21)
        Me.Button5.TabIndex = 8
        Me.Button5.Text = "Go To"
        Me.Button5.UseVisualStyleBackColor = True
        '
        'Button6
        '
        Me.Button6.Location = New System.Drawing.Point(542, 698)
        Me.Button6.Name = "Button6"
        Me.Button6.Size = New System.Drawing.Size(100, 21)
        Me.Button6.TabIndex = 10
        Me.Button6.Text = "Settings"
        Me.Button6.UseVisualStyleBackColor = True
        '
        'Button7
        '
        Me.Button7.Location = New System.Drawing.Point(542, 671)
        Me.Button7.Name = "Button7"
        Me.Button7.Size = New System.Drawing.Size(100, 21)
        Me.Button7.TabIndex = 11
        Me.Button7.Text = "Load Dump"
        Me.Button7.UseVisualStyleBackColor = True
        '
        'OpenFileDialog1
        '
        Me.OpenFileDialog1.FileName = "OpenFileDialog1"
        '
        'RamView1
        '
        Me.RamView1.BulletMargin = 0
        Me.RamView1.CursorPos = CType(0, Long)
        Me.RamView1.Location = New System.Drawing.Point(12, 12)
        Me.RamView1.Name = "RamView1"
        Me.RamView1.SelLine = CType(0, Long)
        Me.RamView1.Size = New System.Drawing.Size(524, 707)
        Me.RamView1.TabIndex = 12
        '
        'frmEmulator
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(1103, 734)
        Me.Controls.Add(Me.RamView1)
        Me.Controls.Add(Me.Button7)
        Me.Controls.Add(Me.Button6)
        Me.Controls.Add(Me.txtGoto)
        Me.Controls.Add(Me.Button5)
        Me.Controls.Add(Me.txtTimeLimit)
        Me.Controls.Add(Me.Button4)
        Me.Controls.Add(Me.txtCycleLimit)
        Me.Controls.Add(Me.Button3)
        Me.Controls.Add(Me.Button2)
        Me.Controls.Add(Me.Button1)
        Me.Controls.Add(Me.TabControl1)
        Me.Name = "frmEmulator"
        Me.Text = "frmEmulator"
        Me.TabControl1.ResumeLayout(False)
        Me.tabOut.ResumeLayout(False)
        Me.tabOut.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents TabControl1 As TabControl
    Friend WithEvents tabEE As TabPage
    Friend WithEvents tabCOP0 As TabPage
    Friend WithEvents tabCOP1 As TabPage
    Friend WithEvents Button1 As Button
    Friend WithEvents Button2 As Button
    Friend WithEvents Button3 As Button
    Friend WithEvents txtCycleLimit As TextBox
    Friend WithEvents txtTimeLimit As TextBox
    Friend WithEvents Button4 As Button
    Friend WithEvents txtGoto As TextBox
    Friend WithEvents Button5 As Button
    Friend WithEvents tabOut As TabPage
    Friend WithEvents txtOutput As TextBox
    Friend WithEvents Button6 As Button
    Friend WithEvents Button7 As Button
    Friend WithEvents OpenFileDialog1 As OpenFileDialog
    Friend WithEvents RamView1 As RAMView
End Class
