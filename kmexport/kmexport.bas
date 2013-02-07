Attribute VB_Name = "Module1"
Sub 匯出知識附件批次檔()
Attribute 匯出知識附件批次檔.VB_Description = "張簡稜剛 在 2013/1/23 錄製的巨集"
Attribute 匯出知識附件批次檔.VB_ProcData.VB_Invoke_Func = " \n14"
'
' Macro1 Macro
' 張簡稜剛 在 2013/1/23 錄製的巨集
'
    Dim sdir, tdir, ps() As String
    sdir = "\\eee\kms_files\Upload"
    tdir = "d:\kmexport\testfile"
    Set fs = CreateObject("Scripting.FileSystemObject")
    Set f = fs.CreateTextFile("c:\export_kmfile.bat", True)
    
    For Each w In Worksheets
        With w.Cells
            Set c = .Find("km", LookIn:=xlValues)
            If Not c Is Nothing Then
                firstAddress = c.Address
                Do
                    ps = Split(CStr(c.Value), Chr(10))
                    For i = 1 To UBound(ps)
                        If ps(i) <> "" Then
                            s = sdir & ps(i)
                            t = tdir & ps(i)
                            p = "copy " & s & " " & t
                            f.WriteLine p
                        End If
                    Next
                    Set c = .FindNext(c)
                Loop While Not c Is Nothing And c.Address <> firstAddress
            End If
        End With
    Next
    f.Close

End Sub
