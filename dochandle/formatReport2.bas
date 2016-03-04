Attribute VB_Name = "NewMacros"
Sub 業務考核附件編號()
Attribute 業務考核附件編號.VB_ProcData.VB_Invoke_Func = "Normal.NewMacros.附件編號"
'
' 附件編號 巨集
'
'
    Dim i As Integer
    i = 0
    Selection.MoveStart
    Do
      With Selection.Find
        i = i + 1
        .ClearFormatting
        .Forward = True
        .Text = "（附件[1-9]{1,2}）"
        .MatchWildcards = True
        .Format = False
        .MatchCase = False
        .MatchWholeWord = False
        .MatchByte = False
        .MatchSoundsLike = False
        .MatchWildcards = True
        .Execute
        If .Found Then
            If Selection.Text = "（附件99）" Then
                i = 1
            End If
            Selection.Text = "（附件" & CStr(i) & "）"
            Selection.Move wdCharacter, 1
        End If
        
      End With
    Loop While Selection.Find.Found
    
     
    
End Sub
