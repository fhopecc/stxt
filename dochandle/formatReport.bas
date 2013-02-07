Attribute VB_Name = "NewMacros"
Type RenameDef
    OldName As String
    NewName As String
End Type

Sub 平時考核格式()
    Call 附件及括號黑體
    Call 標題灰底
End Sub

Sub 附件及括號黑體()
'
' 附件及括號黑體 巨集
' 巨集錄製於 2011/12/19，錄製者 張簡稜剛
'
    Selection.Find.ClearFormatting
    With Selection.Find
        .Text = "「*」"
        .Forward = True
        .Wrap = wdFindContinue
        .Format = True
        .MatchWildcards = True
    End With
    Do
      Selection.Find.Execute
      Selection.Font.Bold = True
    Loop While Selection.Find.Found
    
    With Selection.Find
        .Text = "(附件[0-9]{1,}*)"
        .Forward = True
        .Wrap = wdFindContinue
        .Format = True
        .MatchWildcards = True
    End With
    Do
      Selection.Find.Execute
      Selection.Font.Bold = True
    Loop While Selection.Find.Found
    
End Sub

Sub 標題灰底()
' 巨集2 巨集
' 巨集錄製於 2013/1/7，錄製者 張簡稜剛
'
    Selection.Find.ClearFormatting
    With Selection.Find
        .Text = "([一二三四五六七八九十].*？)"
        .Forward = True
        .Wrap = wdFindContinue
        .Format = True
        .MatchWildcards = True
    End With
        
    Do
      Selection.Find.Execute
      Selection.Paragraphs(1).Shading.BackgroundPatternColor = wdColorGray15
      Selection.Font.Bold = False
    Loop While Selection.Find.Found
    
End Sub

Sub 資安文件重命名()
Attribute 資安文件重命名.VB_Description = "巨集錄製於 2013/1/16，錄製者 張簡稜剛"
Attribute 資安文件重命名.VB_ProcData.VB_Invoke_Func = "Normal.NewMacros.資安文件重命名"
    Dim renames(2) As RenameDef
    renames(1).OldName = "【機關全銜】"
    renames(1).NewName = "花蓮縣地方稅務局"
    renames(2).OldName = "機關代碼"
    renames(2).NewName = "HLTB"
    
    For i = 1 To 2
        Dim r As RenameDef
        r = renames(i)
        Selection.Find.ClearFormatting
        Selection.Find.Replacement.ClearFormatting
        With Selection.Find
            .Text = r.OldName
            .Replacement.Text = r.NewName
            .Forward = True
        End With
        Selection.Find.Execute Replace:=wdReplaceAll
    Next i

End Sub
