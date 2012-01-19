Attribute VB_Name = "NewMacros"
Sub 附件及括號黑體()
Attribute 附件及括號黑體.VB_Description = "巨集錄製於 2011/12/19，錄製者 張簡稜剛"
Attribute 附件及括號黑體.VB_ProcData.VB_Invoke_Func = "Normal.NewMacros.附件及括號黑體"
'
' 附件及括號黑體 巨集
' 巨集錄製於 2011/12/19，錄製者 張簡稜剛
'
    Selection.Find.Execute Replace:=wdReplaceAll
    With Selection.Find
        .Text = "「*」"
        .Replacement.Text = ""
        .Forward = True
        .Wrap = wdFindContinue
        .Format = True
        .MatchCase = False
        .MatchWholeWord = False
        .MatchByte = False
        .CorrectHangulEndings = False
        .MatchAllWordForms = False
        .MatchSoundsLike = False
        .MatchFuzzy = False
        .MatchWildcards = True
    End With
    Selection.Find.Execute Replace:=wdReplaceAll
    With Selection.Find
        .Text = "\(如附件*\)"
        .Replacement.Text = ""
        .Forward = True
        .Wrap = wdFindContinue
        .Format = True
        .MatchCase = False
        .MatchWholeWord = False
        .MatchByte = False
        .CorrectHangulEndings = False
        .MatchAllWordForms = False
        .MatchSoundsLike = False
        .MatchFuzzy = False
        .MatchWildcards = True
    End With
    Selection.Find.Execute Replace:=wdReplaceAll
End Sub
