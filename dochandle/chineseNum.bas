Attribute VB_Name = "Module2"
Sub 中文月轉數字月()
'
' Macro1 Macro
' 張簡稜剛 在 2013/1/3 錄製的巨集
'
    Dim vCNum(1 To 12) As Variant
    vCNum(1) = "一"
    vCNum(2) = "二"
    vCNum(3) = "三"
    vCNum(4) = "四"
    vCNum(5) = "五"
    vCNum(6) = "六"
    vCNum(7) = "七"
    vCNum(8) = "八"
    vCNum(9) = "九"
    vCNum(10) = "十"
    vCNum(11) = "十一"
    vCNum(12) = "十二"

    For i = 12 To 1 Step -1
        cMon = vCNum(i) & "月"
        nMon = i & "月"
        Cells.Replace What:=cMon, Replacement:=nMon, _
                             LookAt:=xlPart, SearchOrder:=xlByRows, _
                             MatchCase:=False, SearchFormat:=False, _
                             ReplaceFormat:=False
    Next
End Sub

