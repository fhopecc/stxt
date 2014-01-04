option explicit

dim txt, src, dst

txt = wscript.arguments(0)
 
'ADODB.Stream file I/O constants
Const adTypeBinary          = 1
const adTypeText            = 2
const adSaveCreateOverWrite = 2
const adReadAll             = -1
const adWriteChar           = 0

with createObject("ADODB.Stream")
    dim strText
    .open
    .type = adTypeBinary
    .loadFromFile txt
    .type = adTypeText
    .charset = "utf-8"
    strText = .ReadText(adReadAll)
    .position = 0
    .setEOS
    .charset = "big5"
    .writeText strText, adWriteChar
    .saveToFile txt & "_big5.txt", adSaveCreateOverWrite
    .close
end with
