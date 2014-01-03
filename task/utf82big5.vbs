option explicit

dim txt, src, dst

txt = wscript.arguments(0)
 
'ADODB.Stream file I/O constants
Const adTypeBinary          = 1
const adTypeText            = 2
const adSaveCreateOverWrite = 2
const adReadAll             = -1
const adWriteChar           = 0

set src = createObject("ADODB.Stream")
src.open
src.type = adTypeBinary
src.loadFromFile txt
src.type = adTypeText
src.charset = "utf-8"
dim strtxt
strtxt = src.ReadText(adReadAll)
src.position = 0
src.setEOS
src.charset = "big5"
src.writeText strtxt, adWriteChar
src.saveToFile txt & "_big5.txt", adSaveCreateOverWrite
src.close

'set dst = createObject("ADODB.Stream")
'dst.open
'dst.type = adTypeText
'dst.charset = "big5"
'src.copyTo dst

'dst.saveToFile txt & "_big5", adSaveCreateOverWrite
'dst.close
