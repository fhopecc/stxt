dim http, url, blogID
set http=createObject("Microsoft.xmlhttp")
blogID="8814043717908464115"
url = "https://www.googleapis.com/blogger/v3/blogs/" & blogID

WScript.Echo "URL : " & url

http.open "GET", url, false
http.setRequestHeader "Content-Type", "text/xml"

WScript.Echo "REQUEST : " & strRequest
http.send strRequest

If http.Status = 200 Then
    WScript.Echo "RESPONSE : " & http.responseXML.xml
else
    WScript.Echo "ERRCODE : " & http.status
End If
