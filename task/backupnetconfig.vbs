
const oONMSIP = "192.168.1.13"
const iONMSIP = "10.66.4.17"

set WshShell = WScript.CreateObject("WScript.Shell")
set WshSysEnv = WshShell.Environment("PROCESS")

select case WshSysEnv("COMPUTERNAME")
    case "RDC"
        BackupFG100A
        BackupONMSDB oONMSIP
    case "EEE"
        BackupEK6
        BackupFG200B
        BackupONMSDB iONMSIP
    case Else
        WScript.Echo WshSysEnv("COMPUTERNAME")
end select

sub BackupC2621()
    dim dest, cmd

    set sh = CreateObject("WScript.Shell")
    sh.run"cmd.exe"
    WScript.Sleep 500

    sh.SendKeys "telnet 10.66.254.254"
    sh.SendKeys("{Enter}")
    WScript.Sleep 500

    sh.SendKeys "ce"
    sh.SendKeys("{Enter}")
    WScript.Sleep 500

    sh.SendKeys "uecicsed"
    sh.SendKeys("{Enter}")
    WScript.Sleep 500

    dest = "tftp://10.66.4.56/c2621_" & TodayStr() & ".cfg"

    sh.SendKeys "copy running-config " & dest
    sh.SendKeys "{Enter}"
    WScript.Sleep 500
    sh.SendKeys "{Enter}"
    WScript.Sleep 500
    sh.SendKeys "{Enter}"
    WScript.Sleep 1000
end sub

sub BackupFG100A()

    dim dest, cmd

    set oShell = CreateObject("WScript.Shell")

    oShell.run "cmd.exe"
    WScript.Sleep 500
    oShell.SendKeys "ssh -l admin 192.168.1.254"
    oShell.SendKeys("{Enter}")
    WScript.Sleep 500
    oShell.SendKeys"!tsinim9"
    oShell.SendKeys("{Enter}")
    WScript.Sleep 500
 
    dest = "FG100A_" & TodayStr() & ".cfg"
    cmd  =  "execute backup full-config tftp " & dest & " 192.168.1.101"
    
    oShell.SendKeys(cmd)
    oShell.SendKeys("{Enter}")
    WScript.Sleep 4000

    oShell.SendKeys("exit")
    oShell.SendKeys("{Enter}")
    WScript.Sleep 500

    oShell.SendKeys("exit")
    oShell.SendKeys("{Enter}")
    WScript.Sleep 500
end sub

sub BackupFG200B()
    dim dest, cmd
    set oShell = CreateObject("WScript.Shell")
    oShell.run "cmd.exe"
    WScript.Sleep 500
    oShell.SendKeys "ssh -l admin 10.66.253.201"
    oShell.SendKeys("{Enter}")
    WScript.Sleep 500
    oShell.SendKeys"!tsinim9"
    oShell.SendKeys("{Enter}")
    WScript.Sleep 500
    
    dest = "FG200B_" & TodayStr() & ".cfg"
    cmd  = "execute backup full-config tftp " & dest & " 10.66.4.9"
    
    oShell.SendKeys(cmd)
    oShell.SendKeys("{Enter}")
    WScript.Sleep 5000

    oShell.SendKeys("exit")
    oShell.SendKeys("{Enter}")
    WScript.Sleep 500

    oShell.SendKeys("exit")
    oShell.SendKeys("{Enter}")
    WScript.Sleep 500
end sub



sub BackupEK6()
    dim dest, cmd
    set sh = CreateObject("WScript.Shell")
    sh.run"cmd.exe"
    WScript.Sleep 500
    sh.SendKeys "ssh -l admin 10.66.4.254"
    sh.SendKeys("{Enter}")
    WScript.Sleep 500
    sh.SendKeys"!tsinim9"
    sh.SendKeys("{Enter}")
    WScript.Sleep 500

    sh.SendKeys "delete slot7/EK6.cfg"
    sh.SendKeys("{Enter}")
    WScript.Sleep 500

    sh.SendKeys "show config outfile slot7/EK6.cfg"
    sh.SendKeys("{Enter}")
    WScript.Sleep 500

    dest = "tftp://10.66.4.9/EK6_" & TodayStr() & ".cfg"
    sh.SendKeys "copy slot7/EK6.cfg " & dest
    sh.SendKeys("{Enter}")
    WScript.Sleep 3000

    sh.SendKeys "exit"
    sh.SendKeys("{Enter}")
    WScript.Sleep 500

    sh.SendKeys "exit"
    sh.SendKeys("{Enter}")
    WScript.Sleep 500
end sub

Sub BackupONMSDB(host)
    dim pgDump, dir, hoststr, dest, cmd, ts
    dir     = "netbackup"
    hoststr = replace(host,".","_")
    dest    = dir & "onmsdb_" & hoststr & "_" & TodayStr() & ".dmp"
    cmd = "pg_dump -h " & host & " -U opennms -f " & dest & " opennms" 
    WScript.echo cmd

    set sh = CreateObject("WScript.Shell")

    sh.run "cmd.exe"
    WScript.Sleep 500
    sh.SendKeys "set path=%path%;c:/Program Files/PostgreSQL/9.0/bin"
    sh.SendKeys("{Enter}")
    WScript.Sleep 500
    sh.SendKeys cmd
    sh.SendKeys("{Enter}")
    WScript.Sleep 500
    sh.SendKeys "opennms" ' for password
    sh.SendKeys("{Enter}")
End Sub

Function DateToStr(d)
    DateToStr = FormatNumber(Year(d),0,,,0) _
            & Lpad(FormatNumber(Month(d),0,,,0), 2,"0") _
            & Lpad(FormatNumber(Day(d),0,,,0), 2,"0")
End Function

Function TodayStr()
    TodayStr = DateToStr(Now)
End Function

Function Lpad(i, l, c)
	Dim r
    If Len(i) >= l Then
		r = i
	Else
        r = String(l - Len(i), c) & i 
	End if
	Lpad = r
End Function
