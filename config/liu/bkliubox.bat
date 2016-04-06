echo off
if "%USERNAME%"=="JONE" (
    rem JONE's Notebook
    copy C:\ProgramData\Boshiamy\liu.box d:\stxt\config\liu
) else (
    copy %windir%\system32\liu.box d:\stxt\config\liu
)
