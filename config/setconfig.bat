rem 設定個人的設定

echo off
goto %COMPUTERNAME%

:ASUS
echo "家裡的筆電"
copy d:\stxt\config\liu\liu.box C:\ProgramData\Boshiamy
copy d:\stxt\config\vim\_vimrc c:\vim 
goto end

:FRANKSHEN-PC
set src=%~dp0
set dst=C:\vim
goto pullfile

:103TT047
echo "稅處外網電腦"
copy d:\stxt\config\liu\liu.box C:\ProgramData\Boshiamy
copy d:\stxt\config\vim\_vimrc C:\vim
goto end

:pullfile
copy %dst% %src%_vimrc 

:end
echo "設定完成"
