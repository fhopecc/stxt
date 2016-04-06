rem 備份個人的設定

echo off
goto %COMPUTERNAME%

:ASUS
echo "家裡的筆電"
copy C:\ProgramData\Boshiamy\liu.box d:\stxt\config\liu
copy C:\vim\_vimrc d:\stxt\config\vim
goto end

:FRANKSHEN-PC
set src=%~dp0
set dst=C:\vim
goto pullfile

:103TT047
echo "稅處電腦"
set src=%~dp0
set dst=C:\vim
goto pullfile

:pullfile
copy %dst% %src%_vimrc 

:end
echo "備份完成"
