rem 設定個人 VIM 環境
rem

goto %COMPUTERNAME%

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
echo "end"
