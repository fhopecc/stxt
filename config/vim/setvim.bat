rem 設定個人 VIM 環境
rem

goto %COMPUTERNAME%

:FRANKSHEN-PC
set src=%~dp0
set dst=C:\vim
goto copyfile

set src=%~p0
set dst=C:\vim

:copyfile
copy %src%_vimrc %dst%

if not exist %dst%\vimfiles mkdir %dst%\vimfiles
copy %src%\format.vim %dst%\vimfiles
 
if not exist %dst%\vimfiles\ftdetect mkdir %dst%\vimfiles\ftdetect
 
copy %src%\ftdetect\* %dst%\vimfiles\ftdetect\
:end
echo "end"
