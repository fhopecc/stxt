rem
rem 設定個人 VIM 環境
rem

set src=D:\stxt\config\vim
set dst=C:\vim

copy _vimrc %dst%

copy %src%\format.vim %dst%\vimfiles

copy %src%\ftdetect\stx.vim %dst%\vimfiles\ftdetect
