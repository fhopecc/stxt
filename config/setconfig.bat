rem �ƥ��ӤH���]�w

echo off
goto %COMPUTERNAME%

:ASUS
echo "�a�̪����q"
copy C:\ProgramData\Boshiamy\liu.box d:\stxt\config\liu
copy C:\vim\_vimrc d:\stxt\config\vim
goto end

:FRANKSHEN-PC
set src=%~dp0
set dst=C:\vim
goto pullfile

:103TT047
echo "�|�B�~���q��"
copy d:\stxt\config\liu\liu.box C:\ProgramData\Boshiamy
copy d:\stxt\config\vim\_vimrc C:\vim
goto end

:pullfile
copy %dst% %src%_vimrc 

:end
echo "�ƥ�����"