import os, shutil
_vimrc = os.path.join(os.path.dirname(__file__).replace('task', ''), '_vimrc') 
t = os.path.join('c:', 'Vim', '_vimrc')
shutil.copy(_vimrc, t)
