import os, shutil
from os import path
root = path.join(path.dirname(__file__).replace('task',''),
       'config', 'vimfiles')

s= path.join(root, '_vimrc') 
t = path.join('c:', 'vim', '_vimrc')
shutil.copy(s, t)

s = path.join(root, 'format.vim')
t = path.join('c:', 'vim', 'vimfiles', 'format.vim')
shutil.copy(s, t)
