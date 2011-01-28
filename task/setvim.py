import os, shutil
from os import path
root = path.join(path.dirname(__file__).replace('task',''),
       'config', 'vimfiles')

s= path.join(root, '_vimrc') 
t = path.join('C:\\', 'vim', '_vimrc')
shutil.copy(s, t)

s = path.join(root, 'format.vim')
t = path.join('C:\\', 'vim', 'vimfiles', 'format.vim')
shutil.copy(s, t)

s = path.join(root, 'ftdetect', 'stx.vim')
t = path.join('C:\\', 'vim', 'vimfiles', 'ftdetect', 'stx.vim')
shutil.copy(s, t)

s = path.join(root, 'after', 'ftplugin', 'python.vim')
t = path.join('C:\\', 'vim', 'vimfiles', 'after', 'ftplugin', 'python.vim')
shutil.copy(s, t)

