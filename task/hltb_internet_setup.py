#coding=utf8
import os, sys
cmd = 'net use T: /delete'
os.system(cmd)
path = r'\\192.168.1.4\scc\暫存資料'.decode('utf8')
if sys.version[0:1] == '3':
    cmd = r'net use T: {0}'.format(path)
else:
    cmd = r'net use T: %s' % path.encode('cp950')
print cmd
os.system(cmd)
