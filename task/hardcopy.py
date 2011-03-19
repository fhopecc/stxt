from tempfile import *
import os, fileinput

name = r'tmp/tmphardcopy'

tmpf = open(name, 'w')

for line in fileinput.input():
    tmpf.write(line)

tmpf.close()

cmd = 'notepad /P %s' % tmpf.name
os.system(cmd)

