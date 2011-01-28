# coding=utf8
# 交談式的搜尋程式
import sys
from os import path
from find import *
from fnmatch import fnmatch

print u'交談搜尋者'

pwd = sys.argv[1]

def parse(i):
    for root, dirs, files in os.walk(pwd):
        for f in files:
            if fnmatch(f, '*stx'):
                p = path.join(root, f)
                grep(p, i)

while True:
    i = sys.stdin.readline()
    parse(i)
