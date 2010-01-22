# coding=utf-8
from __future__ import with_statement
from urlparse import urlparse
import sys, os, urllib
def wget(url, file=None):
    bf = os.path.basename(urlparse(url).path)
    if not file: file = bf
    r = urllib.urlopen(url)
    with open(file, 'wb') as f:
        f.write(r.read())

def usage():
    usage = u'將指定 URL 之資源下載至本機檔案\n'
    usage += u'用法: %s url [file]\n'
    usage += u'file: 指定資源所要存成本機檔案之檔名\n'
    return usage % os.path.basename(sys.argv[0]) 

if __name__ == '__main__':
    try:
        if len(sys.argv) == 1: raise IndexError()
        elif len(sys.argv) == 2:
            wget(sys.argv[1])
        elif len(sys.argv) == 3:
            wget(sys.argv[1], sys.argv[2])
    except IndexError:
        print usage()
