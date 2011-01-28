# coding=utf-8
from __future__ import with_statement
import sys, os, gzip
def gunzip(gz, file=None):
    if not file: file = gz.replace('.gz', '')

    z = gzip.open(gz)
    with open(file, 'wb') as f:
        f.write(z.read())
    z.close()

def usage():
    usage = u'將指定 gzip 檔解壓縮\n'
    usage += u'用法: %s gzip [file]\n'
    usage += u'file: 指定資源所要存成本機檔案之檔名\n'
    return usage % os.path.basename(sys.argv[0]) 

if __name__ == '__main__':
    try:
        if len(sys.argv) == 1: raise IndexError()
        elif len(sys.argv) == 2:
            gunzip(sys.argv[1])
        elif len(sys.argv) == 3:
            gunzip(sys.argv[1], sys.argv[2])
    except IndexError:
        print usage()
