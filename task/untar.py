# coding=utf-8
from __future__ import with_statement
import sys, os, tarfile
def untar(tar):
    tar = tarfile.open(tar)
    tar.extractall()
    tar.close()

def usage():
    usage = u'將指定 tar 檔解開\n'
    usage += u'用法: %s tar\n'
    return usage % os.path.basename(sys.argv[0]) 

if __name__ == '__main__':
    try:
        if len(sys.argv) == 1: raise IndexError()
        elif len(sys.argv) == 2:
            untar(sys.argv[1])
    except IndexError:
        print usage()
