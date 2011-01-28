# coding=utf8
from __future__ import with_statement
import sys, os, shutil
from os import path
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from lib.eltap import ap1
from config import logger

def download_templates(*files):
    for file in files:
        bf = os.path.basename(file)
        sys = bf[0:3].lower()
        ap1.get_file('/elt/eltapp/bin/%s/%s' % (sys, bf ), 'tmp/%s' % bf)

def usage():
    usage = u'將指定函稿下載至本機\n'
    usage += u'用法: %s template1 template2...\n'
    return usage % os.path.basename(sys.argv[0]) 

if __name__ == '__main__':
    try:
        if len(sys.argv) < 2: raise IndexError()
        download_templates(*sys.argv[1:])
    except IndexError:
        print usage()
