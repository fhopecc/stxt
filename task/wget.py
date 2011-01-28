# coding=utf-8
from __future__ import with_statement
from logging import config
import sys, os, logging

config.fileConfig(r'config\log.conf')
logger = logging.getLogger()

def wget(url, file=None):
    u'將指定 URL 之資源下載至本機檔案\n'
    from urlparse import urlparse
    import urllib2
    print u'開始下載 %s ' % url
    bf = os.path.basename(urlparse(url).path)
    if not file: file = bf

    try:
        r = urllib2.urlopen(url)
        with open(file, 'wb') as f:
            f.write(r.read())
        r.close()
    except urllib2.URLError, e:
        print u'下載 %s 失敗' % url
        exit()

    print u'下載 %s 完成' % url

def usage():
    usage =  wget.__doc__
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
