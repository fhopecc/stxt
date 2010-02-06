# coding=utf-8
from __future__ import with_statement
from urllib import parse
from logging import config
import sys, os, urllib, logging

config.fileConfig(r'task\log.conf')
logger = logging.getLogger()

def wget(url, file=None):
    '將指定 URL 之資源下載至本機檔案\n'
    logger.info('開始下載 %s ' % url)
    bf = os.path.basename(urlparse(url).path)
    if not file: file = bf
    r = urllib.urlopen(url)
    with open(file, 'wb') as f:
        f.write(r.read())

def usage():
    usage =  wget.__doc__
    usage += '用法: %s url [file]\n'
    usage += 'file: 指定資源所要存成本機檔案之檔名\n'
    return usage % os.path.basename(sys.argv[0]) 

if __name__ == '__main__':
    try:
        if len(sys.argv) == 1: raise IndexError()
        elif len(sys.argv) == 2:
            wget(sys.argv[1])
        elif len(sys.argv) == 3:
            wget(sys.argv[1], sys.argv[2])
    except IndexError:
        logger.info(usage())
