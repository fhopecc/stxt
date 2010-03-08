# coding=utf8
from __future__ import with_statement
from os import path
from wget import wget
from logging import config
from zipfile import ZipFile
import sys, os, shutil, logging , socket

config.fileConfig(r'config\log.conf')
logger = logging.getLogger()

def update_hlland(docdate, docno, password):
    u'更新地政觸控系統'
    url = 'http://att.hl.gov.tw/SENDATT_FILE/%s/376550400A_%s.zip' % \
          (docdate, docno)
    file = os.path.join('tmp', '376550400A_%s.zip' % docno)
    wget(url, file)
    unzip(file, password)

def unzip(file, password):
    zip = ZipFile(file)
    zrar = zip.namelist()[0]
    rar = os.path.join('tmp', zrar)
    logger.info('unzip %s to %s' % (file, rar))
    if sys.version[0:1] == '3':
        zip.extractall('tmp')
        cmd = "{0} e -o+ -p{1} {2} {3}".format(r'lib\bin\rar', 
              password, rar, os.path.join('tmp', 'hlland'))
    else:
        with open(rar, 'wb') as f:
            f.write(zip.read(zrar))
        zip.close()
        cmd = "%s e -o+ -p%s %s %s" % (r'lib\bin\rar', 
              password, rar, os.path.join('tmp', 'hlland'))
    logger.info(cmd)
    os.system(cmd)

def isalive(host):
    targetIP = socket.gethostbyname(host)  
    socket.setdefaulttimeout(1)
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)  
  
    result = s.connect_ex((targetIP, 445))  
    if result == 0 :  
        return True
    s.close() 
    return False

def deploy():
    targets  = ['96tt003', '96tt006', '97tt024', 
                '97tt025', '97tt027', '97tt040']
    for t in targets:
        if isalive(t):
            if sys.version[0:1] == '3':
                logger.info('{0}:445 is OPENED.'.format(t))
            else:
                logger.info('%s:445 is OPENED.' % t)

            cmd = 'net use T: /delete'
            os.system(cmd)

            if sys.version[0:1] == '3':
                cmd = r'net use T: \\{0}\HL'.format(t)
            else:
                cmd = r'net use T: \\%s\HL' % t

            os.system(cmd)
            dir = os.path.join('tmp', 'hlland')
            for root, dirs, files in os.walk(dir):
                for f in files:
                    src  = os.path.join(root, f)
                    try:
                        shutil.copy(src, 'T:')
                    except IOError:
                        pass
        else:
            if sys.version[0:1] == '3':
                logger.info('{0}:445 is CLOSED.'.format(t))
            else:
                logger.info('%s:445 is CLOSED.' % t)

def usage():
    usage =  update_hlland.__doc__ + '\n'
    usage += u'用法: %s docdate docno password\n'
    usage += u'docdate: 文件日期\n'
    usage += u'docno: 文號\n'
    usage += u'password: 壓縮檔密碼\n'
    return usage % os.path.basename(sys.argv[0]) 

if __name__ == '__main__':
    try:
        if len(sys.argv) < 4: raise IndexError()
        update_hlland(*sys.argv[1:4])
        deploy()
    except IndexError:
        print usage()
