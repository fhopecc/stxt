# coding=utf8
from __future__ import with_statement
from optparse import OptionParser
from datetime import date
import sys, os, re
'''
1.0:備份指定日期區間的log
'''
def parselocaldate(l):
    pat = '(?P<y>\d\d\d)(?P<m>\d\d)(?P<d>\d\d)' 
    m = re.match(pat, l)
    y = int(m.group('y')) + 1911
    mon = int(m.group('m'))
    d = int(m.group('d'))
    return date(y, mon, d)

def strpdate(str):
    pat = r'(?P<m>\d\d)-(?P<d>\d\d)-(?P<y>\d\d\d\d)'
    m = re.match(pat, str)
    y = int(m.group('y'))
    mon = int(m.group('m'))
    d = int(m.group('d'))
    return date(y, mon, d)

def match(f, options):
    if options.all: 
        return True
    pat  = r'(?P<d>\d\d-\d\d-\d\d\d\d)\.'
    pat += r'\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}'
    pat += r'\.log(\s|$)'
    res = False
    m = re.match(pat, f)
    if m:
        res = True
        if options.before:
            d = m.group('d')
            d = strpdate(d)
            b = parselocaldate(options.before)
            res = d < b
    return res


if __name__ == "__main__":
    usage = u"usage: %prog logpath [options]"
    parser = OptionParser(usage, version="%prog 1.1", 
             description=u"檢視、摘要、列印 syslog"
        )
    parser.add_option("-b", "--before", dest="before", 
                       help=u"列出指定日期前之日誌，例：0990902")

    parser.add_option("-a", "--all", action="store_true",
                      dest="all", 
                      help=u"列出指定根目錄之所有日誌檔，不作任何動作")

    parser.add_option("-t", "--tar", action="store_true",
                      dest="tar", 
                      help=u"加入壓縮檔")

    parser.add_option("-R", "--remove", action="store_true",
                      dest="remove", 
                      help=u"加入壓縮檔後刪除原檔案")



    (options, args) = parser.parse_args()

    from datetime import date

    
    if len(args) < 1:
        print u'請指定日誌檔之根目錄'
        sys.exit()

    if not os.path.exists(args[0]):
        print u'日誌檔之根目錄不存在'
        sys.exit()       

    bak = r'%s.logs.bak.tar' % date.today().strftime('%Y%m%d')

    if options.tar:
        import tarfile
        tar = tarfile.TarFile(bak, 'w')
                
    #import pdb; pdb.set_trace()
    for f in os.listdir(args[0]):
        if match(f, options):
            p = os.path.join(args[0], f)
            print p
            if options.tar:
                tar.add(p)
                if options.remove:
                    os.remove(p)
    if options.tar:
        tar.close()
