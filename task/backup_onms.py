# coding=utf8
from zipfile import *
import sys, os

pg_dump_path = r'"c:/Program Files/PostgreSQL/9.0/bin/pg_dump.exe"'

def backup_onms(host):
    fp = fpath(host)
    cmd = '%s -h %s -U opennms -f %s opennms' % (pg_dump_path, host, fp)
    os.system(cmd)
    zipfile(fp)
    print u'資料庫已備份至 %s' % fp + '.zip'

def zipfile(fn):
    zfn = fn + '.zip'
    zf = ZipFile(zfn, 'w', ZIP_DEFLATED)
    zf.write(fn)
    zf.close()

def fpath(host):
    import datetime
    today = datetime.date.today()
    return 'onmsdb_%s_%s' % (host.replace('.', '_'), today.strftime('%Y%m%d')) 

# 0.1: 可備出 onms 之 DB
if __name__ == "__main__":
    from optparse import OptionParser
    usage = u"usage: %prog [options]"
    oparser = OptionParser(usage, version="%prog 0.1", 
                          description=u"備份ONMS設定"
             )

    oparser.add_option("-s", "--host", dest="host", 
                       default='192.168.1.13',
                       help=u"指定備出之來源設備 IP")

    oparser.add_option("-c", "--short_cut", dest="short_cut", 
                       choices=['outer', 'inner'],
                       help=u"使用捷徑" \
                            u"inner 表內網、outer 表外網")

    (options, args) = oparser.parse_args()

    if options.short_cut:
        # options short cut
        if options.short_cut == 'outer':
            options.host  = '192.168.1.13'
        elif options.short_cut == 'inner':
            options.host  = '10.66.4.17'
    
    backup_onms(options.host)
