# coding=utf8
import sys, os
from telnetlib import Telnet

def backup_FG100A(src, dest):
    #cmd = 'execute backup allconfig FG100A 192.168.1.101'
    cmd = 'execute backup allconfig FG100A %s' % dest 
    t = Telnet(src)
    print t.read_until('login:')
    t.write('ccl00695\n')
    print t.read_until('Password:')
    t.write('btmw_111\n')
    print t.read_until('$')
    t.write('%s\n' % cmd)
    print t.read_until('done.')

# 0.1: 可備出外網之 FG100A

if __name__ == "__main__":
    from optparse import OptionParser
    usage = u"usage: %prog [options]"
    oparser = OptionParser(usage, version="%prog 0.1", 
                          description=u"備份網路設備設定"
             )

    oparser.add_option("-s", "--source", dest="src", 
                       help=u"指定須備出之來源設備 IP")

    oparser.add_option("-d", "--destination", dest="dest", 
                       help=u"指定備出存放之目的主機 IP")

    oparser.add_option("-t", "--type", dest="type", 
                       choices=['FG100A', 'FG50A'],
                       help=u"指定來源設備類型")

    oparser.add_option("-H", "--HLTB", dest="hltb", 
                       choices=['outer', 'inner'],
                       help=u"稅處備份快捷，inner 表內網，outer 表外網")
    
    (options, args) = oparser.parse_args()

    if options.hltb:
        # options short cut
        if options.hltb == 'outer':
            options.src  = '192.168.1.254'
            options.dest = '192.168.1.101'
            options.type = 'FG100A'

    globals()['backup_' + options.type](options.src, options.dest)

