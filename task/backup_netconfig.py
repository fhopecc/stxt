# coding=utf8
import sys, os
from telnetlib import Telnet

def backup_FG100A(src, dest):
    #cmd = 'execute backup allconfig FG100A 192.168.1.101'
    cmd = 'execute backup full-config tftp FG100A %s' % dest 

    t = Telnet(src)
    sys.stdout.write(t.read_until('login:'))
    t.write('admin\n')
    sys.stdout.write( t.read_until('Password:'))
    t.write('!tsinim9\n')
    sys.stdout.write( t.read_until('#'))
    t.write('%s\n' % cmd)
    sys.stdout.write( t.read_until('OK.'))

def backup_FG50B(src, dest):
    cmd = 'execute backup full-config ftp FG50B 10.66.4.56 administrator !tsinim9'
    t = Telnet('10.66.7.252')
    sys.stdout.write( t.read_until('login:'))
    t.write('admin\n')
    sys.stdout.write( t.read_until('Password:'))
    t.write('!tsinim9\n')
    sys.stdout.write( t.read_until('#'))
    t.write('%s\n' % cmd)
    sys.stdout.write( t.read_until('OK.'))
    t.close()

def backup_EnterasysC2(src, dest):
    t = Telnet('10.66.4.254')
    sys.stdout.write(t.read_until('Username:'))
    t.write('admin\n')
    sys.stdout.write(t.read_until('Password:'))
    t.write('uecicsed\n')
    sys.stdout.write(t.read_until('->'))
    cmd = 'delete configs/EterasysC2_10_66'
    t.write('%s\n' % cmd)
    sys.stdout.write(t.read_until('->'))
    cmd = 'show config all outfile configs/EterasysC2_10_66'
    t.write('%s\n' % cmd)
    sys.stdout.write(t.read_until('->'))
    cmd = 'copy configs/EterasysC2_10_66 tftp://10.66.4.56/EterasysC2_10_66 '
    t.write('%s\n' % cmd)
    sys.stdout.write(t.read_until('->'))
    t.close()

def backupSGS5420():
    import select, paramiko
    paramiko.util.log_to_file('ssh-cmd.log')
    t = paramiko.Transport(('10.66.253.201', 22))
    t.connect(username='admin', password='!tsinim9')

    chan = t.open_session()
    chan.exec_command('./backup_ccl00695.sh')

    if chan.recv_ready: 
        print chan.recv(2048)
        print chan.recv_stderr(2048)

    sys.stdout = os.fdopen(1, 'w', 0)
    t.close()

# 0.1: 可備出外網之 FG100A
# 0.2: 支援 EnterasysC2, FG50B 之設定備份
# 0.3: 修正 FG100A 升級之指令
# 0.4: 改進遠端指令顯示方式
# 0.5: 命令錯誤會自動顯示用法
# 0.6: 加入完成自動郵件通知
if __name__ == "__main__":
    from optparse import OptionParser
    usage = u"usage: %prog [options]"
    oparser = OptionParser(usage, version="%prog 0.5", 
                          description=u"備份網路設備設定"
             )

    oparser.add_option("-s", "--source", dest="src", 
                       help=u"指定須備出之來源設備 IP")

    oparser.add_option("-d", "--destination", dest="dest", 
                       help=u"指定備出存放之目的主機 IP")

    oparser.add_option("-t", "--type", dest="type", 
                       choices=['FG100A', 'FG50B', 'EnterasysC2'],
                       help=u"指定來源設備類型")

    oparser.add_option("-H", "--HLTB", dest="hltb", 
                       choices=['outer', 'l2', 'sco'],
                       help=u"稅處備份快捷，" \
                            u"outer 表外網FG100A、l2 表內網主幹 switch、sco 表 sco 防火牆")
    
    (options, args) = oparser.parse_args()

    if options.hltb:
        # options short cut
        if options.hltb == 'outer':
            options.src  = '192.168.1.254'
            options.dest = '192.168.1.101'
            options.type = 'FG100A'
        elif options.hltb == 'sco':
            options.src  = '10.66.7.252'
            options.dest = '10.66.4.56'
            options.type = 'FG50B'
        elif options.hltb == 'l2':
            options.src  = '10.66.4.254'
            options.dest = '10.66.4.56'
            options.type = 'EnterasysC2'

    if options.src and options.dest:
        globals()['backup_' + options.type](options.src, options.dest)
    else:
        oparser.print_help()
