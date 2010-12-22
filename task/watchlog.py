# coding=utf8
from __future__ import with_statement
from optparse import OptionParser
from datetime import date
from datetime import timedelta
import sys, os, re

# TODO
#
# 預設為查看原始 LOG 檔，Raw 改為含 3CSyslog 的標頭。
# 將查看最近日期記在 .wathlog 中
# 
pattern  = r'(?P<ts>(\w\w\w) \d\d (\d\d:){2}\d\d) '
pattern += r'(?P<ip>(\d{1,3}\.){3}\d{1,3}) '
pattern += r'\w+.\w+( [\w_]+=[\w_\-():.]+)+ (msg="(?P<msg>[^"]+)")'

class LogRecord(object):
    def __init__(self, message='', timestamp='', hostname='', 
                 facility='', servity=''):
        self.message = message
        self.timestamp = timestamp
        self.hostname = hostname
        self.facility = facility
        self.servity = servity

    def format(self):
        return "%s %s %s " % (self.timestamp, self.servity,
            self.message)

    def report_title(self): 
        title =  'syslog from %s at %s\n' % (self.hostname, 
                                             self.format()[:6])
        title += '=' * len(title) 
        return title

def parse3cheader(header):
    mon = r'(?P<mon>Jan|Feb|Mar|Apr|Aug|Sep|Oct|Nov|Dec)'
    pat = r'(?P<ts>%s \d\d (\d\d:){2}\d\d) ' % mon
    pat += r'(?P<ip>(\d{1,3}\.){3}\d{1,3}) '
    pat += r'(?P<fact>\w+)\.(?P<ser>\w+) '

def read3clogs(str):
    mon = r'(Jan|Feb|Mar|Apr|Aug|Sep|Oct|Nov|Dec)'
    head  = r'(%s \d\d (\d\d:){2}\d\d ' % mon
    head += r'(\d{1,3}\.){3}\d{1,3} '
    head += r'\w+\.\w+)\s'
    pat = r'(?P<head>%s)'% head
    pat += r'(?P<msg>.*?)((?=%s)|\Z)' % head

    for r in re.finditer(pat, str, re.M|re.S):
        yield (r.group('head'), r.group('msg'))

def parselog(str): 
    for r in re.finditer(pattern, str):
        yield LogRecord(r.group('msg'), r.group('ts'), r.group('ip'))

def parse(pattern, str): 
    for r in re.finditer(pattern, str):
        yield LogRecord(r.group('msg'), r.group('ts'), r.group('ip'), 
                        r.group('fact'), r.group('ser'))

def parse_fortinet_log(str):
    pattern  = r'(?P<ts>%s \d\d (\d\d:){2}\d\d) ' % MONPAT
    pattern += '(?P<ip>(\d{1,3}\.){3}\d{1,3}) '
    pattern += r'\w+.\w+( [\w_]+=[\w_\-():.]+)+ (msg="(?P<msg>[^"]+)")'


def parse_enterasys_log(str):
    pattern  = r'(?P<ts>(\w\w\w) \d\d (\d\d:){2}\d\d) '
    pattern += r'(?P<ip>(\d{1,3}\.){3}\d{1,3}) '
    pattern += r'(?P<fact>\w+)\.(?P<ser>\w+)  '
    pattern += r'[^%]+ %% (?P<msg>[^\n]*)(\n)?'

    return parse(pattern, str)

def parsefile(path, format):
    str = open(path).read()
    if format == 'fortinet':
        return parselog(str)
    elif format == 'enterasys':
        return parse_enterasys_log(str)

def hardcopy(path):
    cmd = 'notepad /P %s' % path
    os.system(cmd)

'''
1.0:檢視、摘要、列印 syslog
1.1:加入 raw 選項
1.2:加入 HLTB 選項，作為稅處的網管人員之快捷用
1.2.1:加入 HLTB 選項，可設定 -p 作為列印用
1.3:實作 -d 選項，列出指定日期之紀錄
1.4.1:HLTB 選項，由命令列捷徑改為選項組，更改 options 之選項值，
      而非組出一條的命令。
1.4.2:實作輸入多個 LOG 檔。TODO
'''
if __name__ == "__main__":
    usage = u"usage: %prog log [options]"
    parser = OptionParser(usage, version="%prog 1.4.1", 
             description=u"檢視、摘要、列印 syslog"
        )
    parser.add_option("-t", "--top", type='int', dest="top", 
                       help=u"只印出前幾筆記錄")

    parser.add_option("-d", "--date", dest="date", 
                       help=u"列出指定日期之紀錄，未指定則為昨天，日期格式例子：12-17-2010")

    parser.add_option("-D", "--direct", action="store_true",
                       dest="direct", 
                       help=u"直接列出紀錄內容")

    parser.add_option("-p", "--hardcopy", action="store_true",
                      dest="hardcopy", 
                      help=u"列印至印表機")

    parser.add_option("-r", "--raw", action="store_true",
                      dest="raw", 
                      help=u"僅以行分割基礎 3C 之紀錄格式")

    parser.add_option("-f", "--format", type="choice", dest="format", 
                      choices=['fortinet', 'enterasys'],
                      default='fortinet', 
                      help=u"指定訊息格式")

    parser.add_option("-H", "--hltb", type="choice", dest="hltb", 
                      choices=['outer', 'inner'],
                      help=u"指定稅局內網或外網")

    (options, args) = parser.parse_args()

    logdate = (date.today() - timedelta(days=1)).strftime('%m-%d-%Y')
    if options.date:
        logdate = options.date

    if options.hltb:
        # options short cut
        if options.hltb == 'outer':
            logfile = r'\\99tt005\syslog\%s.192.168.1.254.log' % logdate

            if len(args) == 0:
                args.append(logfile)
            else:
                args[0] = logfile 
            options.raw = True
        elif options.hltb == 'inner':
            cmd = r'%s -r \\99tt004\log\%s.10.66.4.254.log' % \
                  (__file__, logdate)
            if options.hardcopy: cmd += ' -p'
            cmd += ' | more'
            os.system(cmd)

            cmd = r'%s -r \\99tt004\log\%s.10.66.7.252.log' % \
                  (__file__, logdate)
            if options.hardcopy: cmd += ' -p'
            cmd += ' | more'
            os.system(cmd)
            exit()

    if options.date:
        pass        
    else:
        logfile = args[0]
        if options.direct:
            with open(logfile, 'r') as f:
                #import pdb;pdb.set_trace()
                print f.read()
                exit()

        if options.raw:
            with open(logfile, 'r') as f:
                logs = read3clogs(f.read())
        else:
            logs = parsefile(logfile, options.format)
        first = True

        if options.top: i = 0
        if options.hardcopy: tmplog = open('%s.tmp' % logfile, 'w')

        for l in logs:
            if first and not options.raw: 
                print l.report_title(); 
                if options.hardcopy: tmplog.write(l.report_title()+'\n')
                first = False

            if options.raw:
                print l[0]
                print l[1]
                if options.hardcopy:
                    tmplog.write(l[0]+'\n'+l[1] + '\n')
            else:
                print l.format()
                if options.hardcopy:
                    tmplog.write(l.format()+'\n')

            if options.top: 
                i+=1
                if i >= options.top: break
        if options.hardcopy:      
            tmplog.close()
            hardcopy('%s.tmp' % logfile)
