# coding=utf8
from optparse import OptionParser
import sys, os, re

#MONPAT = r'(Jan|Feb|Mar|Apr|Aug|Sep|Oct|Nov|Dec)'
#3CHEAD  = r'%s \d\d (\d\d:){2}\d\d ' % MONPAT
#3CHEAD += r'(\d{1,3}\.){3}\d{1,3} '
#3CHEAD += r'\w+\.\w+ '

pattern  = r'(?P<ts>(\w\w\w) \d\d (\d\d:){2}\d\d) '
pattern += '(?P<ip>(\d{1,3}\.){3}\d{1,3}) '
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

def parse3clog(str):
    mon = r'(Jan|Feb|Mar|Apr|Aug|Sep|Oct|Nov|Dec)'
    head  = r'(%s \d\d (\d\d:){2}\d\d ' % mon
    head += r'(\d{1,3}\.){3}\d{1,3} '
    head += r'\w+\.\w+)\s'
    pat = r'(?P<head>%s)'% head
    pat += r'(?P<msg>.*?)((?=%s)|\Z)' % head


    for r in re.finditer(pat, str, re.M|re.S):
        yield (r.group('head'), r.group('msg'))


r'(?P<head>((Jan|Feb|Mar|Apr|Aug|Sep|Oct|Nov|Dec) \\d\\d (\\d\\d:){2}\\d\\d (\\d{1,3}\\.){3}\\d{1,3} \\w+\\.\\w+) )(?P<msg>.*)(?=((Jan|Feb|Mar|Apr|Aug|Sep|Oct|Nov|Dec) \\d\\d (\\d\\d:){2}\\d\\d (\\d{1,3}\\.){3}\\d{1,3} \\w+\\.\\w+) )'

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

#import pdb; pdb.set_trace()

if __name__ == "__main__":
    usage = u"usage: %prog log [options]"
    parser = OptionParser(usage, version="%prog 1.0", 
             description=u"檢視、摘要、列印 syslog"
        )
    parser.add_option("-t", "--top", type='int', dest="top", 
                       help=u"只印出前幾筆記錄")

    parser.add_option("-d", "--date", dest="date", 
                       help=u"列出指定日期之紀錄，尚未完成")

    parser.add_option("-p", "--hardcopy", action="store_true",
                      dest="hardcopy", 
                      help=u"列印至印表機")

    parser.add_option("-f", "--format", type="choice", dest="format", 
                      choices=['fortinet', 'enterasys'],
                      default='fortinet', 
                      help=u"指定訊息格式")


    (options, args) = parser.parse_args()

    if options.date:
        pass        
    else:
        file = args[0]
        logs = parsefile(file, options.format)
        first = True

        if options.top: i = 0
        if options.hardcopy: tmplog = open('tmplog.txt', 'w')

        for l in logs:
            if first: 
                print l.report_title(); 
                if options.hardcopy: tmplog.write(l.report_title()+'\n')
                first = False

            print l.format()
            if options.hardcopy:
                tmplog.write(l.format()+'\n')

            if options.top: 
                i+=1
                if i >= options.top: exit()
        if options.hardcopy:      
            tmplog.close()
            hardcopy('tmplog.txt')
