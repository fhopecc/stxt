# coding=utf8
from optparse import OptionParser
import sys, os, re

pattern  = r'(?P<ts>(\w\w\w) \d\d (\d\d:){2}\d\d) '
pattern += '(?P<ip>(\d{1,3}\.){3}\d{1,3}) '
pattern += r'\w+.\w+( [\w_]+=[\w_\-():.]+)+ (msg="(?P<msg>[^"]+)")'

class LogRecord(object):
    def __init__(self, message='', timestamp='', ip=''):
        self.message = message
        self.timestamp = timestamp
        self.ip = ip

    def format(self):
        return self.timestamp + " " + self.message

    def report_title(self): 
        title =  'syslog from %s at %s\n' % (self.ip, 
                                             self.format()[:6])
        title += '=' * len(title) 
        return title

def parselog(str): 
    for r in re.finditer(pattern, str):
        yield LogRecord(r.group('msg'), r.group('ts'), r.group('ip'))

def parsefile(path):
    str = open(path).read()
    return parselog(str)

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

    (options, args) = parser.parse_args()

    if options.date:
        pass        
    else:
        file = args[0]
        logs = parsefile(file)
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
