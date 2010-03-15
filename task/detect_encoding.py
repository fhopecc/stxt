# coding=utf8
from os import path
import os, re
from optparse import OptionParser

def detect(file):
    raw_data=open(file).read(1024)
    detect_data(raw_data)
    print file

def detect_data(data):
    import chardet
    result = chardet.detect(data)
    print "encoding is %s" % result['encoding']
    
if __name__ == "__main__":
    usage = u"usage: %prog FILE"
    parser = OptionParser(usage)

    parser.add_option("-d", "--data", dest="data",
                  help="raw data", metavar="DATA")

    (options, args) = parser.parse_args()

    if options.data:
        detect_data(options.data)
        print options.data.decode('big5')
    else:
        if len(args) != 1:
            parser.error("Doesn't specified FILE!")
        detect(args[0])
