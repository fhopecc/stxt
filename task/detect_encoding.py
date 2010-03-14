# coding=utf8
from os import path
import os, re
from optparse import OptionParser

def detect(file):
    import chardet
    raw_data=open(file).read(1024)
    result = chardet.detect(raw_data)
    print "%s's encoding is %s" % (file, result['encoding'])

    
if __name__ == "__main__":
    usage = u"usage: %prog FILE"
    parser = OptionParser(usage)
    (options, args) = parser.parse_args()

    if len(args) != 1:
        parser.error("Doesn't specified FILE!")
    detect(args[0])
