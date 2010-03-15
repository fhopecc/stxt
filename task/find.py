# coding=utf8
from optparse import OptionParser
from os import path
import os, re

def find_keyword(path, keyword):
    f = open(path)
    if not keyword:
        print u'必須指定關鍵字'
        exit(1)

    import chardet
    result = chardet.detect(f.read(1024))
    encode = result['encoding']
    f.close()

    f = open(path)
    for i, l in enumerate(f):
        try:
            l = l.decode(encode)
            if re.search(keyword.decode('cp950'), l):
                print u"%s(%s):%d: %s" % (path.decode('cp950'), encode, i, l)
        except UnicodeDecodeError:
            try:
                print u'%s(%s):%d: decode error' % (path.decode('cp950'), encode, i)
            except:
                print "error happen"
    f.close()
        

def find_dir(dir, keyword):
    pattern = '(.*\.stx|.*\.txt|.*\.xml|.*\.fdoc|.*\.erb)$'
    for root, dirs, files in os.walk(dir):
        for f in files:
            if re.match(pattern, f):
                find_keyword(path.join(root, f), keyword)

def main():
    usage = u"usage: %prog START [options]"
    parser = OptionParser(usage)
    parser.add_option("-k", "--keyword", dest="keyword",
                      help=u"檔案是否含有關鍵字")
    (options, args) = parser.parse_args()

    if len(args) != 1:
        parser.error("Must enter START.")
    
    if not path.exists(args[0]):
        print u'不存在 %s' % args[0]
        exit(1)
    if path.isfile(args[0]):    
        print u'%s 是一個檔案' % args[0]
        find_keyword(args[0], options.keyword)
    elif path.isdir(args[0]):
        print u'%s 是一個目錄' % args[0]
        find_dir(args[0], options.keyword)
    
if __name__ == "__main__":
    main()
