# coding=utf8
from optparse import OptionParser
from os import path
import sys, os, re
from fnmatch import fnmatch
import site

def grep(path, keyword):
    f = open(path)
    if not keyword:
        print u'必須指定關鍵字'
        exit(1)

    import chardet
    result = chardet.detect(f.read(1024))
    encode = result['encoding']
    f.close()
    if not encode:
        encode = 'utf8'

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
        

def find_by_pattern(dir, pattern):
    for root, dirs, files in os.walk(dir):
        for f in files:
            if re.match(pattern, f):
                print path.join(root, f)

def find_by_glob(dir, pattern):
    from fnmatch import fnmatch
    for file in os.listdir('.'):
        if fnmatch.fnmatch(file, '*.txt'):
            print file

def find_by_keyword(dir, keyword):
    pattern = r'.*%s.*' % keyword
    find_by_pattern(dir, pattern)

def main():
    usage = u"usage: %prog START [options]"
    parser = OptionParser(usage)
    parser.add_option("-k", "--keyword", dest="keyword",
                      help=u"檔名是否含有關鍵字")

    parser.add_option("-p", "--pattern", dest="pattern",
                      help=u"檔名是否符合指定的正規表示式樣式")

    parser.add_option("-g", "--glob", dest="glob",
                      help=u"檔名是否符合指定 glob 樣式")
 
    (options, args) = parser.parse_args()

    if len(args) != 1:
        print u"必須指定 START"
        exit(1)
    
    if not path.exists(args[0]):
        print u'不存在 %s' % args[0]
        exit(1)

    for root, dirs, files in os.walk(args[0]):
        for f in files:
            p = path.join(root, f)
            if options.pattern:
                if re.match(options.pattern, f):
                    if options.keyword:
                        grep(p, options.keyword)
                    else:
                        print p
            elif options.glob:
                if fnmatch(f, options.glob):
                    if options.keyword:
                        grep(p, options.keyword)
                    else:
                        print p
            else:
                print p

if __name__ == "__main__":
    main()
