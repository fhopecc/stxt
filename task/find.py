# coding=utf8
u'''
0.9 初始版本
1.0 加入執行命令 -e
1.1 未用 -g 篩選則預設使用 .stx, .txt, .html
'''
from optparse import OptionParser
import sys, os, re
from fnmatch import fnmatch
import site

def grep(path, keyword):
    if not keyword:
        print u'必須指定關鍵字'
        exit(1)

    import chardet
    f = open(path)
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
        
def is_matched(options, path):
    if options.pattern:
        return re.match(options.pattern, path)
    elif options.glob:
        return fnmatch(path, options.glob)

def main():
    usage = u"usage: %prog TOP [options]"
    parser = OptionParser(usage, version="%prog 1.0")
    parser.add_option("-k", "--keyword", dest="keyword",
                      help=u"檔名是否含有關鍵字")

    parser.add_option("-p", "--pattern", dest="pattern",
                      default=".*\.(stx|txt|html)", 
                      help=u"檔名是否符合指定的正規表示式樣式")

    parser.add_option("-g", "--glob", dest="glob",
                      help=u"檔名是否符合指定 glob 樣式")

    parser.add_option("-e", "--exec", dest="execute",
                      help=u"每找到一個檔案後執行命令，%s代表檔案。")

    parser.add_option("-t", "--top", type='int', dest="top", 
                       help=u"只印出前幾筆檔案")
 
    (options, args) = parser.parse_args()

    cwd = os.getcwd()
    if len(args) == 1:
        cwd = args[0]
    
    if not os.path.exists(cwd):
        print u'不存在 %s' % cwd
        exit(1)

    count = 0 

    for root, dirs, files in os.walk(cwd):
        for f in files:
            p = os.path.join(root, f)
            if is_matched(options, f):
                count += 1
                if options.keyword:
                    grep(p, options.keyword)
                elif options.execute:
                    os.system(options.execute % ('"%s"' % p))
                else:
                    print p
                if options.top:
                    if count == options.top:
                        exit(0)

if __name__ == "__main__":
    main()
