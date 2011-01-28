# coding=utf8
u'''
0.9 初始版本
1.0 加入執行命令 -e
'''
from optparse import OptionParser
from os import path
import sys, os, re

def c_style_name(name):
    ps = [r'(?P<n>([A-Z][a-z]*\s)+).*(?P<e>\.[a-zA-Z]+)',
          r'(?P<n>(([A-Z][a-z]*)|&)+).*(?P<e>\.[a-zA-Z]+)']
    m = re.match(ps[0], name)
    if m:
        name = m.group('n')
        name = name.lower()
        if name[-1] == ' ': name = name[:-1]
        name = name.replace(' ', '_')
        return name + m.group('e') 
    m = re.match(ps[1], name)
    if m:
        name = m.group('n')
        name = name.replace('&', 'And')
        name = re.sub(r'[A-Z]',r' \g<0>', name)
        name = name.lower()
        if name[0] == ' ': name = name[1:]
        name = name.replace(' ', '_')
        return name + m.group('e') 


    return name

if __name__ == "__main__":
    usage = u"usage: %prog path [options]"
    parser = OptionParser(usage, version="%prog 1.0")
    parser.add_option("-r", action="store_true", dest="rename", 
                      default = False, 
                      help=u"將指定 path 重新命名成 c 變數風格的檔名")

    (options, args) = parser.parse_args()

    if len(args) < 1:
        parser.error('path must be specified!')
    p  = args[0]
    d  = path.dirname(p)
    n  = path.basename(p)
    cn = c_style_name(path.basename(p)) 
    if options.rename:
        os.rename(p, path.join(d, cn))
    print cn
