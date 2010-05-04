# coding=utf8
from __future__ import with_statement
import sys, os, re, logging
import booker
from booker_lexer import MutipleFileLexer

logging.basicConfig(level=logging.DEBUG,
                    format='%(name) %(levelname) %(message)s',
                    filename='stxt.log',
                    filemode='w')
logger = logging.getLogger('stxt')

if __name__ == '__main__':
    from optparse import OptionParser
    usage = u"usage: %prog SOURCE [options]"
    oparser = OptionParser(usage, version="%prog 1.1")
    oparser.add_option("-d", "--debug", action="store_true", 
                      dest="debug", default=False,
                      help=u"檔名是否含有關鍵字")
    oparser.add_option("-t", "--table", action="store_true", 
                      dest="dump_table", default=False,
                      help=u"印出符號表")
    oparser.add_option("-i", "--noinline", action="store_true", 
                      dest="noinline", default=False,
                      help=u"不剖析行內元素")
    oparser.add_option("-p", "--dump", action="store_true", 
                      dest="dump_tree", default=False,
                      help=u"印出文件樹")
    oparser.add_option("-f", "--format", action="store", 
                      dest="format", default=False,
                      help=u"指定輸出格式")

    (options, args) = oparser.parse_args()

    if len(args) < 1:
        oparser.error("Must supply the SOURCE file!")

    src = args[0]

    DEBUG = options.debug

    with open(src) as f:
        d = booker.parse(f.read(), lexer = MutipleFileLexer(src),
            inline = (not options.noinline))
        if options.dump_tree:
            d.dump()
        if options.dump_table:
            d.make_symbol_table()
            print '符號表：'
            d.dump_symbol_table()
