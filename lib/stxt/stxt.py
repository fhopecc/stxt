# coding=utf8
# stxt 之前端命令介面
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
                       help=u"除錯模式")
    oparser.add_option("-i", "--noinline", action="store_true", 
                       dest="noinline", default=False,
                       help=u"不剖析行內元素")
    oparser.add_option("-f", "--format", dest="format", 
                       choices=['html', 'doctree', 'name_table',
                                'web', 'slide'],
                       default='html',
                       help=u"指定輸出格式")

    (options, args) = oparser.parse_args()

    if len(args) < 1:
        oparser.error("Must supply the SOURCE file!")

    src = args[0]

    DEBUG = options.debug

    with open(src) as f:
        d = booker.parse(f.read(), lexer = MutipleFileLexer(src),
            inline = (not options.noinline))
        d.make_symbol_table()
        d.file = src # this parameter is used by web formatter

        if options.format == 'doctree':
            d.dump()
        elif options.format == 'name_table':
            d.dump_symbol_table()
        elif options.format == 'html':
            from formater import html
            print html.to_html(src)
        elif options.format == 'web':
            from formater import web
            web.to_doc(d)
