# coding=utf-8
from __future__ import with_statement
from spark import *
from node import Node
from datetime import date
import os, template

class Token(object):
    def __init__(self, type, value):
        self.type = type
        self.value = value

    def __cmp__(self, o):
		    return cmp(self.type, o)

    def __str__(self):
        #print self.value.decode('utf8')
        return "%s" % (self.type)

class Lexer(GenericScanner):
    "Tokenize words, punctuation and markup"
    def tokenize(self, input):
        self.rv = []
        GenericScanner.tokenize(self, input)
        return self.rv

    def t_emptyline(self, s):
        ur"\s*\n"
        self.rv.append(Token('emptyline', ' '))

    def t_titlesep(self, s):
        ur"[-]+\n"
        self.rv.append(Token('titlesep', s))

    def t_label(self, s):
        ur".+：\n"
        self.rv.append(Token('label', s.strip()))

    def t_date(self, s):
        ur"\d{8}\n"
        self.rv.append(Token('date', s[:-1]))

    def t_line(self, s):
        ur"((\d+[^.])|([^\d\-])).+\n"
        self.rv.append(Token('line', s.strip()))

class Parser(GenericParser):
    def __init__(self, start='doc'):
        GenericParser.__init__(self, start)

    def p_doc(self, args):
        ''' doc ::= sects
        '''
        doc = Node(type="doc")

        for s in args[0]:
            doc.append(s)
        return doc

    def p_title(self, args):
        '''title ::= line titlesep emptyline
           title ::= date titlesep emptyline'''
        return args[0].value

    def p_init_list(self, args):
        '''docattrs ::= docattr
           sects    ::= sect
           paras    ::= para
           contents ::= content
        '''
        return [args[0]]

    def p_lists(self, args):
        '''paras    ::= paras emptyline para
           sects    ::= sects emptyline sect
           contents ::= contents emptyline content 
        '''
        args[0].append(args[2])
        return args[0]

    def p_sect(self, args):
        ''' sect ::= title contents
        '''
        sect = Node(type='sect')
        sect.title = args[0]
        ps = args[1]

        for p in ps:
            sect.append(p)
        return sect

    def p_content(self, args):
        ''' content ::= subsect
            content ::= para
        '''
        return args[0]

    def p_subsect(self, args):
        ''' subsect ::= label emptyline paras
        '''
        subsect = Node(type='subsect')
        subsect.title = args[0].value[:-1]
        ps = args[2]

        for p in ps:
            subsect.append(Node(type='para', value = p.value))

        return subsect

    def p_para_0(self, args):
        ''' para ::= line
        '''
        return Node('para', args[0].value.strip())

    def p_para(self, args):
        ''' para ::= para line
        '''
        args[0].value += args[1].value
        return args[0]

class MakeCheckList(GenericASTTraversal):
    def __init__(self, ast):
        GenericASTTraversal.__init__(self, ast)
        self.checks = []
        self.preorder()

    def n_sect(self, node):
        d = {}
        d['date'] = node.title
        for s in node.kids:
            v = ""
            for p in s.kids:
                v += p.value
            d[s.title] = v
        self.checks.append(d)
        #print d
        
class TreeDump(GenericASTTraversal):
    def __init__(self, ast):
        GenericASTTraversal.__init__(self, ast)
        self.preorder()
        self.current_sect = None

    def n_sect(self, node):
        print '%s%s:%s' % ('*' * node.height, node.type,
                                 node.title)

    def n_subsect(self, node):
        print '%s%s:%s' % ('*' * node.height, node.type,
                                 node.title)

    def n_para(self, node):
        print '%s%s' % ('*' * node.height, node.type)
        print node.value

    def default(self, node):
        print '%s%s' % ('*' * node.height, node.type)

def pad_check(check):
    c = check
    try:
        c[u'外網防火牆']
        c[u'內網防火牆']
    except KeyError, k:
        c[k.message] = '正常'
        #import pdb; pdb.set_trace()
    return c

def find_check(checks, options):
    logdate = (date.today()).strftime('%Y%m%d')
    if options.date:
        logdate = unicode(options.date)

    cs = [c for c in checks if c['date'] == logdate]
    if len(cs) == 0:
        c = {}
        c['date'] = logdate
        c[u'外網防火牆'] = u'正常'
        c[u'內網防火牆'] = u'正常'
        return c
    return pad_check(cs[0])

# 0.1 預設的 HTML 報告
# 0.2 加入內網防火牆項目
if __name__ == '__main__':
    from optparse import OptionParser
    usage = u"usage: %prog SOURCE [options]"
    oparser = OptionParser(usage, version="%prog 1.1")

    oparser.add_option("-c", "--fileencoding", dest="fileencoding", 
                       default='utf8',
                       help=u"指定輸入之字串編碼")

    oparser.add_option("-d", "--date", dest="date", 
                       help=u"列出指定日期之日誌，未指定則為昨天，日期格式例子：20101217")

    oparser.add_option("-o", "--output", dest="output", 
                       default='test.html',
                       help=u"輸出檔案")

    oparser.add_option("-f", "--format", dest="format", 
                       choices=['html', 'tree', 'msword', 
                                'tokens', 'checks'],
                       default='tree',
                       help=u"指定輸出格式")

    oparser.add_option("-D", "--debug", action="store_true", 
                       dest="debug", default=False,
                       help=u"除錯模式")

    (options, args) = oparser.parse_args()

    if len(args) < 1:
        #oparser.error("Must supply the SOURCE file!")
        src = r'doc\hltb\netchecks.txt'
    else:
        src = args[0]
        
    with open(src) as f:
        source = f.read() 

        # To unicode string
        ustr = source.decode(options.fileencoding)
        if ustr[0] == u'\ufeff':
            ustr = ustr[1:]

        # Why ustr[0] is u+feff
        tokens = Lexer().tokenize(ustr)
        if options.format == 'tokens':
            for t in tokens:
                print t.type
            exit()

        doc = Parser().parse(tokens)
        #MakeDocTree(doc)
        if options.format == 'tree':
            TreeDump(doc)
        elif options.format == 'checks':
            l = MakeCheckList(doc)
            c = find_check(l.checks, options)
            print c
        elif options.format == 'html':
            tempf = os.path.join(os.path.dirname(__file__), 'netcheck.html')
            net_check = template.frender(tempf)

            l = MakeCheckList(doc)
            check = find_check(l.checks, options)
            #check = checks[0]
            #import pdb;pdb.set_trace()
            #print net_check('0991110', check)
            if options.output:
                with open(options.output, 'w') as f:
                    f.write(str(net_check(check[u'date'], check)))
