# coding=utf-8
from __future__ import with_statement
from spark import *
from node import Node

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

    def t_docattr(self, s):
        ur"[^:\n]+[:][^:\n]+\n"
        self.rv.append(Token('docattr', s.strip()))

    def t_secnumber(self, s):
        ur"(\d+\.)+"
        self.rv.append(Token('secnumber', s))

    def t_line(self, s):
        ur"((\d+[^.])|([^\d\-])).+\n"
        self.rv.append(Token('line', s.strip()))

class Parser(GenericParser):
    def __init__(self, start='doc'):
        GenericParser.__init__(self, start)

    def p_doc(self, args):
        ''' doc ::= title docattrs emptyline sects
            doc ::= title sects
        '''
        doc = Node(type="doc", title=args[0])
        if len(args) == 4:
            attrs = {}
            for a in args[1]:
                a = a.value.split(':')
                attrs[a[0]]=a[1]
            doc.attrs = attrs
            for s in args[3]:
                doc.append(s)
        else:
            for s in args[1]:
                doc.append(s)
            
        return doc

    def p_title(self, args):
        ' title ::= line titlesep emptyline'
        return args[0].value

    def p_init_list(self, args):
        '''docattrs ::= docattr
           sects ::= sect
           paras ::= para
        '''
        return [args[0]]

    def p_lists(self, args):
        '''docattrs ::= docattrs docattr
           sects ::= sects sect
        '''
        args[0].append(args[1])
        return args[0]

    def p_paras(self, args):
        'paras ::= paras emptyline para'
        args[0].append(args[2])
        return args[0]

    def p_sect_0(self, args):
        ''' sect ::= secnumber line emptyline
            sect ::= secnumber emptyline
        '''
        sect = Node(type='sect')
        sect.secnumber = args[0].value
        if len(args) == 3:
            sect.append(Node(type='para', value = args[1].value))
        return sect

    def p_sect(self, args):
        ''' sect ::= secnumber line emptyline paras emptyline
            sect ::= secnumber emptyline paras emptyline
            sect ::= secnumber line emptyline paras
            sect ::= secnumber emptyline paras
        '''
        sect = Node(type='sect')
        sect.secnumber = args[0].value
        if len(args) == 5:
            sect.append(Node(type='para', value = args[1].value))
            ps = args[3]
        else:
            ps = args[2]

        for p in ps:
            sect.append(Node(type='para', value = p.value))
        return sect

    def p_para_0(self, args):
        ''' para ::= line
        '''
        return Node('para', args[0].value.strip())

    def p_para(self, args):
        ''' para ::= para line
        '''
        args[0].value += args[1].value
        return args[0]

class MakeDocTree(GenericASTTraversal):
    def __init__(self, ast):
        GenericASTTraversal.__init__(self, ast)
        self.sect_stack = []
        self.preorder()

    @property
    def level(self):
        return len(self.sect_stack)

    @property
    def current_sect(self):
        return self.sect_stack[-1]

    def push(self, node):
        if self.level > 0:
           node.parent =  self.current_sect
        self.sect_stack.append(node)

    def pop(self):
        self.sect_stack.pop()

    def n_sect(self, node):
        node.numbers = node.secnumber.split('.')[:-1]
        node.level = len(node.numbers)

        if self.level == 0:
            self.push(node)
        elif self.level == node.level:
            brother = self.pop()
            self.push(node)
        elif self.level < node.level:
            self.push(node)
        elif self.level > node.level:
            diff = self.level - node.level
            for i in range(diff + 1):
                self.pop()
            self.push(node)
        
    def n_para(self, node):
        node.parent = self.current_sect

class TreeDump(GenericASTTraversal):
    def __init__(self, ast):
        GenericASTTraversal.__init__(self, ast)
        self.preorder()
        self.current_sect = None

    def n_sect(self, node):
        print '%s%s[%s]:%s' % ('*' * node.height, node.type,
                               node.secnumber, node.title)

    def n_para(self, node):
        print '%s%s' % ('*' * node.height, node.type)
        print node.value

    def default(self, node):
        print '%s%s' % ('*' * node.height, node.type)

# 1.1 輸入均轉為 unicode 串流 
if __name__ == '__main__':
    from optparse import OptionParser
    usage = u"usage: %prog SOURCE [options]"
    oparser = OptionParser(usage, version="%prog 1.1")

    oparser.add_option("-c", "--fileencoding", dest="fileencoding", 
                       default='utf8',
                       help=u"指定輸入之字串編碼")

    oparser.add_option("-f", "--format", dest="format", 
                       choices=['tree', 'msword', 'tokens'],
                       default='tree',
                       help=u"指定輸出格式")

    oparser.add_option("-D", "--debug", action="store_true", 
                       dest="debug", default=False,
                       help=u"除錯模式")

    (options, args) = oparser.parse_args()

    if len(args) < 1:
        oparser.error("Must supply the SOURCE file!")

    src = args[0]
    with open(src) as f:
        source = f.read() 

        # To unicode string
        ustr = source.decode(options.fileencoding)

        # tokens = Lexer().tokenize(f.read())
        tokens = Lexer().tokenize(ustr)
        if options.format == 'tokens':
            for t in tokens:
                print t.type
                exit()

        doc = Parser().parse(tokens)
        MakeDocTree(doc)
        if options.format == 'tree':
            TreeDump(doc)
        elif options.format == 'msword':
            from formater import word
            word.MSWordOut(doc)
