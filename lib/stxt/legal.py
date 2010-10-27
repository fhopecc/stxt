# coding=utf-8
from __future__ import with_statement
from spark import *
from tree import Node
import win32com
from win32com.client import Dispatch, constants, DispatchEx

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

    #def t_whitespace(self, s):
    #    r" [ \t\r]+ "
    #    self.rv.append(Token('whitespace', ' '))
    #    pass

    def t_emptyline(self, s):
        r"\s*\n"
        self.rv.append(Token('emptyline', ' '))

    def t_titlesep(self, s):
        r"[-]+\n"
        self.rv.append(Token('titlesep', s))

    def t_docattr(self, s):
        r"[^:\n]+[:][^:\n]+\n"
        self.rv.append(Token('docattr', s.strip()))

    def t_secnumber(self, s):
        r"(\d+\.)+"
        self.rv.append(Token('secnumber', s))

    def t_line(self, s):
        r"[^\d\s-][^\n]+\n"
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
           paras ::= paras emptyline para
        '''
        args[0].append(args[1])
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
        '''
        sect = Node(type='sect')
        sect.secnumber = args[0].value
        print sect.secnumber
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

class SectLevel(GenericASTTraversal):
    def __init__(self, ast):
        GenericASTTraversal.__init__(self, ast)
        self.preorder()
        self.current_sect = None

    def n_sect(self, node):
        node.numbers = node.secnumber.split('.')[:-1]
        node.level = len(node.numbers)
        
        self.current_sect = node
        print node.type
        print node.level
        print node.secnumber

    def n_para(self, node):
        #pass
        #import pdb;pdb.set_trace()
        #import pdb;pdb.set_trace()
        #self.current_sect = node
        node.parent = self.current_sect

    def default(self, node):
        print node.type

class WordOut(GenericASTTraversal):
    def __init__(self, ast):
        GenericASTTraversal.__init__(self, ast)
        msword = DispatchEx('Word.Application')
        msword.Visible = 1	# 1表示要顯示畫面，若為0則不顯示畫面。

        self.word = msword
        self.doc 	= msword.Documents.Add() # 開啟一個新的文件。
        self.range	= self.doc.Range()
        self.range.Style.Font.Name = u"標楷體".encode('cp950')  # 設定字型為標楷體

        self.range.Style.Font.Size = 12
        self.range.Style.Font.Bold = 0
        self.preorder()

    def n_doc(self, node):

        self.range.InsertAfter(node.title + '\n')

    def n_sect(self, node):
        #import pdb;pdb.set_trace()
        self.range.InsertAfter(self.sect_num(node))
        if len(node.kids) == 0:
            self.range.InsertAfter('\n')

    def n_para(self, node):
        r	= self.range
        r.InsertAfter(node.value + '\n')
        for i in range(node.parent.level):
            self.range.Paragraphs.Indent

    def sect_num(self, node):
        cbd = [u'零',u'壹',u'貳',u'參',u'肆',u'伍',u'陸',u'柒',u'捌',u'玖','拾',
             '拾壹','拾貳','拾參','拾肆','拾伍','陸','柒','捌','玖','拾']
        cd = [u'零',u'一',u'二',u'三',u'四',u'五',u'六',u'七',
              u'八',u'九',u'十', u'十一','十二','十三','十四',
              '十五','十六','十七','十九','二十'
              '二十一','二十二','二十三','二十四','二十五','二十六',
              '二十七','二十八','二十九', '三十', 
              '三十一','三十二','三十三','三十四','三十五','三十六',
              '三十七','三十八','三十九'
             ]
        n = int(node.numbers[-1])
        spaces = (u'　' * (node.level - 1) * 2).encode('big5')
        if node.level == 1:
            return cd[n].encode('big5') + '.'
        elif node.level == 2:
            return spaces + \
                  '(' + cd[n].encode('big5') + ')' + ' '
        elif node.level == 3:
            return spaces + str(n) + '.'
        elif node.level == 4:
            return spaces + '(' + str(n) + ')' + ' '
        else:
            return spaces + str(n) + '.'

    def default(self, node):
        # this handles + and * nodes

        print node.type


if __name__ == '__main__':
    from optparse import OptionParser
    usage = u"usage: %prog SOURCE [options]"
    oparser = OptionParser(usage, version="%prog 1.1")
    oparser.add_option("-d", "--debug", action="store_true", 
                       dest="debug", default=False,
                       help=u"除錯模式")

    (options, args) = oparser.parse_args()

    if len(args) < 1:
        oparser.error("Must supply the SOURCE file!")

    src = args[0]
    with open(src) as f:
        tokens = Lexer().tokenize(f.read())
        print len(tokens)
        doc = Parser().parse(tokens)
        SectLevel(doc)
        WordOut(doc)
