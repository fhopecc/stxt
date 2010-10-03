# coding=utf-8
from spark import *
from tree import Node

class Token(object):
    def __init__(self, type, value):
        self.type = type
        self.value = value

	def __cmp__(self, o):
		return cmp(self.type, o)

    def __str__(self):
        return "%s:%s" % (self.type, self.value.decode('utf8'))

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
        r"(\d\.)+"
        self.rv.append(Token('secnumber', s))

    def t_line(self, s):
        r"[^-\d\s][^-\s]+\n"
        self.rv.append(Token('line', s.strip()))

class Parser(GenericParser):
    def __init__(self, start='title'):
        GenericParser.__init__(self, start)

    def p_title(self, args):
        ' title ::= line '
        return args[0]

    """def p_doc(self, args):
        ' doc ::= title'
        return Node(type="doc", title=args[0])
        
        
    def p_lists(self, args):
        ''' attrs ::= attrs attr 
            sects ::= sects sect
            paras ::= paras para
        '''

    def p_attr(self, args):
        ' attr ::= docattr '
        
    def p_sect(self, args):
        ''' sect ::= secnumber emptyline paras
            sect ::= secnumber line emptyline paras
        '''

    def p_para(self, args):
        ''' para ::= para line
            para ::= para emptyline
        '''
    """
