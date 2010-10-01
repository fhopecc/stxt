# coding=utf-8
from spark import *

class Token(object):
    def __init__(self, type, value):
        self.type = type
        self.value = value

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
    def __init__(self, start='doc'):
        GenericParser.__init__(self, start)
                
    def p_expr_1(self, args):
        ' expr ::= expr + term '
        return AST(type=args[1],
                   left=args[0],
                   right=args[2])
        
    def p_expr_2(self, args):
        ' expr ::= term '
        return args[0]
        
    def p_term_1(self, args):
        ' term ::= term * factor '
        return AST(type=args[1],
                   left=args[0],
                   right=args[2])
        
    def p_term_2(self, args):
        ' term ::= factor '
        return args[0]
        
    def p_factor_1(self, args):
        ' factor ::= number '
        return AST(type=args[0])

    def p_factor_2(self, args):
        ' factor ::= float '
        return AST(type=args[0])
