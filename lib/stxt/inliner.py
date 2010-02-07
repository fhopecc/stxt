# coding=utf8
import sys, lex, yacc
from tree import Tree
# Lexer
tokens = [
          'CBLOCK', 
          'STAR', 
          'LSQUARE', 
          'RSQUARE', 
          'BACKSLASH' 
         ] 

def find_column(input,token):
    last_cr = input.rfind('\n',0,token.lexpos)
    if last_cr < 0:
	last_cr = 0
    column = (token.lexpos - last_cr) + 1
    return column

def t_BACKSLASH(t):
    r'\\'
    return t

def t_STAR(t):
    r'\*'
    return t

def t_LSQUARE(t):
    r'\['
    return t

def t_RSQUARE(t):
    r'\]'
    return t

def t_CBLOCK(t):
    r'[^*\[\]\n]+'           
    return t

def t_NEWLINE(t):
    r'\n'
    t.lexer.lineno += 1

def t_error(t):
    c = t.value[0]
    raise lex.LexError('illegal char[%s](%s) at %s:%s:%s\n%s' % (c, 
                str(ord(t.value[0])), t.lexer.file, t.lexer.lineno, 
                find_column(t.lexer.lexdata, t), t.lexer.lexdata), 
                t.lexer.lexdata)

lexer = lex.lex(reflags=0)

# PARSER
def p_para(p):
    '''para : elem
            | para elem
    ''' 
    if len(p) == 2:
        p[0] = Tree('para', '')
        p[0].append(p[1])
    else:
        p[0] = p[1].append(p[2])

def p_elem(p):
    '''elem : cblock
            | emphasis
            | reference
    '''
    p[0] = p[1]

def p_element(p):
    '''emphasis  : STAR cblock STAR
       reference : LSQUARE cblock RSQUARE
    '''
    if p[1] == '*':
        p[0] = Tree('emphasis', p[2].value)
    elif p[1] == '[':
        p[0] = Tree('reference', p[2].value)

def p_cblock(p):
    '''cblock : CBLOCK
              | unmagic_char
              | cblock CBLOCK
              | cblock unmagic_char
    '''
    if len(p) == 2:
        p[0] = Tree('cblock', p[1])
    else:
        p[1].value += p[2]
        p[0] = p[1]

def p_unmagic_char(p):
    '''unmagic_char : BACKSLASH STAR
                    | BACKSLASH LSQUARE
                    | BACKSLASH RSQUARE
    '''
    p[0] = p[2]

def p_error(t):
    print 'parse error:' + str(t)
    raise SyntaxError('parse error:' + str(t))

parser = yacc.yacc()

def parse(input, file='__string__', lineno=1):
    lexer.lineno = lineno
    lexer.file = file
    d = parser.parse(input, lexer=lexer, debug=True)
    return d
