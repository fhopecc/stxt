# coding=utf8
import sys, lex, yacc
from stxt_tree import DocTreeNode
# Lexer
tokens = [
          'CBLOCK', 
          'EMPHASIS', 
          'REFERENCE'
         ] 

def find_column(input,token):
    last_cr = input.rfind('\n',0,token.lexpos)
    if last_cr < 0:
	last_cr = 0
    column = (token.lexpos - last_cr) + 1
    return column

def t_REFERENCE(t):
    r'\[(?P<ref>[^\]]*)]'           
    c = t.lexer.lexmatch.group('ref')
    t.value = DocTreeNode('reference', c)
    return t

def t_EMPHASIS(t):
    r'\*(?P<em>[^* ]*)\*'           
    c = t.lexer.lexmatch.group('em')
    t.value = DocTreeNode('reference', c)
    return t

def t_CBLOCK(t):
    r'[^*[\n]+'           
    return t

def t_NEWLINE(t):
    r'\n'
    t.lexer.lineno += t.lexeme.count('\n')

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
        p[0] = DocTreeNode('para', '')
        p[0].append(p[1])
    else:
        p[0] = p[1].append(p[2])

def p_elem(p):
    '''elem : cblock
            | EMPHASIS
            | REFERENCE
    '''
    p[0] = p[1]

def p_cblock(p):
    '''cblock : CBLOCK
              | cblock CBLOCK
    '''
    if len(p) == 2:
        p[0] = DocTreeNode('cblock', p[1])
    else:
        p[1].value += p[2]
        p[0] = p[1]

def p_error(t):
    print 'parse error:' + str(t)
    raise SyntaxError('parse error:' + str(t))

parser = yacc.yacc()

def parse(input):
    d = parser.parse(input, lexer=lexer)
    return d
