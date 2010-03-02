# coding=utf8
import sys, lex, yacc
import logging
from logging import config
from tree import *

config.fileConfig(r'config\log.conf')
console = logging.getLogger()

DEBUG = False

# Lexer
tokens = [
          'EMPHASIS', 
          'EMAIL', 
          'REFERENCE', 
          'URL',
          'CBLOCK' 
         ] 

def find_column(input,token):
    last_cr = input.rfind('\n',0,token.lexpos)
    if last_cr < 0: last_cr = 0
    column = (token.lexpos - last_cr) + 1
    return column

def t_EMAIL(t):
    r'\b(?i[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,4})\b' 
    t.value = Tree('email', t.value))
    return t

def t_URL(t):
    r"\b(https?|ftp|gopher|telnet|file|notes|ms-help):((//)|(\\\\))[\w\d:#%/;$()~_?\\-=\\\.&]*\b"
    t.value = Tree('url', t.value))
    return t

def t_EMPHASIS(t):
    r'\*\*(?P<t>[^*]+)\*\*'
    m = t.lexer.lexmatch
    t.value = Tree('emphasis', m.group('t'))
    return t

refpat =  r'\[\['
refpat += r'(?P<1>[^:\]]+):(?P<2>[^:\]]+):(?P<3>[^:\]]+)|'
refpat += r'(?P<4>[^:\]]+):(?P<5>[^:\]]+)|'
refpat += r'(?P<5>[^:\]]+)'
refpat += r'\]\]'
TOKEN(refpat)
def t_REFERENCE(t):
    m = t.lexmatch
    if m:
        if m.group('1'): #[[label:name:type]]
            p[0] = ReferenceNode(m.group('2'), m.group('3'), m.group('1'))
        elif m.group('4'): #[[name]]
            p[0] = ReferenceNode(m.group('4'), m.group('5'))
        elif m.group('6'): #[[name:type]]
            p[0] = ReferenceNode(m.group('6'))
    else: 
        console.error("[%s]:It isn't correct address." % p[2].value)
        console.error("error at %s:%s" % (p.lexer.file, p.lexer.lineno))
        sys.exit()

    p[0].file   = p.lexer.file
    p[0].lineno = p.lexer.lineno
    return t

def t_CBLOCK(t):
    r'.+'           
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
            | single_star_error
    '''
    p[0] = p[1]

def p_element(p):
    '''emphasis  : STAR cblock STAR
       reference : LDSQUARE cblock RDSQUARE
    '''
    if p[1] == '*':
        p[0] = Tree('emphasis', p[2].value)
    elif p[1] == '[':

def p_single_star_error(p):
    '''single_star_error : STAR cblock'''
    console.info("inliner error report:")
    console.info("error at %s:%s" % (p.lexer.file, p.lexer.lineno))
    console.info("emphasis element:could not find ending '*'.")
    sys.exit()

def p_cblock(p):
    '''cblock : CBLOCK
              | escape_sequence
              | cblock CBLOCK
              | cblock escape_sequence
    '''
    if len(p) == 2:
        p[0] = Tree('cblock', p[1])
    else:
        p[1].value += p[2]
        p[0] = p[1]

def p_escape_sequence(p):
    '''escape_sequence : BACKSLASH STAR
                       | BACKSLASH LDSQUARE
                       | BACKSLASH RDSQUARE
                       | BACKSLASH BACKSLASH
                       | BACKSLASH CBLOCK
    '''
    if p[2] not in ('*', '[[', ']]', '\\'):
        console.info("inliner error report:")
        console.info("error at %s:%s" % (p.lexer.file, p.lexer.lineno))
        console.info("Wrong escape sequence!")
        sys.exit()

    p[0] = p[2]

def p_error(p):
    console.error("inliner error report:")
    if p:
        console.error("error at %s:%s" % (p.lexer.file, p.lexer.lineno))
        console.error("Input %s" % p.type)
    sys.exit()

parser = yacc.yacc()
def parse(input, file='__string__', lineno=1):
    lexer.lineno = lineno + 1
    lexer.file = file
    d = parser.parse(input, lexer=lexer, tracking=True, debug=DEBUG)
    return d
