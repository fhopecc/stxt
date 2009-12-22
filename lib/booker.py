# coding=utf8
from __future__ import with_statement
import sys, os, re, lex3
from stxt_tree import DocTreeNode
import yacc
from booker_lexer import *

# Parser
def p_doc(p):
    '''doc : para
           | doc para
    '''
    if len(p) == 2:
        p[0] = DocTreeNode('doc', '')
        p[0].append(p[1])
    else:
        p[1].append(p[2])
        p[0] = p[1]

def p_para(p):
    '''para : LINE
            | para LINE
    '''
    if len(p) == 2:
        p[0] = p[1]
        p[0].type = 'para'
        p[0].level = p[1].level
    else:
        if p[1].level != p[2].level:
            raise IndentationError
        p[1].value += p[2].value
        p[0] = p[1]

def p_theorem(p):
    r'theorem : THEOREM contents'
    p[0] = p[1]
    for c in p[2]:
        p[0].append(c)

parser = yacc.yacc()

def parse(source):
    return parser.parse(source, lexer=lexer)
