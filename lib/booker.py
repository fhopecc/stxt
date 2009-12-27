# coding=utf8
from __future__ import with_statement
import sys, os, re, lex3
from stxt_tree import DocTreeNode
import yacc, stxt_tb_parser
from booker_lexer import *

def p_doc(p):
    '''doc : content 
           | doc content
    '''
    if len(p) == 2:
        p[0] = DocTreeNode('doc', '')
        p[0].append(p[1])
    else:
        p[1].append(p[2])
        p[0] = p[1]

def p_contents(p):
    '''contents : content
                | contents content'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[1].append(p[2])
        p[0] = p[1]

def p_content(p):
    '''content : para
               | list
               | theorem
               | define
               | question
               | code
               | table
               | para EMPTYLINE
    '''
    p[0] = p[1]

def p_para(p):
    '''para : LINE
            | para LINE
    '''
    if len(p) == 2:
        p[0] = DocTreeNode('para', p[1])
    else:
        p[1].value += p[2]
        p[0] = p[1]

def p_list(p):
    '''list : listitem
            | list listitem
    '''
    if len(p) == 2:
        if p[1].type == 'listitem':
            p[0] = DocTreeNode('list', '')
        elif p[1].type == 'olistitem':
            p[0] = DocTreeNode('olist', '')
        p[0].append(p[1])
    else:
        p[0] = p[1].append(p[2])

def p_listitem(p):
    '''listitem : listhead
                | listitem EMPTYLINE
                | listitem EMPTYLINE l1contents
    '''
    p[0] = p[1]
    if len(p) == 4 and p[3]:
        for c in p[3]:
            p[0].append(c)

def p_listhead(p):
    '''listhead : LI
                | OL
                | listhead L1LINE
    '''
    p[0] = p[1]
    if len(p) == 3 and p[2]:
        p[0].value += p[2]

def p_define(p):
    '''define : DEFINE l1contents
    '''
    p[0] = p[1]
    for c in p[2]:
        p[0].append(c)

def p_theorem(p):
    '''theorem : THEOREM l1contents
    '''
    p[0] = p[1]
    for c in p[2]:
        p[0].append(c)

def p_theorem_with_proof(p):
    '''theorem : theorem PROOF l1contents'''
    p[0] = p[1]
    for c in p[3]:
        p[2].append(c)
    p[0].append(p[2])

def p_question(p):
    '''question : QUESTION l1contents
    '''
    p[0] = p[1]
    for c in p[2]:
        p[0].append(c)

def p_question_with_answer(p):
    '''question : question ANSWER l1contents'''
    p[0] = p[1]
    for c in p[3]:
        p[2].append(c)
    p[0].append(p[2])

def p_code(p):
    '''code : CODE CODEBLOCK'''
    p[0] = p[1]
    p[0].value = p[2]

def p_table(p):
    '''table : TABLE TABLEBLOCK'''
    table = stxt_tb_parser.parse(p[2].decode('utf8'))
    table.title = p[1].title
    table.name = p[1].name
    p[0] = table

def p_l1contents(p):
    '''l1contents : l1content
                  | l1contents l1content'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[1].append(p[2])
        p[0] = p[1]

def p_l1content(p):
    '''l1content : l1para
                 | l1list
                 | l1para EMPTYLINE
    '''
    p[0] = p[1]

def p_l1para(p):
    '''l1para : L1LINE
              | l1para L1LINE
    '''
    if len(p) == 2:
        p[0] = DocTreeNode('para', p[1])
    else:
        p[1].value += p[2]
        p[0] = p[1]

def p_l1list(p):
    '''l1list : l1listitem
              | l1list l1listitem
    '''
    if len(p) == 2:
        if p[1].type == 'listitem':
            p[0] = DocTreeNode('list', '')
        elif p[1].type == 'olistitem':
            p[0] = DocTreeNode('olist', '')
        p[0].append(p[1])
    else:
        p[0] = p[1].append(p[2])

def p_l1listitem(p):
    '''l1listitem : l1listhead
                  | l1listitem EMPTYLINE
                  | l1listitem EMPTYLINE l2content
    '''
    p[0] = p[1]
    if len(p) == 4 and p[3]:
        p[0].append(p[3])

def p_l1listhead(p):
    '''l1listhead : L1LI
                  | L1OL
                  | l1listhead L2LINE
    '''
    p[0] = p[1]
    if len(p) == 3 and p[2]:
        p[0].value += p[2]

def p_l2content(p):
    '''l2content : l2para
    '''
    p[0] = p[1]

def p_l2para(p):
    '''l2para : L2LINE
              | l2para L2LINE
    '''
    if len(p) == 2:
        p[0] = DocTreeNode('para', p[1])
    else:
        p[1].value += p[2]
        p[0] = p[1]

parser = yacc.yacc()

def parse(source):
    # TABLE parsing will failed in yacc debug mode    
#   return parser.parse(source, lexer=lexer, debug=1)
    return parser.parse(source, lexer=lexer)
