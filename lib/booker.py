# coding=utf8
from __future__ import with_statement
import sys, os, re, lex3
from stxt_tree import DocTreeNode
import yacc, stxt_tb_parser
from booker_lexer import *

def p_doc(p):
    '''doc : sect1
           | content 
           | doc sect1
           | doc content
    '''
    if len(p) == 2:
        p[0] = DocTreeNode('doc', '')
        p[0].append(p[1])
    else:
        p[1].append(p[2])
        p[0] = p[1]

def p_sect1(p):
    '''sect1 : h1
             | sect1 content
             | sect1 sect2
    '''
    if len(p) == 3:
        p[1].append(p[2])
    p[0] = p[1]

def p_h1(p):
    r'h1 : para H1SEP'
    m = re.match(HEADER_PATTERN, p[1].value)
    sect = DocTreeNode('sect1', m.group('h'))
    sect.name = m.group('n')
    if len(sect.value) < 1:
        sect.value = sect.name
    p[0] = sect

def p_sect2(p):
    '''sect2 : h2
             | sect2 content
             | sect2 sect3
    '''
    if len(p) == 3:
        p[1].append(p[2])
    p[0] = p[1]

def p_h2(p):
    r'h2 : para H2SEP'
    m = re.match(HEADER_PATTERN, p[1].value)
    sect = DocTreeNode('sect2', m.group('h'))
    sect.name = m.group('n')
    if len(sect.value) < 1:
        sect.value = sect.name
    p[0] = sect

def p_sect3(p):
    '''sect3 : h3
             | sect3 content
             | sect3 sect4
    '''
    if len(p) == 3:
        p[1].append(p[2])
    p[0] = p[1]

def p_h3(p):
    r'h3 : para H3SEP'
    m = re.match(HEADER_PATTERN, p[1].value)
    sect = DocTreeNode('sect3', m.group('h'))
    sect.name = m.group('n')
    if len(sect.value) < 1:
        sect.value = sect.name
    p[0] = sect

def p_sect4(p):
    '''sect4 : h4
             | sect4 content
             | sect4 sect5
    '''
    if len(p) == 3:
        p[1].append(p[2])
    p[0] = p[1]

def p_h4(p):
    r'h4 : para H4SEP'
    m = re.match(HEADER_PATTERN, p[1].value)
    sect = DocTreeNode('sect4', m.group('h'))
    sect.name = m.group('n')
    if len(sect.value) < 1:
        sect.value = sect.name
    p[0] = sect

def p_sect5(p):
    '''sect5 : h5
             | sect5 content
    '''
    if len(p) == 3:
        p[1].append(p[2])
    p[0] = p[1]

def p_h5(p):
    r'h5 : para H5SEP'
    m = re.match(HEADER_PATTERN, p[1].value)
    sect = DocTreeNode('sect5', m.group('h'))
    sect.name = m.group('n')
    if len(sect.value) < 1:
        sect.value = sect.name
    p[0] = sect

def p_contents(p):
    '''contents : content
                | contents content'''
    if len(p) == 2: p[1] = [p[1]]
    else: p[1].append(p[2])
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
        p[0] = DocTreeNode(p[1].type.replace('item', ''))
        p[0].append(p[1])
    else: p[0] = p[1].append(p[2])

def p_listitem(p):
    '''listitem : listhead
                | listitem l1contents
    '''
    if len(p) == 3: 
       for i, c in enumerate(p[2]): 
           if not p[1].is_onelinelabel and i == 0 and c.type == 'para': 
                p[1].value += c.value
           else: p[1].append(c)
    p[0] = p[1]

def p_listhead(p):
    '''listhead : LI
                | OL
                | listhead EMPTYLINE
    '''
    p[1].is_onelinelabel = len(p) == 3
    p[0] = p[1]

def p_l1contents(p):
    '''l1contents : l1content
                  | l1contents l1content
                  | l1contents EMPTYLINE l1content
    '''
    if len(p) == 4: p[2] = p[3]
    if len(p) == 2: p[1] = [p[1]]
    else: p[1].append(p[2])
    p[0] = p[1]

def p_l1content(p):
    '''l1content : l1para
                 | l1list
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
        p[0] = DocTreeNode(p[1].type.replace('item', ''))
        p[0].append(p[1])
    else: p[0] = p[1].append(p[2])

def p_l1listitem(p):
    '''l1listitem : l1listhead
                  | l1listitem l2contents
    '''
    if len(p) == 3: 
       for i, c in enumerate(p[2]): 
           if not p[1].is_onelinelabel and i == 0 and c.type == 'para': 
                p[1].value += c.value
           else: p[1].append(c)
    p[0] = p[1]

def p_l1listhead(p):
    '''l1listhead : L1LI
                  | L1OL
                  | l1listhead EMPTYLINE
    '''
    p[1].is_onelinelabel = len(p) == 3
    p[0] = p[1]

def p_l2contents(p):
    '''l2contents : l2content
                  | l2contents l2content
                  | l2contents EMPTYLINE l2content
    '''
    if len(p) == 4: p[2] = p[3]
    if len(p) == 2: p[1] = [p[1]]
    else: p[1].append(p[2])
    p[0] = p[1]

def p_l2content(p):
    '''l2content : l2para
                 | l2list
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

def p_l2list(p):
    '''l2list : l2listitem
              | l2list l2listitem
    '''
    if len(p) == 2:
        p[0] = DocTreeNode(p[1].type.replace('item', ''))
        p[0].append(p[1])
    else: p[0] = p[1].append(p[2])

def p_l2listitem(p):
    '''l2listitem : l2listhead
                  | l2listitem l3contents
    '''
    if len(p) == 3: 
       for i, c in enumerate(p[2]): 
           if not p[1].is_onelinelabel and i == 0 and c.type == 'para': 
                p[1].value += c.value
           else: p[1].append(c)
    p[0] = p[1]

def p_l2listhead(p):
    '''l2listhead : L2LI
                  | L2OL
                  | l2listhead EMPTYLINE
    '''
    p[1].is_onelinelabel = len(p) == 3
    p[0] = p[1]

def p_l3contents(p):
    '''l3contents : l3content
                  | l3contents l3content
                  | l3contents EMPTYLINE l3content
    '''
    if len(p) == 4: p[2] = p[3]
    if len(p) == 2: p[1] = [p[1]]
    else: p[1].append(p[2])
    p[0] = p[1]

def p_l3content(p):
    '''l3content : l3para
                 | l3list
    '''
    p[0] = p[1]

def p_l3para(p):
    '''l3para : L3LINE
              | l3para L3LINE
    '''
    if len(p) == 2:
        p[0] = DocTreeNode('para', p[1])
    else:
        p[1].value += p[2]
        p[0] = p[1]

def p_l3list(p):
    '''l3list : l3listitem
              | l3list l3listitem
    '''
    if len(p) == 2:
        p[0] = DocTreeNode(p[1].type.replace('item', ''))
        p[0].append(p[1])
    else: p[0] = p[1].append(p[2])

def p_l3listitem(p):
    '''l3listitem : l3listhead
                  | l3listitem l4contents
    '''
    if len(p) == 3: 
       for i, c in enumerate(p[2]): 
           if not p[1].is_onelinelabel and i == 0 and c.type == 'para': 
                p[1].value += c.value
           else: p[1].append(c)
    p[0] = p[1]

def p_l3listhead(p):
    '''l3listhead : L3LI
                  | L3OL
                  | l3listhead EMPTYLINE
    '''
    p[1].is_onelinelabel = len(p) == 3
    p[0] = p[1]

def p_l4contents(p):
    '''l4contents : l4content
                  | l4contents l4content
                  | l4contents EMPTYLINE l4content
    '''
    if len(p) == 4: p[2] = p[3]
    if len(p) == 2: p[1] = [p[1]]
    else: p[1].append(p[2])
    p[0] = p[1]

def p_l4content(p):
    '''l4content : l4para
                 | l4list
    '''
    p[0] = p[1]

def p_l4para(p):
    '''l4para : L4LINE
              | l4para L4LINE
    '''
    if len(p) == 2:
        p[0] = DocTreeNode('para', p[1])
    else:
        p[1].value += p[2]
        p[0] = p[1]

def p_l4list(p):
    '''l4list : l4listitem
              | l4list l4listitem
    '''
    if len(p) == 2:
        p[0] = DocTreeNode(p[1].type.replace('item', ''))
        p[0].append(p[1])
    else: p[0] = p[1].append(p[2])

def p_l4listitem(p):
    '''l4listitem : l4listhead
                  | l4listitem l5contents
    '''
    if len(p) == 3: 
       for i, c in enumerate(p[2]): 
           if not p[1].is_onelinelabel and i == 0 and c.type == 'para': 
                p[1].value += c.value
           else: p[1].append(c)
    p[0] = p[1]

def p_l4listhead(p):
    '''l4listhead : L4LI
                  | L4OL
                  | l4listhead EMPTYLINE
    '''
    p[1].is_onelinelabel = len(p) == 3
    p[0] = p[1]

def p_l5contents(p):
    '''l5contents : l5content
                  | l5contents l5content
                  | l5contents EMPTYLINE l5content
    '''
    if len(p) == 4: p[2] = p[3]
    if len(p) == 2: p[1] = [p[1]]
    else: p[1].append(p[2])
    p[0] = p[1]

def p_l5content(p):
    '''l5content : l5para
                 | l5list
    '''
    p[0] = p[1]

def p_l5para(p):
    '''l5para : L5LINE
              | l5para L5LINE
    '''
    if len(p) == 2:
        p[0] = DocTreeNode('para', p[1])
    else:
        p[1].value += p[2]
        p[0] = p[1]

def p_l5list(p):
    '''l5list : l5listitem
              | l5list l5listitem
    '''
    if len(p) == 2:
        p[0] = DocTreeNode(p[1].type.replace('item', ''))
        p[0].append(p[1])
    else: p[0] = p[1].append(p[2])

def p_l5listitem(p):
    '''l5listitem : l5listhead
                  | l5listitem l6contents
    '''
    if len(p) == 3: 
       for i, c in enumerate(p[2]): 
           if not p[1].is_onelinelabel and i == 0 and c.type == 'para': 
                p[1].value += c.value
           else: p[1].append(c)
    p[0] = p[1]

def p_l5listhead(p):
    '''l5listhead : L5LI
                  | L5OL
                  | l5listhead EMPTYLINE
    '''
    p[1].is_onelinelabel = len(p) == 3
    p[0] = p[1]

def p_l6contents(p):
    '''l6contents : l6content
                  | l6contents l6content
                  | l6contents EMPTYLINE l6content
    '''
    if len(p) == 4: p[2] = p[3]
    if len(p) == 2: p[1] = [p[1]]
    else: p[1].append(p[2])
    p[0] = p[1]

def p_l6content(p):
    'l6content : l6para'
    p[0] = p[1]

def p_l6para(p):
    '''l6para : L6LINE
              | l6para L6LINE
    '''
    if len(p) == 2:
        p[0] = DocTreeNode('para', p[1])
    else:
        p[1].value += p[2]
        p[0] = p[1]

HEADER_PATTERN = r'^(\[(?P<n>[^]]+)\])?(?P<h>.+)'

parser = yacc.yacc()

def parse(source):
    # TABLE parsing will failed in yacc debug mode    
   return parser.parse(source, lexer=lexer, debug=1)
#    return parser.parse(source, lexer=lexer)
