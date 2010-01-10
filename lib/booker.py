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

def p_sect(p):
    '''sect1 : h1 content1s
       sect2 : h2 content2s
       sect3 : h3 content3s
    '''
    if len(p) == 3:
        for s in p[2]:
            p[1].append(s)
    p[0] = p[1]

def p_h(p):
    'h1 : para H1SEP'
    'h2 : para H2SEP'
    'h3 : para H3SEP'
    m = re.match(HEADER_PATTERN, p[1].value)

    s = p[2][0]
    if s == '=':
        sect = DocTreeNode('sect1', m.group('h'))
    elif s == '-':
        sect = DocTreeNode('sect2', m.group('h'))
    elif s == '~':
        sect = DocTreeNode('sect3', m.group('h'))

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
    '''content1 : sect2
       content2 : sect3
                | content
       content  : para
                | list
                | theorem
                | define
                | question
                | code
                | table
                | para EMPTYLINE
    '''
    p[0] = p[1]



"""def p_sect4s(p):
    '''sect4s : sect4
              | sect4s sect4'''
    if len(p) == 2: p[1] = [p[1]]
    else: p[1].append(p[2])
    p[0] = p[1]

def p_sect4(p):
    '''sect4 : h4
             | h4 sect5s'''
    if len(p) == 3:
        p[1].append(p[2])
    p[0] = p[1]

def p_h4(p):
    '''h4 : para H4SEP 
          | para H4SEP contents'''
    m = re.match(HEADER_PATTERN, p[1].value)
    sect = DocTreeNode('sect4', m.group('h'))
    sect.name = m.group('n')
    if len(sect.value) < 1:
        sect.value = sect.name
    if len(p) == 4:
        for c in p[3]:
            sect.append(c)
    p[0] = sect

def p_sect5s(p):
    '''sect5s : sect5
              | sect5s sect5'''
    if len(p) == 2: p[1] = [p[1]]
    else: p[1].append(p[2])
    p[0] = p[1]

def p_sect5(p):
    'sect5 : h5'
    p[0] = p[1]

def p_h5(p):
    '''h5 : para H5SEP 
          | para H5SEP contents'''
    m = re.match(HEADER_PATTERN, p[1].value)
    sect = DocTreeNode('sect5', m.group('h'))
    sect.name = m.group('n')
    if len(sect.value) < 1:
        sect.value = sect.name
    if len(p) == 4:
        for c in p[3]:
            sect.append(c)
    p[0] = sect
"""
def p_define(p):
    '''define : DEFINE subdoc
    '''
    p[0] = p[1]
    for c in p[2]:
        p[0].append(c)

def p_theorem(p):
    '''theorem : THEOREM subdoc
    '''
    p[0] = p[1]
    for c in p[2]:
        p[0].append(c)

def p_theorem_with_proof(p):
    '''theorem : theorem PROOF subdoc'''
    p[0] = p[1]
    for c in p[3]:
        p[2].append(c)
    p[0].append(p[2])

def p_question(p):
    '''question : QUESTION subdoc
    '''
    p[0] = p[1]
    for c in p[2]:
        p[0].append(c)

def p_question_with_answer(p):
    '''question : question ANSWER subdoc'''
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
                | listitem subdoc
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

def p_subdoc(p):
    'subdoc : indent_block'
    p[0] = parse(p[1], lexer= MutipleFileLexer()).children

def p_indent_block(p):
    '''indent_block : INDENT
                    | indent_block EMPTYLINE
                    | indent_block INDENT
    '''
    if len(p) == 3:
       p[1] += p[2] + '\n'
    else: p[1] += '\n'
    p[0] = p[1]

def p_error(p):
    print p
    sys.exit(0)

HEADER_PATTERN = r'^(\[(?P<n>[^]]+)\])?(?P<h>.+)'

parser = yacc.yacc()

def parse(source, lexer=lexer):
    # TABLE parsing will failed in yacc debug mode    
   return parser.parse(source, lexer=lexer, debug=1)
   return parser.parse(source, lexer=lexer)

def usage():
    usage = os.path.basename(__file__) + " filename\n"
    usage += 'filename: structed text file\n'
    usage += 'dump the doctree'
    return usage

if __name__ == '__main__':
    try:
        with open(sys.argv[1]) as f:
            d = parse(f.read())
            d.dump_type_tree()
    except IndexError:
        print usage()
