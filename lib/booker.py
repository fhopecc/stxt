# coding=utf8
from __future__ import with_statement
import sys, os, re, lex3
from stxt_tree import DocTreeNode
import yacc, stxt_tb_parser
from booker_lexer import *

def p_doc(p):
    '''doc : sect1s
           | contents 
    '''
    doc = DocTreeNode('doc')

    for c in p[1]: doc.append(c)

    p[0] = doc

def p_sects(p):
    '''sect1s : sect1
              | sect1s sect1
       sect2s : sect2
              | sect2s sect2 
       sect3s : sect3
              | sect3s sect3 
       sect4s : sect4
              | sect4s sect4
       sect5s : sect5
              | sect5s sect5
    '''
    if len(p) == 2: p[1] = [p[1]]
    else: p[1].append(p[2])
    p[0] = p[1]

def p_sect(p):
    '''sect1 : H1
             | H1 contents
             | H1 sect2s
       sect2 : H2
             | H2 contents
             | H2 sect3s
       sect3 : H3
             | H3 contents
             | H3 sect4s
       sect4 : H4
             | H4 contents
             | H4 sect5s
       sect5 : H5
             | H5 contents
                          '''
    if len(p) == 3:
        for s in p[2]:
            p[1].append(s)
    p[0] = p[1]

def p_sect_with_contents(p):
    '''sect1 : H1 contents sect2s
       sect2 : H2 contents sect3s
       sect3 : H3 contents sect4s
       sect4 : H4 contents sect5s
    '''
    for s in p[2]:
        p[1].append(s)
    for s in p[3]:
        p[1].append(s)
    p[0] = p[1]
 
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
               | footnotes
               | para EMPTYLINE
    '''
    p[0] = p[1]

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

def p_footnotes(p):
    '''footnotes : FOOTNOTE
                 | footnotes FOOTNOTE
    '''
    if len(p) == 2:
        p[0] = DocTreeNode('footnotes')
        p[0].append(p[1])
    else: p[0] = p[1].append(p[2])

def p_error(p):
    print p
    sys.exit(0)

HEADER_PATTERN = r'^(\[(?P<n>[^]]+)\])?(?P<h>.+)'

parser = yacc.yacc()

def parse(source, lexer=lexer):
    # TABLE parsing will failed in yacc debug mode    
    #return parser.parse(source, lexer=lexer, debug=1)
   return parser.parse(source, lexer=lexer)

def usage():
    usage = os.path.basename(__file__) + " filename\n"
    usage += u'filename: structed text file\n'
    usage += u'dump the doctree'
    return usage

if __name__ == '__main__':
    try:
        with open(sys.argv[1]) as f:
            d = parse(f.read())
            d.dump_type_tree()
    except IndexError:
        print usage()
