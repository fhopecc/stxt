# coding=utf8
from __future__ import with_statement
import sys, os, re, lex, yacc, tabler, inliner
from tree import Tree
from lexer import *

DEBUG = False

def p_doc(p):
    '''doc : sect1s
           | sect2s 
           | sect3s 
           | sect4s 
           | sect5s 
           | contents 
    '''
    doc = Tree('doc')
    for c in p[1]: doc.append(c)
    p[0] = doc

def p_make_element_list(p):
    '''sect1s   : sect1
       sect2s   : sect2
       sect3s   : sect3
       sect4s   : sect4
       sect5s   : sect5
       contents : content
    '''
    p[0] = [p[1]]

def p_element_list(p):
    '''sect1s    : sect1s sect1
       sect2s    : sect2s sect2 
       sect3s    : sect3s sect3 
       sect4s    : sect4s sect4
       sect5s    : sect5s sect5
       contents  : contents content
       list      : list listitem
       footnotes : footnotes FOOTNOTE
    '''
    p[1].append(p[2])
    p[0] = p[1]

def p_sect(p):
    '''sect1 : H1 contents
             | H1 sect2s
       sect2 : H2 contents
             | H2 sect3s
       sect3 : H3 contents
             | H3 sect4s
       sect4 : H4 contents
             | H4 sect5s
       sect5 : H5 contents
    '''
    for s in p[2]:
        p[1].append(s)
    p[0] = p[1]

def p_sect_with_timestamp(p):
    '''sect1 : H1 TIMESTAMP contents
             | H1 TIMESTAMP sect2s
       sect2 : H2 TIMESTAMP contents
             | H2 TIMESTAMP sect3s
       sect3 : H3 TIMESTAMP contents
             | H3 TIMESTAMP sect4s
       sect4 : H4 TIMESTAMP contents
             | H4 TIMESTAMP sect5s
       sect5 : H5 TIMESTAMP contents
    '''
    for c in p[3]:
        p[1].append(c)
    p[1].timestamp = p[2]
    p[0] = p[1]

def p_sect_with_contents(p):
    '''sect1 : H1 contents sect2s
       sect2 : H2 contents sect3s
       sect3 : H3 contents sect4s
       sect4 : H4 contents sect5s
    '''
    for c in p[2]: p[1].append(c)
    for s in p[3]: p[1].append(s)
    p[0] = p[1]

def p_sect_with_contents_and_timestamp(p):
    '''sect1 : H1 TIMESTAMP contents sect2s
       sect2 : H2 TIMESTAMP contents sect3s
       sect3 : H3 TIMESTAMP contents sect4s
       sect4 : H4 TIMESTAMP contents sect5s
    '''
    for c in p[4]: p[1].append(c)
    for s in p[5]: p[1].append(s)
    p[1].timestamp = p[2]
    p[0] = p[1]
 
 
def p_token(p):
    '''sect1   : H1
       sect2   : H2
       sect3   : H3
       sect4   : H4
       sect5   : H5
       content : para
               | list
               | theorem
               | define
               | question
               | table
               | footnotes
               | footnotes EMPTYLINE
               | term
               | IMAGE
               | COMMENT
               | code
               | code EMPTYLINE
               | table EMPTYLINE
               | para EMPTYLINE
               | IMAGE EMPTYLINE
               | INSERT EMPTYLINE
    '''
    if p[1].type == 'para':
        value = p[1].value  
        start, end = p.linespan(1)
        slineno = p.lexer.active_lexer().startlineno + start - 1
        try:
            p[1] = inliner.parse(p[1].value, 
                                 file = p.lexer.active_file(), 
                                 lineno = slineno)
            p[1].value = value
        except SyntaxError, e:
            print '%s at %s:%s' % (e.message, e.filename, e.lineno)
            exit()
    p[0] = p[1]

def p_element_with_subdoc(p):
    '''define    : DEFINE subdoc
       theorem   : THEOREM subdoc
       question  : QUESTION subdoc
    '''
    p[0] = p[1]
    for c in p[2]:
        p[0].append(c)

def p_q_and_a_form(p):
    '''theorem : theorem PROOF subdoc
       question : question ANSWER subdoc
    '''
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
    if DEBUG == 0:
        table = tabler.parse(p[2].decode('utf8'))
        table.title = p[1].title
        table.name = p[1].name
        p[0] = table
    else:
        p[0] = Tree('table', 'debug mode do not support table')

def p_para(p):
    '''para : LINE
            | para LINE
    '''
    if len(p) == 2: p[1] = Tree('para', p[1])
    else: p[1].value += p[2]
    p[0] = p[1]

def p_make_footnotes(p):
    'footnotes : FOOTNOTE'
    p[0] = Tree('footnotes')
    p[0].append(p[1])

def p_term(p):
    'term : LINE subdoc'
    p[0] = Tree('term', p[1])
    p[0].title = p[0].value
    for c in p[2]:
        p[0].append(c)

def p_make_list(p):
    'list : listitem'
    p[0] = Tree(p[1].type.replace('item', ''))
    p[0].append(p[1])

def p_listitem(p):
    '''listitem : listhead
                | listitem subdoc
    '''
    if len(p) == 3: 
       for i, c in enumerate(p[2]): 
           if not p[1].is_onelinepara and i == 0 and c.type == 'para': 
                para = p[1].children[0]
                para.value +=  c.value
                src = para.value
                para = inliner.parse(src)
                para.value = src
                p[1].children[0] = para
           else: p[1].append(c)
    p[0] = p[1]

def p_listhead(p):
    '''listhead : LI
                | OL
                | listhead EMPTYLINE
    '''
    onelinelable_pattern = r'.*[:|：]$'
    m = re.match(onelinelable_pattern, p[1].value)
    p[1].is_onelinepara = (len(p) == 3 or m is not None)
    p[0] = p[1]

def p_subdoc(p):
    'subdoc : indent_block'
    file = p[1][0]
    lexdata = p[1][1]
    active_lexer = p.lexer.active_lexer()
    start, end = p.linespan(1)
    slineno = active_lexer.startlineno + start - 1
    indent = active_lexer.indent + 1
    p[0] = parse(lexdata, MutipleFileLexer(file, slineno, indent))

    if not p[0]: raise SyntaxError('subdoc error')
    elif not p[0].children: raise SyntaxError('subdoc error')
    else: p[0] = p[0].children

def p_indent_block(p):
    '''indent_block : INDENT
                    | indent_block EMPTYLINE
                    | indent_block INDENT
    '''
    if len(p) == 3:
        p[1][1] += p[2] + '\n'
    else: 
        p[1] += '\n'
        file = p.lexer.mflexer.active_file()
        p[1] = [file, p[1]]
    p[0] = p[1]

def p_error(p):
    active_lexer = p.lexer.mflexer.active_lexer()

    lineno = active_lexer.startlineno + active_lexer.lineno
    col = find_column(active_lexer.lexdata, p) + active_lexer.indent * 2
    print '%s at %s:%s:%s' % (p.type, active_lexer.file, lineno, col)
#print active_lexer.lexdata
    sys.exit(0)

HEADER_PATTERN = r'^(\[(?P<n>[^]]+)\])?(?P<h>.+)'

parser = yacc.yacc()
def parse(source, lexer=lexer):
    # TABLE parsing will failed in yacc debug mode    
    return parser.parse(source, lexer=lexer, tracking=True, debug=DEBUG)

def read(file):
    with open(file) as f:
        tree = parse(f.read(), lexer = MutipleFileLexer(file))
        tree.file = file
        return tree

def usage():
    usage = os.path.basename(__file__) + " filename\n"
    usage += u'filename: structed text file\n'
    usage += u'dump the doctree'
    return usage

if __name__ == '__main__':
    try:
        fn = sys.argv[1]
        with open(fn) as f:
            d = parse(f.read(), lexer = MutipleFileLexer(fn))
            d.dump_type_tree()
    except IndexError:
       console.info(usage()) 
