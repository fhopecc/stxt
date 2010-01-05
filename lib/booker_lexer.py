# coding=utf8
from __future__ import with_statement
import sys, os, re, lex3
from stxt_tree import DocTreeNode

states = (
    ('code', 'exclusive'), 
    ('table', 'exclusive')
)

tokens = [
          'H1SEP', 'H2SEP', 'H3SEP', 'H4SEP', 'H5SEP', 
          'DEFINE', 'THEOREM', 'PROOF', 
          'QUESTION', 'ANSWER', 
          'IMAGE',
          'FOOTNOTE', 'CITATION', 
          'CODE', 'CODEBLOCK',
          'TABLE', 'TABLEBLOCK',
          'INSERT', 
          'LINE', 'INDENT', 
          'LI', 'OL', 
          'EMPTYLINE', 
         ] 

t_H1SEP = r'^=+$'
t_H2SEP = r'^-+$'
t_H3SEP = r'^~+$'
t_H4SEP = r'^\*+$'
t_H5SEP = r'^\^+$'

def t_include(t):
    r'^<(?P<file>.*)>'
    t.lexer.include_lexer = t.lexer.clone()
    column = find_column(t.lexer.lexdata, t)
    file = t.lexer.lexmatch.group('file')
    try:
        with open(file) as f:
            t.lexer.include_lexer.input(f.read())
            t.lexer.include_lexer.file = file
            t.lexer.include_lexer.lineno = 1
    except IOError:
        raise IOError("(%s:%i:%i): include file %s doesn't exist" % \
               (t.lexer.file, t.lexer.lineno, column, file))
    return t.lexer.include_lexer.token()

def t_CODE(t):
    r'^code(\[(?P<n>[^]]*)\])?\.(?P<title>.*)\n'
    m = t.lexer.lexmatch
    t.value = DocTreeNode('code') 
    t.value.name = m.group('n')
    t.value.title = m.group('title')
    if len(t.value.title) < 1:
        t.value.title = t.value.name
    t.lexer.lineno += 1
    t.lexer.block_start = t.lexer.lexpos
    t.lexer.block_lineno = t.lexer.lineno
    t.lexer.begin('code')
    return t

def t_code_CODEBLOCK(t):
    r'\n::\s*$'
    block_end = t.lexer.lexpos - len(t.lexer.lexmatch.group(0))
    t.value = t.lexer.lexdata[t.lexer.block_start:block_end]
    t.lineno = t.lexer.block_lineno
    t.lexer.lineno += 1
    t.lexer.begin('INITIAL')
    return t

def t_code_pass(t):
    r'[^\n]+'
    pass 
 
def t_TABLE(t):
    r'^table(\[(?P<n>[^]]*)\])?\.(?P<title>.*)\n'
    m = t.lexer.lexmatch
    t.value = DocTreeNode('table') 
    t.value.name = m.group('n')
    t.value.title = m.group('title')
    if len(t.value.title) < 1:
        t.value.title = t.value.name
    t.lexer.lineno += 1
    t.lexer.block_start = t.lexer.lexpos
    t.lexer.block_lineno = t.lexer.lineno
    t.lexer.read_head_sep = False
    t.lexer.begin('table')
    return t

def t_table_TABLEBLOCK(t):
    r'=[= ]*'
    if t.lexer.read_head_sep:
        m = t.lexer.lexmatch
        block_end = t.lexer.lexpos
        t.value = t.lexer.lexdata[t.lexer.block_start:block_end]
        t.lineno = t.lexer.block_lineno 
        t.lexer.begin('INITIAL') 
        return t
    else:
        t.lexer.read_head_sep = True

def t_table_pass(t):
    r'[^\n]+'
    pass 

def t_INSERT(t):
    r'^(?P<t>table|image)\[(?P<n>[^]]*)\]$'
    t.value = DocTreeNode('insert') 
    t.node_type = t.lexer.lexmatch.group('t')
    t.name = t.lexer.lexmatch.group('n')
    return t

def t_LI(t):
    r'^\*[ ](?P<c>.*)'
    t.value = t.lexer.lexmatch.group('c')
    t.value = DocTreeNode('listitem', t.value)
    return t

def t_OL(t):
    r'^(?P<n>\#|\d+)\.(?P<c>.*)'
    n = t.lexer.lexmatch.group('n')
    t.value = t.lexer.lexmatch.group('c')
    t.value = DocTreeNode('olistitem', t.value)
    if n != '#':
        t.value.number = int(n)
    else:
        t.value.number = '#'
    return t

def t_HEAD(t):
    r'^(?P<h>\w+)(\[(?P<n>[^]]*)\])?\.(?P<title>.*)$'
    m = t.lexer.lexmatch
    t.type = m.group('h').upper()
    t.value = DocTreeNode(m.group('h')) 
    t.value.name = m.group('n')
    t.value.title = m.group('title')
    if len(t.value.title) < 1:
        t.value.title = t.value.name
    return t

def t_FOOTNOTE(t):
    r'^\.\.[ ]\[(?P<id>[\w#]+)][ ](?P<c>.+)$'
    m = t.lexer.lexmatch
    id = m.group('id')
    content = m.group('c')
    if id == '#':
        t.value = DocTreeNode('footnote', content)
    else:
        t.type = 'CITATION'
        t.value = DocTreeNode('citation', content)
    return t

def t_LINE(t):
    r'^(?P<l>[^ \n=\-~*^<#:].+)'
    t.value = t.lexer.lexmatch.group('l')
    return t

def t_EMPTYLINE(t):
    r'^[ ]*\n'
    t.lexer.lineno += t.value.count('\n')
    t.value = ''
    return t

def t_INDENT(t):
    r'^[ ][ ](?P<l>.+)$'
    t.value = t.lexer.lexmatch.group('l')
    return t

def t_ANY_newline(t):
    r'\n'
    t.lexer.lineno += t.value.count('\n')

def t_ANY_error(t):
    c = t.value[0]
    raise lex3.LexError('illegal char(%s) "%s" at %s:%s:%s\n%s' % 
                (str(ord(t.value[0])), c, 
                 t.lexer.file, t.lexer.lineno, 
                 find_column(t.lexer.lexdata, t), t.lexer.lexdata), 
                 t.lexer.lexdata)

def find_column(input,token):
    last_cr = input.rfind('\n',0,token.lexpos)
    if last_cr < 0:
	last_cr = 0
    column = (token.lexpos - last_cr) + 1
    return column

class MutipleFileLexer(object):
    def __init__(self):
        self.lexer = lex3.lex(reflags=re.M)
        self.lexer.include_lexer = None
        self.lexer.file = '__string__'

    def token(self):
        if self.lexer.include_lexer:
            t = self.lexer.include_lexer.token()
            if t: return t
        else:
            self.lexer.include_lexer = None
     
        return self.lexer.token()

    def begin(self, state):
        self.lexer.begin(state)

    def input(self, lexdata):
        self.lexer.include_lexer = None 
        self.lexer.input(lexdata)
        self.lexer.lineno = 1

    def read(self, f):
        self.lexer.file = f
        with open(f) as f:
           self.input(f.read())

lexer = MutipleFileLexer()

#if __name__ == '__main__':
#    case = 'theorem[name].this is a theorem title'
#    lexer.input(case)
#    tok = lexer.token()
#    print tok
