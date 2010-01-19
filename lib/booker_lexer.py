# coding=utf8
from __future__ import with_statement
import sys, os, re, lex3
from stxt_tree import Tree
from lex3 import TOKEN
from lex3 import LexError

states = (
    ('code', 'exclusive'), 
    ('table', 'exclusive')
)

tokens = [
          'H1', 'H2', 'H3', 'H4', 'H5', 
          'DEFINE', 'THEOREM', 'PROOF', 
          'QUESTION', 'ANSWER', 
          'IMAGE',
          'COMMENT', 'FOOTNOTE', 'CITATION', 
          'CODE', 'CODEBLOCK',
          'TABLE', 'TABLEBLOCK',
          'INSERT', 
          'LINE', 'INDENT', 
          'LI', 'OL', 
          'EMPTYLINE', 
         ] 

def t_include(t):
    r'^<(?P<file>.*)>$'
    column = find_column(t.lexer.lexdata, t)
    file = t.lexer.lexmatch.group('file')
    t.lexer.include_lexer = MutipleFileLexer(file)
    try:
        with open(file) as f:
            t.lexer.include_lexer.input(f.read())
    except IOError:
        raise IOError("(%s:%i:%i): include file %s doesn't exist" % \
               (t.lexer.file, t.lexer.lineno, column, file))
    return t.lexer.include_lexer.token()

def t_CODE(t):
    r'^code(\[(?P<n>[^]]*)\])?\.(?P<title>.*)\n'
    m = t.lexer.lexmatch
    t.value = Tree('code') 
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
    r'\n::[ ]*$'
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
    t.value = Tree('table') 
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
    t.value = Tree('insert') 
    t.node_type = t.lexer.lexmatch.group('t')
    t.name = t.lexer.lexmatch.group('n')
    return t

def t_LI(t):
    r'^\*[ ](?P<c>.*)'
    content = t.lexer.lexmatch.group('c')
    t.value = Tree('listitem', content)
    return t

def t_OL(t):
    r'^(?P<n>\#|\d+)\.(?P<c>.*)'
    n = t.lexer.lexmatch.group('n')
    content = t.lexer.lexmatch.group('c')
    t.value = Tree('olistitem', content)
    if n != '#':
        t.value.number = int(n)
    else:
        t.value.number = '#'
    return t

head  = r'image|question|answer|define|theorem|'
head += r'proof'
head  = r'^(?P<h>(%s))' % head
head += r'(\[(?P<n>[^]]*)\])?\.(?P<title>.*)$'

@TOKEN(head)
def t_HEAD(t):
    m = t.lexer.lexmatch
    t.type = m.group('h').upper()
    t.value = Tree(m.group('h')) 
    t.value.name = m.group('n')
    t.value.title = m.group('title')
    if len(t.value.title) < 1:
        t.value.title = t.value.name
    return t

def t_COMMENT(t):
    r'^\.\.[ ](\[(?P<id>[\w#]+)][ ])?(?P<c>.+)$'
    m = t.lexer.lexmatch
    id = m.group('id')
    content = m.group('c')
    if not id:
        t.value = Tree('comment', content)
    elif id == '#':
        t.type = 'FOOTNOTE'
        t.value = Tree('footnote', content)
    else:
        t.type = 'CITATION'
        t.value = Tree('citation', content)
    return t

#sep =  r'^(\[(?P<name>.*)\])?(?P<title>.*)\n(=+|-+|~+|\*+|\^+)$' 

#@TOKEN(sep)
def t_H(t):
    r'^(\[(?P<name>.*)\])?(?P<title>.*)\n(=+|-+|~+|\*+|\^+)$' 
    m = t.lexer.lexmatch
    t.lexer.lineno += m.group(0).count('\n')
     
    sep = m.group(0)[-1]
    level = 1
    if sep == '-': level = 2
    elif sep == '~': level = 3
    elif sep == '*': level = 4
    elif sep == '^': level = 5
    title = m.group('title')
    t.type = 'H%s' % level
    t.value = Tree('sect%s' % level, title) 
    t.value.level = level
    t.value.name = m.group('name')
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
    active_lexer = t.lexer.mflexer.active_lexer()

    lineno = active_lexer.startlineno + active_lexer.lineno
    
    col = find_column(active_lexer.lexdata, t) + active_lexer.indent * 2
    raise LexError('illegal char(%s) "%s" at %s:%s:%s' % 
                  (str(ord(t.value[0])), c, 
                   active_lexer.file, lineno, col), c)

def find_column(input,token):
    last_cr = input.rfind('\n',0,token.lexpos)
    if last_cr < 0:
	last_cr = 0
    column = (token.lexpos - last_cr) + 1
    return column

class MutipleFileLexer(object):
    def __init__(self, f = '__string__', startlineno = 0, indent = 0):
        self.lexer = lex3.lex(reflags=re.M)
        self.lexer.include_lexer = None
        self.lexer.file = f
        self.lexer.startlineno = startlineno
        self.lexer.indent = indent
        self.lexer.mflexer = self
        self.mflexer = self
    
    def active_lexer(self):
        lexer = self.lexer
        while lexer.include_lexer:
            lexer = lexer.include_lexer.lexer
        return lexer

    def active_file(self):
        return self.active_lexer().file

    def token(self):
        if self.lexer.include_lexer:
            t = self.lexer.include_lexer.token()
            if t: return t
            else: self.lexer.include_lexer = None
        return self.lexer.token()

    def begin(self, state):
        self.lexer.begin(state)

    def input(self, lexdata):
        self.lexer.include_lexer = None 
        self.lexer.input(lexdata)
        self.lexer.lineno = 1

lexer = MutipleFileLexer()

if __name__ == '__main__':
    file = sys.argv[1]
    with open(file) as f:
        lexer.input(f.read())
    t = lexer.token()
    while t:
        print "%i:%s" % (t.lexer.lineno , t.type)
        t = lexer.token()
