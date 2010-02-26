# coding=utf8
from __future__ import with_statement
from tree import Tree
from lex import TOKEN
from lex import LexError
from logging import config
import sys, os, re, lex, logging

config.fileConfig(os.path.join(os.path.dirname(__file__),'log.conf'))
console = logging.getLogger('console')

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
          'LI', 'OL', 'TIMESTAMP', 
          'EMPTYLINE'
         ] 

def t_include(t):
    r'^<(?P<file>.*)>$'
    column = find_column(t.lexer.lexdata, t)
    fn = t.lexer.lexmatch.group('file')
    t.lexer.include_lexer = MutipleFileLexer(fn)
    try:
        with open(fn) as f:
            t.lexer.include_lexer.input(f.read())
    except IOError:
        raise IOError("(%s:%i:%i): include file %s doesn't exist" % \
               (t.lexer.file, t.lexer.lineno, column, fn))
    return t.lexer.include_lexer.token()

def t_TIMESTAMP(t):
    r'^(?P<y>\d{3})(?P<m>[01]\d)(?P<d>[0123]\d)\n\n'

    t.lexer.lineno += t.value.count('\n')

    m   = t.lexer.lexmatch
    y   = int(m.group('y')) + 1911 # 民國年轉西元年
    mon = int(m.group('m'))
    d   = int(m.group('d'))
    from datetime import date
    t.value = date(y, mon, d)

    return t

def t_CODE(t):
    r'^(diagram|code)(\[(?P<n>[^]]*)\])?\.(?P<title>.*)\n'
    m = t.lexer.lexmatch
    t.value = Tree('code') 
    t.value.name = m.group('n')
    t.value.title = m.group('title')
    if len(t.value.title) < 1:
        t.value.title = t.value.name
    t.value.file = t.lexer.file
    t.value.lineno = t.lexer.lineno
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
    r'^(?P<t>table|image|sect\d)\[(?P<n>[^]]*)\]$'
    t.value = Tree('insert') 
    t.value.node_type = t.lexer.lexmatch.group('t')
    t.value.node_name = t.lexer.lexmatch.group('n')
    return t

def t_LI(t):
    r'^\*[ ](?P<text>.*)'
    text = t.lexer.lexmatch.group('text')
    t.value = Tree('listitem', text)
    t.value.append(Tree('para', text))
    return t

def t_OL(t):
    r'^(?P<n>\#|\d+)\.(?P<text>.*)'
    n = t.lexer.lexmatch.group('n')
    text = t.lexer.lexmatch.group('text')
    t.value = Tree('olistitem', text)
    t.value.append(Tree('para', text))
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
    if t.value.name and len(t.value.title) < 1:
        t.value.title = t.value.name
    t.value.value = t.value.title
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

H  = r'^(\[(?P<oname>.*)\])?' # old name specifier should be deprecated
H += r'(?P<title>[^(\n]*)'
H += r'(\((?P<name>.*)\))?' # name specifier use this
H += r'\n(=+|-+|~+|\*+|\^+)$' 

@TOKEN(H)
def t_H(t):
    m = t.lexer.lexmatch
    t.lexer.lineno += m.group(0).count('\n')
     
    sep = m.group(0)[-1]

    level = '=-~*^'.index(sep) + 1
    title = m.group('title')
    name = m.group('name')
    oname = m.group('oname')

    t.type = 'H%s' % level
    t.value = Tree('sect%s' % level, title) 
    t.value.title = title
    t.value.level = level
    t.value.name = oname
    if name:
        t.value.name = name
    if not t.value.title: t.value.title = t.value.name
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

__lexer__  = lex.lex(reflags=re.M)

class MutipleFileLexer(object):
    def __init__(self, f = '__string__', startlineno = 0, indent = 0):
        self.lexer = __lexer__.clone()
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

def usage():
    usage = '''Dump the stxt doctree.
Syntax: %s stxt
Dump the stxt doctree of file named stxt.
'''
    return usage % os.path.basename(sys.argv[0])
    

if __name__ == '__main__':
    try:
        fn = sys.argv[1]
        with open(fn) as f:
            lexer.input(f.read())
        t = lexer.token()
        while t:
            console.info("%i:%s" % (t.lexer.lineno , t.type))
            t = lexer.token()
    except IndexError:
       console.info(usage()) 
