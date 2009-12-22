# coding=utf8
from __future__ import with_statement
import sys, os, re, lex3
from stxt_tree import DocTreeNode

tokens = [
          'INSERT', 
          'H1SEP', 'H2SEP', 'H3SEP', 'H4SEP', 'H5SEP', 
          'EMPTYLINE', 
          'LINE', 'L1LINE', 'L2LINE', 'L3LINE', 'L4LINE', 'L5LINE', 
          'LI', 'L1LI', 'L2LI', 'L3LI', 'L4LI', 'L5LI', 
          'OL', 'L1OL', 'L2OL', 'L3OL', 'L4OL', 'L5OL', 
          'IMAGE', 'DEFINE', 'THEOREM', 'QUESTION',
          'CODE', 'TABLE', 
          'PROOF', 'ANSWER', 
          'FOOTNOTE', 'CITATION', 
          'CODESEP', 'TABLESEP'
         ] 

t_H1SEP = r'^=+$'
t_H2SEP = r'^-+$'
t_H3SEP = r'^~+$'
t_H4SEP = r'^\*+$'
t_H5SEP = r'^\^+$'
t_TABLESEP = r'^=+[ ][= ]+$'
t_CODESEP = r'^::\s*$'

def find_column(input,token):
    last_cr = input.rfind('\n',0,token.lexpos)
    if last_cr < 0:
	last_cr = 0
    column = (token.lexpos - last_cr) + 1
    return column

def t_INCLUDE(t):
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

#def t_code(t):
#    r'^code(\[(?P<name>.*)\])?\.(?P<title>.*)\n'
#    t.lexer.lineno += t.lexeme.count('\n')
#    t.type = 'CODEHEAD'
#    t.value = DocTreeNode('code') 
#    m = t.lexer.lexmatch
#    t.value.name = m.group('name')
#    t.value.title = m.group('title')
#    if not t.value.title: 
#        t.value.title = t.value.name
#    t.lexer.block_start = t.lexer.lexpos# + len(m.group(0))
#    t.lexer.block_start_lineno = t.lexer.lineno
#    t.lexer.begin('code')
#    return t

#def t_code_sep(t):
#    r'::\n'
#    t.lexer.lineno += 1
#    block_end = t.lexer.lexpos - 3
#    t.block = t.lexer.lexdata[t.lexer.block_start:block_end]
#    t.type = 'CODEBLOCK'
#    t.value = t.block
#    t.lineno = t.lexer.block_start_lineno 
#    t.lexer.begin('INITIAL')
#    return t

#def t_code_line(t):
#    r'[^\n]+'
#    pass 

#def t_code_newline(t):
#    r'\n+'
#    t.lexer.lineno += len(t.value) 
 
#def t_code_error(t):
#    msg = 'state(code)%s:%s:%s illegal char [%s]' % (
#                t.lexer.file, t.lexer.lineno, 
#                find_column(t.lexer.lexdata, t), t.value[0]
#               )
#    logger.info(msg)
#    raise SyntaxError, msg

def t_INSERT(t):
    r'^(?P<t>table|image)\[(?P<n>[^]]*)\]$'
    t.value = DocTreeNode('insert') 
    t.node_type = t.lexer.lexmatch.group('t')
    t.name = t.lexer.lexmatch.group('n')
    return t

def t_LI(t):
    r'^(?P<s>[ ]*)(\*[ ](?P<c>.*))'
    s = t.lexer.lexmatch.group('s')
    level = len(s) / 2
    t.value = t.lexer.lexmatch.group('c')
    t.value = DocTreeNode('listitem', t.value)
    if level > 0:
        t.type = 'L%sLI' % str(level)
    return t

def t_OL(t):
    r'^(?P<s>[ ]*)((?P<n>\#|\d+)\.[ ](?P<c>.*))'
    s = t.lexer.lexmatch.group('s')
    n = t.lexer.lexmatch.group('n')
    level = len(s) / 2
    t.value = t.lexer.lexmatch.group('c')
    t.value = DocTreeNode('olistitem', t.value)
    t.value.number = n
    if level > 0:
        t.type = 'L%sOL' % str(level)
    return t

def t_HEAD(t):
    r'^(?P<h>\w+)(\[(?P<n>\w*)\])?\.(?P<title>.*)$'
    m = t.lexer.lexmatch
    t.type = m.group('h').upper()
    t.value = DocTreeNode(t.type) 
    t.value.name = m.group('n')
    t.value.title = m.group('title')
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
    r'(?P<s>[ ]*)(?P<l>[^ \n=\-~*^<#:].+)'
    s = t.lexer.lexmatch.group('s')
    level = len(s) / 2
    t.value = DocTreeNode('line', t.lexer.lexmatch.group('l'))
    t.value.level = level
    return t

def t_EMPTYLINE(t):
    r'^[ ]*\n'
    t.lexer.lineno += t.value.count('\n')
    return t

def t_NEWLINE(t):
    r'\n+'
    t.lexer.lineno += t.value.count('\n')

def t_error(t):
    c = t.value[0]
    raise lex3.LexError('illegal char[%s](%s) at %s:%s:%s\n%s' % (c, 
                str(ord(t.value[0])), t.lexer.file, t.lexer.lineno, 
                find_column(t.lexer.lexdata, t), t.lexer.lexdata), 
                t.lexer.lexdata)

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
        self.file = f
        with open(f) as f:
           self.input(f.read())

lexer = MutipleFileLexer()

#if __name__ == '__main__':
#    case = 'theorem[name].this is a theorem title'
#    lexer.input(case)
#    tok = lexer.token()
#    print tok
