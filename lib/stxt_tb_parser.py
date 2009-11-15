# coding=utf8
from stxt_tree import DocTreeNode
import sys, lex, yacc, unicodedata, unittest

# Visual Length:
# These functions must pass unicode string as arguments
def vlen(str):
    "visual width len: a chinese character'len is two times of ascii's"
    len = 0
    for c in str:
        if unicodedata.east_asian_width(c) == 'W':
            len += 2
        else:
            len += 1
    return len

def vsubstr(str, start, end):
    "visual width len: a chinese character'len is two times of ascii's"
    w = ''
    p = 0
    for c in str:
        if p >= start and p < end:
            w += c
        p += vlen(c)
    return w

# column definition
class Column(object):
    def __init__(self, len):
        self.length = len

# Lexer
tokens = ['COLSEP', 'LINE']#, 'ROWSEP']

def t_COLSEP(t):
    r'(?P<sepline>=+[= ]*)(\n|$)'
    t.lexer.lineno += t.lexeme.count('\n')
    m = t.lexer.lexmatch
    sepline = m.group('sepline')
    seps = sepline.split(' ')
    cols = []
    for s in seps:
        cols.append(Column(len(s)))
    t.value = cols
    return t

#def t_ROWSEP(t):
#    r'(?P<rowsep>[- ]*)\n'
#    m = t.lexer.lexmatch
#    line = m.group('rowsep')
#    t.value = line
#    return t

def t_LINE(t):
    r'(?P<line>.+)\n'
    m = t.lexer.lexmatch
    line = m.group('line')
    t.value = line
    return t

def t_error(t):
    print >>sys.stderr, 'lexerror:' + str(t) + t.lexeme
    raise SyntaxError('lexerror')

lexer = lex.lex()
# Parser

def parse_row(cols, l):
    vals = []
    p = 0
    for c in cols:
        vals.append(vsubstr(l, p, p + c.length).strip())
        p += c.length + 1
    return vals

def p_simple_table(p):
    '''table : lines COLSEP lines COLSEP'''
    # parse header
    row = None
    for l in p[1]:
        if row:
            vals = parse_row(p[2], l)
            for i in range(0, len(row)):
                row[i] += vals[i]
        else:
            row = parse_row(p[2], l)
    header = DocTreeNode('tr', '')
    for v in row:
        header.append(DocTreeNode('th', v))
    table = DocTreeNode('table', '')
    table.append(header)
    # parse row
    for l in p[3]:
        r = DocTreeNode('tr', '')
        table.append(r)
        for v in parse_row(p[2], l):
            r.append(DocTreeNode('td', v))
    p[0] = table

def p_lines(p):
    '''lines : LINE
             | lines LINE'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[1].append(p[2])
        p[0] = p[1]

def p_error(t):
    raise SyntaxError('parse error:' + str(t))

parser = yacc.yacc()

def parse(input):
    d = parser.parse(input, lexer=lexer)
    return d


