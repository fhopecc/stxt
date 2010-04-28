# coding=utf8
from tree import Tree
import sys, lex, yacc, unicodedata, inliner

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
    m = t.lexer.lexmatch
    lexme = m.group(0)
    t.lexer.lineno += lexme.count('\n')
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
    print >>sys.stderr, 'lexerror:' + str(t) + t.lexedata
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
    header = Tree('tr', '')
    for v in row:
        header.append(Tree('th', v))
    table = Tree('table', '')
    table.append(header)
    # parse row
    for l in p[3]:
        r = Tree('tr', '')
        table.append(r)
        for v in parse_row(p[2], l):
            td = Tree('td', v)
            if type(v) in [str, unicode]:
                 para = parser_node(p, 1, type='para',
                                 value=v) 
            else:
                print 'v is not string, it is %s' % type(v)

            td.append(para)         
            r.append(td)
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

def parser_node(p, num=1, type=None, value=None):
    '''傳回表示此 Paser Element 的 Tree。
       會將 Parser 中的源碼資訊寫到 Tree 中。 
    '''
    source = p.lexer.source
    spos = p.lexpos(num)
    slineno = p.lineno(num)
    if not value:
        value = p[num]
    return Tree(type  = type,
                value = value, 
                source = source, 
                spos = spos,
                slineno = slineno
               )


def parse(input, source='__string__', lineno=0):
    lexer.source = source
    lexer.lineno = lineno
    d = parser.parse(input, lexer=lexer)
    return d
