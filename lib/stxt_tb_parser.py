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
tokens = ['ROWSEP', 'LINE']

def t_ROWSEP(t):
  r'(?P<sepline>=+[= ]*)\n'
  t.lexer.lineno += t.lexeme.count('\n')
  m = t.lexer.lexmatch
  sepline = m.group('sepline')
  seps = sepline.split(' ')
  cols = []
  for s in seps:
    cols.append(Column(len(s)))
  t.value = cols
  return t

def t_LINE(t):
  r'(?P<line>[^ ]+.*)\n'
  m = t.lexer.lexmatch
  line = m.group('line')
  t.value = line
  return t

def t_error(t):
  print 'lexerror:' + str(t) + t.lexeme
  sys.exit()

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
  '''table : lines ROWSEP lines ROWSEP'''
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
  print 'parse error:' + str(t) + t.lexeme
  sys.exit()

parser = yacc.yacc()

def parse(input):
  d = parser.parse(input, lexer=lexer)
  return d

class VisualLenthTest(unittest.TestCase):
  def testVLen(self):
    self.assertEqual(vlen('時間'.decode('utf8')), vlen(u'===='))
    self.assertEqual(vlen(u'交易A      '), vlen(u'==========='))
    self.assertNotEqual(len(u'時間'), len(u'===='))

  def testVSubstr(self):
    self.assertEqual(vsubstr(u'時間 交易A       交易B',0, 5), u'時間 ')
    self.assertEqual(vsubstr(u'時間 交易A       交易B',5, 10), u'交易A')

class LexerTest(unittest.TestCase):
  def testROWSEP(self):
    testcase = '=== ====\n'
    lexer.input(testcase)
    tok = lexer.token()
    self.assertEqual(3, tok.value[0].length)
    self.assertEqual(4, tok.value[1].length)

  def testLINE(self):
    testcase = '時間 交易A       交易B\n'
    lexer.input(testcase)
    tok = lexer.token()
    self.assertEqual('時間 交易A       交易B', tok.value)

class ParserTest(unittest.TestCase):
  def testParseRow(self):
    testcase = u'==== =========== ===========\n'
    lexer.input(testcase)
    tok = lexer.token()
    cols = tok.value
    vals = parse_row(cols, u't3               B.read(p)')
    self.assertEqual(u't3', vals[0])
    self.assertEqual(u'', vals[1])
    self.assertEqual(u'B.read(p)', vals[2])

  def testParser(self):
    testcase = '''時間 交易A       交易B
==== =========== ===========
t1   A.read(p)    
t2   A.update(p)  
t3               B.read(p)   
t4               B.update(p)
==== =========== ===========
'''
    d = parser.parse(testcase.decode('utf8'))   
    self.assertEqual('table', d.type)
    header = d.children[0]
    self.assertEqual('tr', header.type)
    th1 = header.children[0]
    self.assertEqual('th', th1.type)
    self.assertEqual(u'時間', th1.value)
    th2 = header.children[1]
    self.assertEqual(u'交易A', th2.value)
    
    r2 = d.children[1]
    self.assertEqual('tr', r2.type)
    td1 = r2.children[0]
    self.assertEqual('td', td1.type)
    self.assertEqual('t1', td1.value)
    td2 = r2.children[1]
    self.assertEqual('A.read(p)', td2.value)

  def testParseError(self):
    testcase = '''時間 交易A       交易B
==== =========== ===========中文'''
    self.assertRaises(SystemExit, parser.parse, testcase.decode('utf8'))

if __name__ == '__main__':
  unittest.main()
  #testcase = u'==== =========== ===========\n'
  #lexer.input(testcase)
  #tok = lexer.token()
  #cols = tok.value
  #vals = parse_row(cols, u't3               B.read(p)')
  #self.assertEqual(u't3', vals[0])
  #self.assertEqual(u'', vals[1])
  #self.assertEqual(u'B.read(p)', vals[2])
