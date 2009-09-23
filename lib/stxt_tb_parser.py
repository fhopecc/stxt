# coding=utf8
from stxt_tree import DocTreeNode
import sys, lex, yacc, unicodedata, unittest
# Lexer
tokens = [
          'INCLUDE', 
          'HEAD1', 
          'HEAD2', 
          'HEAD3', 
          'LI',
          'OLI', 
          'DLI', 
          'CODEHEAD', 
          'CODEBLOCK', 
          'TABLEHEAD', 
          'TABLEBLOCK', 
          'FOOTNOTE', 
          'INDENTLINE', 
          'EMPTYLINE', 
          'LINE', 
          'PARA'] 
def t_INCLUDE(t):
  r'^<(?P<file>.*)>\n'
  t.lexer.lineno += t.lexeme.count('\n')
  t.lexer.include_lexer = t.lexer.clone()
  t.lexer.include_lexer.read(t.lexer.lexmatch.group('file'))
  return t.lexer.include_lexer.token()
def t_HEAD1(t):
  r'^(\[(?P<name>.*)\])?(?P<title>.*)\n=+\n'
  t.lexer.lineno += t.lexeme.count('\n')
  t.value = DocTreeNode('sect1') 
  m = t.lexer.lexmatch
  t.value.name = m.group('name')
  t.value.title = m.group('title')
  return t
def t_HEAD2(t):
  r'^(\[(?P<name>.*)\])?(?P<title>.*)\n-+\n'
  t.lexer.lineno += t.lexeme.count('\n')
  t.value = DocTreeNode('sect2') 
  m = t.lexer.lexmatch
  t.value.name = m.group('name')
  t.value.title = m.group('title')
  return t
def t_HEAD3(t):
  r'^(\[(?P<name>.*)\])?(?P<title>.*)\n~+\n'
  t.lexer.lineno += t.lexeme.count('\n')
  t.value = DocTreeNode('sect3') 
  m = t.lexer.lexmatch
  t.value.name = m.group('name')
  t.value.title = m.group('title')
  return t
def t_CODEHEAD(t):
  r'^code(\[(?P<name>.*)\])?\.(?P<title>.*)\n'
  t.lexer.lineno += t.lexeme.count('\n')
  t.value = DocTreeNode('code') 
  m = t.lexer.lexmatch
  t.value.name = m.group('name')
  t.value.title = m.group('title')
  return t
def t_CODEBLOCK(t):
  r'(?P<code>(.+\n)+)(^::\n\n)'
  t.lexer.lineno += t.lexeme.count('\n')
  m = t.lexer.lexmatch
  t.value = m.group('code')
  return t
def t_TABLEHEAD(t):
  r'^table(\[(?P<name>.*)\])?\.(?P<title>.*)\n'
  t.lexer.lineno += t.lexeme.count('\n')
  t.value = DocTreeNode('table') 
  m = t.lexer.lexmatch
  t.value.name = m.group('name')
  t.value.title = m.group('title')
  return t
def t_INDENTLINE(t):
  r'^ +(?P<content>.*)\n'
  t.lexer.lineno += t.lexeme.count('\n')
  t.value = t.lexer.lexmatch.group('content')
  return t
def t_OLI(t):
  r'# (?P<content>.*)\n'
  t.lexer.lineno += t.lexeme.count('\n')
  t.value = t.lexer.lexmatch.group('content')
  t.value = DocTreeNode('olistitem', t.value)
  return t
def t_LI(t):
  r'\* (?P<content>.*)\n'
  t.lexer.lineno += t.lexeme.count('\n')
  t.value = t.lexer.lexmatch.group('content')
  t.value = DocTreeNode('listitem', t.value)
  return t
def t_DLI(t):
  r'(?P<title>[^#*\n][^\n]+)\n(?=^  .*\n)'
  t.lexer.lineno += t.lexeme.count('\n')
  t.value = t.lexer.lexmatch.group('title')
  t.value = DocTreeNode('dlistitem', t.value)
  return t
def t_FOOTNOTE(t):
  r'^\.\. \[#] (?P<content>.+)\n'
  t.lexer.lineno += t.lexeme.count('\n')
  t.value = DocTreeNode('FOOTNOTE', 
              t.lexer.lexmatch.group('content'))
  return t
def t_EMPTYLINE(t):
  r'^\n'
  t.lexer.lineno += t.lexeme.count('\n')
  return t
def t_TABLEBLOCK(t):
  r'(.+\n)+=[= ]+\n'
  t.lexer.lineno += t.lexeme.count('\n')
  return t
def t_LINE(t):
  r'(.+)\n'
  t.lexer.lineno += t.lexeme.count('\n')
  return t
# Define a rule so we can track line numbers
#def t_newline(t):
#  r'\n+'
#  t.lexer.lineno += len(t.value)
# A string containing ignored characters (spaces and tabs)
#t_ignore  = ' \t'
# Error handling rule
def t_error(t):
  print "Error happened at " + \
         str(t).decode('utf8').encode('cp950')
  sys.exit()
lexer = lex.lex(debug=True)
# Parser 
def p_book(p):
  '''book : sect1          
          | book sect1'''
  if len(p) == 2:
    p[0] = DocTreeNode('book', '')
    p[0].append(p[1])
  else:
    p[1].append(p[2])
    p[0] = p[1]
def p_sect1(p):
  '''sect1 : HEAD1 content1s'''
  p[0] = p[1]
  for c in p[2]:
    p[0].append(c) 
def p_content1s(p):
  '''content1s : sect2
               | block
               | content1s sect2
               | content1s block'''
  if len(p) == 2:
    p[0] = [p[1]]
  else:
    p[1].append(p[2])
    p[0] = p[1]
def p_sect2(p):
  r'sect2 : HEAD2 content2s'
  p[0] = p[1]
  for c in p[2]:
    p[0].append(c) 
def p_content2s(p):
  '''content2s : sect3
               | block
               | content2s sect3
               | content2s block'''
  if len(p) == 2:
    p[0] = [p[1]]
  else:
    p[1].append(p[2])
    p[0] = p[1]
def p_sect3(p):
  r'sect3 : HEAD3 content3s'
  p[0] = p[1]
  for c in p[2]:
    p[0].append(c) 
def p_content3s(p):
  '''content3s : block
               | content3s block'''
  if len(p) == 2:
    p[0] = [p[1]]
  else:
    p[1].append(p[2])
    p[0] = p[1]
def p_block(p):
  '''block : PARA
           | code
           | table
           | list
           | list EMPTYLINE
           | footnotes EMPTYLINE
  '''
  if isinstance(p[1], str):
    p[0] = DocTreeNode('para', p[1])
  else:
    p[0] = p[1]
def p_code(p):
  r'code : CODEHEAD CODEBLOCK'
  p[0] = p[1]
  p[0].value = p[2]
def p_table(p):
  '''table : TABLEHEAD TABLEBLOCK'''
  p[0] = p[1]
  p[0].value = p[2]
def p_nparas(p): #nested paragraph
  '''nparas : npara EMPTYLINE
            | nparas npara EMPTYLINE
  '''
  if len(p) == 3:
    p[0] = [p[1]]
  elif len(p) == 4:
    p[1].append(p[2])
    p[0] = p[1]
def p_npara(p):
  '''npara : INDENTLINE
           | npara INDENTLINE
  '''
  if len(p) == 2:
    p[0] = DocTreeNode('npara', p[1])
  else:
    p[1].value +=  p[2]
    p[0] = p[1]
def p_listitem(p):
  '''listitem : LI 
              | OLI 
              | DLI
              | LI  nparas
              | OLI nparas
              | DLI nparas
  '''
  p[0] = p[1]
  if len(p) == 3 and p[2]:
    for npara in p[2]:
      p[0].append(npara)
def p_list(p):
  '''list : listitem
          | list listitem
  '''
  if len(p) == 2:
    if p[1].type == 'listitem':
      p[0] = DocTreeNode('list', '')
    elif p[1].type == 'olistitem':
      p[0] = DocTreeNode('olist', '')
    elif p[1].type == 'dlistitem':
      p[0] = DocTreeNode('dlist', '')
    p[0].append(p[1])
  else:
    p[0] = p[1].append(p[2])
def p_footnotes(p):
  '''footnotes : FOOTNOTE
               | footnotes FOOTNOTE'''
  if len(p) == 2:
    p[0] = DocTreeNode('footnotes', '')
    p[0].append(p[1])
  else:
    p[1].append(p[2])
    p[0] = p[1]
def p_error(t):
    print ("Syntax error at '%s'" \
          % str(t)).decode('utf8').encode('cp950')
parser = yacc.yacc()
def vlen(str):
  "visual width len: a chinese character'len is two times of ascii's"
  str = str.decode('utf8')
  len = 0
  for c in str:
    if unicodedata.east_asian_width(c) == 'W':
      len += 2
    else:
      len += 1
  return len

def vsubstr(str, start, end):
  str = str.decode('utf8')
  w = ''
  p = 0
  for c in str:
    if p == start:
      w += c
      p += vlen(c)
    if p >= end:

class Unittest(unittest.TestCase):
    
    def setUp(self):
      self.t1 = '''
時間 交易A       交易B
==== =========== ===========
t1   A.read(p)    
t2   A.update(p)  
t3               B.read(p)   
t4               B.update(p)
==== =========== ===========
''' 
    def testvlen(self):
      self.assertEqual(vlen('時間'), vlen(u'===='))
      self.assertEqual(vlen('交易A      '), vlen('==========='))
      self.assertNotEqual(len('時間'), len('===='))

    def testvsubstr(self)
      self.assertEqual('時', vsubstr('時間', 0, 2))

if __name__ == '__main__':
  unittest.main()
  # Give the lexer some input
  #lexer.writetab('lextab')
  #lexer.read(r"d:\stxt\doc\net\x_25.stx")
  # Tokenize
  #while True:
  #  tok = lexer.token()
  #  if not tok: break      # No more input
  #  print str(tok).decode('utf8').encode('cp950')
  #parser = yacc.yacc()
  #d = parser.read(r"d:\stxt\stxt\db\concurrent_control.stx")
  #d.print_type_tree(3)
  #for c in d.dfs():
  #  print c.type
  #u = u'張ac'
  #for c in u:
  #  print unicodedata.category(c)
  #  print unicodedata.name(c)
  #  print unicodedata.east_asian_width(c)
