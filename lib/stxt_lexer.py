# coding=utf8
import sys, stxt_tree, lex
from stxt_tree import DocTreeNode
# Lexer
tokens = [
          'INCLUDE', 
          'HEAD1', 
          'HEAD2', 
          'HEAD3', 
          'CODEHEAD', 
          'CODEBLOCK', 
          'TABLEHEAD', 
          'TABLEBLOCK', 
          #'ROWSEP', 
          'FOOTNOTE', 
          'LI',           # list start
          #'L2LI',         # level2 list start
          'OL', 
          #'L2OL', 
          'DL', 
          'EMPTYLINE', 
          'L3LINE', 
          'L2LINE', 
          'LINE'] 
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
def t_TABLEHEAD(t):
  r'^table(\[(?P<name>.*)\])?\.(?P<title>.*)\n'
  t.lexer.lineno += t.lexeme.count('\n')
  t.value = DocTreeNode('table') 
  m = t.lexer.lexmatch
  t.value.name = m.group('name')
  t.value.title = m.group('title')
  return t
def t_TABLEBLOCK(t):
  r'(.+\n)+=[= ]+\n'
  t.lexer.lineno += t.lexeme.count('\n')
  m = t.lexer.lexmatch
  t.value = m.group(0)
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
def t_OL(t):
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
def t_DL(t):
  r'^(?P<content>(?!(table|code|# |\* |  )).+)\n  ((?P<line>.*)\n)'
  t.lexer.lineno += t.lexeme.count('\n')
  t.value = t.lexer.lexmatch.group('content')
  t.value = DocTreeNode('dlistitem', t.value)
  line = t.lexer.lexmatch.group('line')
  if line:
    t.value.append(DocTreeNode('l2para', line))
  return t
#def t_L2OL(t):
#  r'  # (?P<content>.*)\n'
#  t.lexer.lineno += t.lexeme.count('\n')
#  t.value = t.lexer.lexmatch.group('content')
#  t.value = DocTreeNode('olistitem', t.value)
#  return t
#def t_L2LI(t):
#  r'  \* (?P<content>.*)\n'
#  t.lexer.lineno += t.lexeme.count('\n')
#  t.value = t.lexer.lexmatch.group('content')
#  t.value = DocTreeNode('listitem', t.value)
#  return t
def t_FOOTNOTE(t):
  r'^\.\. \[#] (?P<content>.+)\n'
  t.lexer.lineno += t.lexeme.count('\n')
  t.value = DocTreeNode('FOOTNOTE', 
              t.lexer.lexmatch.group('content'))
  return t
#def t_ROWSEP(t):
#  r'(^=[= ]*)\n'
#  t.lexer.lineno += t.lexeme.count('\n')
#  return t
def t_EMPTYLINE(t):
  r'^\n'
  t.lexer.lineno += t.lexeme.count('\n')
  return t
def t_L2LINE(t):
  r'^  (?P<content>.*)\n'
  t.lexer.lineno += t.lexeme.count('\n')
  t.value = t.lexer.lexmatch.group('content')
  return t
def t_LINE(t):
  r'^[^ ](.+)\n'
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
if __name__ == '__main__':
  # Give the lexer some input
  lexer.writetab('lextab')
  #lexer.read(r"d:\stxt\doc\net\ipsec.stx")
  lexer.read(r"d:\stxt\doc\net\ipsec.stx")
  # Tokenize
  while True:
    tok = lexer.token()
    if not tok: break      # No more input
    print str(tok).decode('utf8').encode('cp950')
