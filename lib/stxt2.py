import sys, lex, yacc
class TreeNode(object):
  def __init__(self):
    self.parent, self.children, self.name = None, [], str(id(self))
  def append(self, *nodes):
    for n in nodes: 
      n.parent = self
      self.children.append(n)
    return self
  def isRoot(self):
    return self.parent is None
  def height(self):
    if self.isRoot(): return 0
    c, h = self, 0
    while c.parent:
      c = c.parent
      h += 1
    return h
# Lexer
tokens = [
          'INCLUDE', 
          'HEAD1', 
          'HEAD2', 
          'HEAD3', 
          'LI',
          'OLI', 
          'CODEHEAD', 
          'CODEBLOCK', 
          'FOOTNOTE', 
          'INDENTLINE', 
          'EMPTYLINE', 
          'PARA'] 
def t_INCLUDE(t):
  r'^<(?P<file>.*)>\n'
  t.lexer.lineno += t.lexeme.count('\n')
  t.lexer.include_lexer = t.lexer.clone()
  t.lexer.include_lexer.read(t.lexer.lexmatch.group('file'))
  return t.lexer.include_lexer.token()
def t_HEAD1(t):
  r'^(\[(.*)\])?(?P<title>.*)\n=+\n'
  t.lexer.lineno += t.lexeme.count('\n')
  return t
def t_HEAD2(t):
  r'^(\[(.*)\])?(.*)\n-+\n'
  t.lexer.lineno += t.lexeme.count('\n')
  return t
def t_HEAD3(t):
  r'^(\[(.*)\])?(.*)\n~+\n'
  t.lexer.lineno += t.lexeme.count('\n')
  return t
def t_INDENTLINE(t):
  r'^ +(.*)\n'
  t.lexer.lineno += t.lexeme.count('\n')
  return t
def t_LI(t):
  r'\* (.*)\n'
  t.lexer.lineno += t.lexeme.count('\n')
  return t
def t_OLI(t):
  r'# (.*)\n'
  t.lexer.lineno += t.lexeme.count('\n')
  return t
def t_CODEHEAD(t):
  r'^code(\[(.*)\])?\.(?P<title>.*)\n'
  t.lexer.lineno += t.lexeme.count('\n')
  return t
def t_CODEBLOCK(t):
  r'((.+\n)+)(^::\n\n)'
  t.lexer.lineno += t.lexeme.count('\n')
  return t
def t_FOOTNOTE(t):
  r'^\.\. \[#] (.+)\n'
  t.lexer.lineno += t.lexeme.count('\n')
  return t
def t_EMPTYLINE(t):
  r'^\n'
  t.lexer.lineno += t.lexeme.count('\n')
  return t
def t_PARA(t):
  r'((.+\n)+)(\n|\Z)'
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
def p_sect1(p):
  '''sect1 : HEAD1 content1'''
def p_content1(p):
  '''content1 : sect2
              | block'''
def p_content1_children(p):
  '''content1 : content1 sect2
              | content1 block'''
def p_sect2(p):
  r'sect2 : HEAD2 content2'
def p_content2(p):
  '''content2 : sect3
             | block'''
def p_content2_children(p):
  '''content2 : content2 sect3
              | content2 block'''
def p_block(p):
  '''block : PARA
           | code
           | list EMPTYLINE
           | olist EMPTYLINE
           | footnotes EMPTYLINE
  '''
def p_code(p):
  r'code : CODEHEAD CODEBLOCK'
def p_npara(p): #nested paragraph
  '''npara : ilines EMPTYLINE
           | npara ilines EMPTYLINE
  '''
def p_ilines(p):
  '''ilines : INDENTLINE
            | ilines INDENTLINE
  '''
def p_olistitem(p):
  '''olistitem : OLI               
               | OLI npara
  '''
def p_olist(p):
  '''olist : olistitem
           | olist olistitem 
  '''
def p_listitem(p):
  '''listitem : LI
              | LI npara
  '''
def p_list(p):
  '''list : listitem
          | list listitem
  '''
def p_footnotes(p):
  '''footnotes : FOOTNOTE
               | footnotes FOOTNOTE'''
def p_error(t):
    print ("Syntax error at '%s'" \
          % str(t)).decode('utf8').encode('cp950')
yacc.yacc()
if __name__ == '__main__':
  # Give the lexer some input
  #lexer.writetab('lextab')
  #lexer.read(r"d:\stxt\stxt\db\concurrent_control.stx")

  # Tokenize
  #while True:
  #  tok = lexer.token()
  #  if not tok: break      # No more input
  #  print str(tok).decode('utf8').encode('cp950')
  parser = yacc.yacc()
  parser.read(r"d:\stxt\stxt\db\concurrent_control.stx")
