# coding=utf8
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
class ParseTreeNode(TreeNode):
  def __init__(self, type, token=None):
    TreeNode.__init__(self)
    self.type, self.token= type, token, 
    if not token is None: 
      self.name, self.value = token.name, token.value
  def print_type_tree(self):
    print '*' * self.height() + self.type
    for c in self.children: c.print_type_tree()
  def print_postfix_tree(self):
    for c in self.children: c.print_postfix_tree()
    print '*' * self.height() + self.type
  def to_doctree(self):
    if self.type in ('book'):
      n = DocTreeNode(self.type)
      if len(self.children) == 2:
        n.append(*self.children[0].to_doctree())
      n.number_children()
      n.count_occurence()
      return n
    elif self.type in ('sect1s', 'content1', 'content2'):
      l = [self.children[0].to_doctree()]
      for c in self.children[1].to_doctree():
        l.append(c)
      return l
    elif self.type in ('list'):
      n = DocTreeNode(self.type)
      n.append(self.children[0].to_doctree())
      for c in self.children[1].to_doctree():
        n.append(c)
      return n
    elif self.type in ('sect1s_', 'content1_', 'content2_', \
                       'list_'):
      if len(self.children) == 2:
        l = [self.children[0].to_doctree()]
        for c in self.children[1].to_doctree():
          l.append(c)
        return l
      return []
    elif self.type in ('sect1', 'sect2'):
      n = DocTreeNode(self.type)
      h = self.children[0]
      n.name, n.title = h.name, h.value
      for c in self.children[1].to_doctree():
        n.append(c)
      return n
    elif self.type in ('PARA', 'LISTITEM'):
      return DocTreeNode(self.type, self.value)
    elif self.type in ('block'):
      return self.children[0].to_doctree()
    elif self.type in ('code'):
      n = DocTreeNode(self.type)
      h = self.children[0]
      n.name, n.title = h.name, h.value
      n.append(self.children[1].to_doctree())
      return n
    elif self.type in ('CODEBLOCK'):
      return DocTreeNode(self.type, self.value, self.token)
    else:
      raise ValueError, "no definition for " + self.type
class DocTreeNode(ParseTreeNode):
  def __init__(self, type, value='', token=None,**attr):
    ParseTreeNode.__init__(self, type, token)
    self.value, self.title = value, ''
    self.number, self.occurence = None, 0
  def __str__(self):
    m = "%s:\n[\n%s\n]" % (self.type, self.value)
    for c in self.children:
      m+=str(c)
  def __repr__(self):
    return str(self)
  def section_number(self):
    if self.number is None: return '' 
    else: 
      if self.parent.number is None: return str(self.number)
      else: return self.parent.section_number()+'.'+str(self.number)
  def number_children(self):
    cs = self.children
    if self.type in ('book'):
      for i, c in enumerate([c for c in cs if c.type == 'sect1']):
        c.number = i + 1
        c.number_children()
    elif self.type in ('sect1'):
      for i, c in enumerate([c for c in cs if c.type == 'sect2']):
        c.number = i + 1
  def _count_occurence(self, type, o=0):
    for c in self.children:
      if c.type in [type]:
        o = o+1
        c.occurence = o
      o = c._count_occurence(type, o)
    return o
  def count_occurence(self):
    for type in ['code', 'table']:
      if self.type in [type]:
        o = o+1
        self.occurence = o
      self._count_occurence(type)
  def print_tree(self):
    out = '+' * self.height() + self.type + '[' + self.name + ']' \
           +'#'+str(self.occurence)+'#' + self.section_number() + self.title + '\n'
    for c in self.children: out += c.print_tree()
    return out
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
  r'^ +(?P<content>.*)\n'
  t.lexer.lineno += t.lexeme.count('\n')
  t.value = t.lexer.lexmatch.group('content')
  return t
def t_LI(t):
  r'\* (?P<content>.*)\n'
  t.lexer.lineno += t.lexeme.count('\n')
  t.value = t.lexer.lexmatch.group('content')
  return t
def t_OLI(t):
  r'# (?P<content>.*)\n'
  t.lexer.lineno += t.lexeme.count('\n')
  t.value = t.lexer.lexmatch.group('content')
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
  r'^\.\. \[#] (?P<content>.+)\n'
  t.lexer.lineno += t.lexeme.count('\n')
  t.value = DocTreeNode('FOOTNOTE', 
              t.lexer.lexmatch.group('content'))
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
def p_sect3(p):
  r'sect3 : HEAD3 content3'
def p_content3(p):
  r'content3 : block'
def p_content3_children(p):
  r'content3 : content3 block'

def p_block(p):
  '''block : PARA
           | code
           | list EMPTYLINE
           | olist EMPTYLINE
           | footnotes EMPTYLINE
  '''
def p_code(p):
  r'code : CODEHEAD CODEBLOCK'
def p_nparas(p): #nested paragraph
  '''nparas : npara EMPTYLINE
            | nparas npara EMPTYLINE
  '''
  if len(p) == 3:
    p[0] = [DocTreeNode('NPARA', p[1])]
  elif len(p) == 4:
    p[0] = p[1].append(DocTreeNode('NPARA', p[2]))
  print str(p[0]).decode('utf8').encode('cp950')
def p_npara(p):
  '''npara : INDENTLINE
           | npara INDENTLINE
  '''
  if len(p) == 2:
    p[0] = p[1]
  else:
    p[0] = p[1] + p[2]
def p_olistitem(p):
  '''olistitem : OLI
               | OLI nparas
  '''
  p[0] = DocTreeNode('olistitem', p[1])
  if len(p) == 3 and p[2]:
    for npara in p[2]:
      p[0].append(npara)
  print str(p[0]).decode('utf8').encode('cp950')
def p_olist(p):
  '''olist : olistitem
           | olist olistitem 
  '''
def p_listitem(p):
  '''listitem : LI
              | LI nparas
  '''
  p[0] = DocTreeNode('listitem', p[1])
  if len(p) == 3:
    p[0].append(p[2])
  print str(p[0]).decode('utf8').encode('cp950')
def p_list(p):
  '''list : listitem
          | list listitem
  '''
def p_footnotes(p):
  '''footnotes : FOOTNOTE
               | footnotes FOOTNOTE'''
  if len(p) == 2:
    p[0] = [p[1]]
    print p[0]
  else:
    p[0] = p[1].append[p[2]]
    print p[0]
def p_error(t):
    print ("Syntax error at '%s'" \
          % str(t)).decode('utf8').encode('cp950')
yacc.yacc()
if __name__ == '__main__':
  # Give the lexer some input
  lexer.writetab('lextab')
  lexer.read(r"d:\stxt\stxt\db\concurrent_control.stx")

  # Tokenize
  while True:
    tok = lexer.token()
    if not tok: break      # No more input
    print str(tok).decode('utf8').encode('cp950')
  parser = yacc.yacc()
  parser.read(r"d:\stxt\stxt\db\concurrent_control.stx")
