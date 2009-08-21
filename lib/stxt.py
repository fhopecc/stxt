# coding=utf-8
from __future__ import with_statement
import os, re, unittest
class Token:
  def __init__(self, lexer, token, lexeme, value, pos, len):
    self.token, self.lexeme, self.value = token, lexeme, value
    self.lexer, self.pos, self.len = lexer, pos, len
  def to_str(self):
    line_num = self.lexer.src.count('\n', 0, self.pos) + 1
    output =  self.token+'('+str(line_num)+':'+str(self.pos)+') at '
    output += self.lexer.file 
    output += '{\n'+self.lexeme+'\n}'
    return output
  def to_cp950_str(self):
    return self.to_str().decode('utf8').encode('cp950')
def tokenize_file(f):
  fn = f
  with open(f) as f:
    l = Lexer(f.read())
    l.file = fn
    return l
class Lexer:
  def __init__(self, src):
    self.src, self.file = src, '_STRING_'
    self.grammar=[(re.compile(r'^<(.*)>\n', re.M), 
                   self.INCLUDE),
                  (re.compile(r'(^.*)\n=+\n', re.M), 
                   self.HEAD1),
                  (re.compile(r'(^.*)\n-+\n', re.M), 
                   self.HEAD2),
                  (re.compile(r'^code\.(.*)\n', re.M), 
                   self.CODEHEAD),
                  (re.compile(r'((.+\n)+)(^::\n\n)', re.M), 
                   self.CODEBLOCK),
                  (re.compile(r'\* (.*)\n', re.M), 
                   self.LISTITEM),
                  (re.compile(r'((.+\n)+)(\n|\Z)', re.M), 
                   self.PARA),
                  (re.compile(r'^\s*$\n', re.M), 
                   self.EMPTYLINE),
                  (re.compile(r'.*',re.M),
                   self.LEXERROR)
                 ]
  def run(self):
    self.cur, self.tokens = 0, []
    while self.cur < len(self.src):
      for p in self.grammar:
        m = p[0].match(self.src, self.cur)
        if m:
          p[1](m)
          self.cur += len(m.group(0))
          break
  def INCLUDE(self,m):
    lexeme = m.group(0)
    l = tokenize_file(m.group(1))
    l.run()
    for t in l.tokens: self.tokens.append(t)
  def HEAD1(self,m):
    lexeme = m.group(0)
    tok = Token(self, 'HEAD1', lexeme, m.group(1), self.cur+1, len(lexeme))
    self.tokens.append(tok)
  def HEAD2(self,m):
    lexeme = m.group(0)
    tok = Token(self, 'HEAD2', lexeme, m.group(1), self.cur+1, len(lexeme))
    self.tokens.append(tok)
  def CODEHEAD(self,m):
    lexeme = m.group(0)
    tok = Token(self, 'CODEHEAD', lexeme, m.group(1), self.cur+1, len(lexeme))
    self.tokens.append(tok)
  def CODEBLOCK(self,m):
    lexeme = m.group(0)
    tok = Token(self, 'CODEBLOCK', lexeme, m.group(1), self.cur+1, len(lexeme))
    self.tokens.append(tok)
  def PARA(self,m):
    lexeme = m.group(0)
    tok = Token(self, 'PARA', lexeme, m.group(1), self.cur+1, len(lexeme))
    self.tokens.append(tok)
  def LISTITEM(self,m):
    lexeme = m.group(0)
    tok = Token(self, 'LISTITEM', lexeme, m.group(1), self.cur+1, len(lexeme))
    self.tokens.append(tok)
  def EMPTYLINE(self,m):
    lexeme = m.group(0)
    tok = Token(self, 'EMPTYLINE', lexeme, 0, self.cur+1, len(lexeme))
    self.tokens.append(tok)
  def LEXERROR(self,m):
    raise ValueError('LexError at (' + 
          str(self.src[self.cur:]).decode('utf8').encode('cp950')+')')
class ParseTreeNode:
  def __init__(self, type, token=None):
    self.type, self.token, self.children= type, token, []
    self.title, self.parent = '', None
    if not token == None: self.value = token.value
  def append(self, *nodes):
    for n in nodes: 
      n.parent = self
      self.children.append(n)
    return self
  def isRoot(self):
    return self.parent == None
  def height(self):
    if self.isRoot(): return 0
    c, h = self, 0
    while c.parent:
      c = c.parent
      h += 1
    return h
  def print_type_tree(self):
    print '*' * self.height() + self.type
    for c in self.children: c.print_type_tree()
  def print_postfix_tree(self):
    for c in self.children: c.print_postfix_tree()
    print '*' * self.height() + self.type
class DocTreeNode:
  def __init__(self, type, value, *attr):
    self.type, self.token, self.children= type, token, []
    self.title, self.parent = '', None
    if not token is None: self.value = token.value
# book     = sect1s EOF | EOF
# sect1s   = sect1s sect1 | sect1
# sect1    = HEAD1 content1
# content1 = content1 sect2 | content1 block | sect2 | block
# sect2    = HEAD2 content2
# content2 = content2 block | block
# block    = PARA | code | list EMPTYLINE
# code     = CODEHEAD CODEBLOCK
# list     = list LISTITEM | LISTITEM 
# ::
# Removing left recursive
# ======================= 
# book     = sect1s EOF | EOF
# sect1s   = sect1 sect1s_
# sect1s_  = sect1 sect1s_ | ''
# sect1    = HEAD1 content1
# content1 = sect2 content1_ | block content1_
# content1_= sect2 content1_ | block content1_ | ''
# sect2    = HEAD2 content2
# content2 = block content2_
# content2_= block content2_ | ''
# block    = PARA | code | list
# code     = CODEHEAD  CODEBLOCK   
# list     = LISTITEM list_ 
# list_    = LISTITEM list_ | ''
def parse_file(f):
  return Parser(tokenize_file(f))
class Parser:
  def __init__(self, lexer):
    self.lexer, self.pos = lexer, 0
    self.lexer.run()
    self.tokens = self.lexer.tokens
    self.tokens.append(Token(self.lexer, 'EOF', \
      '_EOF_', None, len(self.lexer.src),0))
    self.advance()
  def has_seen(self, *tokens):
    if self.lookahead.token in tokens: return True
    return False
  def advance(self):
    try:
      self.lookahead = self.tokens[self.pos]
      self.pos += 1
    except:
      self.lookahead = None
  def match(self, token):
    if self.lookahead.token == token:
      print 'match ' + self.lookahead.to_cp950_str()
      n = ParseTreeNode(token, self.lookahead)
      self.advance()
      return n
    raise ValueError("match " + token + ", but was " + \
    self.lookahead.to_cp950_str())
  def parse(self):
    self.tree = self.book()
    return True
  # book = sect1s EOF | EOF
  def book(self):
    n = ParseTreeNode('book')
    if self.has_seen('HEAD1'):
      return n.append(self.sect1s(), self.match('EOF'))
    else: return n.append(self.match('EOF'))
  # sect1s = sect1 sect1s_
  def sect1s(self):
    return ParseTreeNode('sect1s').append(self.sect1(), self.sect1s_())
  # sect1s_ = sect1 sect1s_ | ''
  def sect1s_(self):
    n = ParseTreeNode('sect1s_')
    if self.has_seen('HEAD1'):
      n.append(self.sect1(), self.sect1s_())
    return n
  # sect1 = HEAD1 content1
  def sect1(self):
    return ParseTreeNode('sect1').append(self.match('HEAD1'), \
           self.content1())
  # content1 = sect2 content1_ | block content1_
  def content1(self):
    n = ParseTreeNode('content1')
    if self.has_seen('HEAD2'):
      return n.append(self.sect2(), self.content1_())
    elif self.has_seen('PARA', 'CODEHEAD', 'LISTITEM'):
      return n.append(self.block(), self.content1_())
  # content1_= sect2 content1_ | block content1_ | ''
  def content1_(self):
    n = ParseTreeNode('content1_')
    if self.has_seen('HEAD2'):
      n.append(self.sect2(), self.content1_())
    elif self.has_seen('PARA', 'CODEHEAD', 'LISTITEM'):
      n.append(self.block(), self.content1_())
    return n
  # sect2 = HEAD2 content2
  def sect2(self):
    return ParseTreeNode('sect2').append(self.match('HEAD2'), self.content2())
  # content2 = block content2_
  def content2(self):
    return ParseTreeNode('content2').append(self.block(), self.content2_())
  # content2_= block content2_ | ''
  def content2_(self):
    n = ParseTreeNode('content2_')
    if self.has_seen('PARA', 'CODEHEAD', 'LISTITEM'):
      n.append(self.block(), self.content2_())
    return n
  # block = PARA | code | list EMPTYLINE
  def block(self):
    n = ParseTreeNode('block')
    if self.has_seen('PARA'): return n.append(self.match('PARA'))
    elif self.has_seen('CODEHEAD'): return n.append(self.code())
    elif self.has_seen('LISTITEM'):
      return n.append(self.list(), self.match('EMPTYLINE'))
  # code = CODEHEAD CODEBLOCK   
  def code(self):
    return ParseTreeNode('code').append(self.match('CODEHEAD'), \
           self.match('CODEBLOCK'))
  # list = LISTITEM list_ 
  def list(self):
    return ParseTreeNode('list').append(self.match('LISTITEM'), \
           self.list_())
  # list_ = LISTITEM list_ | ''
  def list_(self):
    n = ParseTreeNode('list_')
    if self.has_seen('LISTITEM'):
      n.append(self.match('LISTITEM'), self.list_())
    return n
def to_html(tree):
  if tree.type == 'sect1':
    html =  '<h1>' + tree.title +'</h1>\n'
  elif tree.type == 'sect2':
    html =  '<h2>' + tree.title +'</h2>\n'
  elif tree.type == 'code':
    html = '<h4>程式碼：' + tree.title +'</h4>\n'
  elif tree.type == 'CODEBLOCK':
    html = '<pre>\n' + tree.value + '</pre>\n'
  elif tree.type == 'PARA':
    html = '<p>\n' + tree.value + '</p>\n'
  else:
    html = ' ' * tree.height() + tree.type + '\n'
  for c in tree.children:
    if c.type == 'list':
      html += '<ul>\n'
      for i in c.children:
        html += '<li>' + i.value + '</li>\n'
      html += '</ul>\n'
    else:
      html += to_html(c)
  return html
class RTxtLexerTest(unittest.TestCase):
  def testSource(self):
    l = Lexer(src='abcd')
    self.assertEqual('abcd', l.src)
class STXTParserTest(unittest.TestCase):
  def testSource(self):
    p = Parser(src='abcd')
    self.assertEqual('abcd', p.src)
  def testParse(self):
    p = Parser(src='abcd')
    self.assert_(p.parse())
if __name__ == '__main__':
  #l = tokenize_file(r"d:\stxt\stxt\db\concurrent_control.stx")
  #l.run()
  #for t in l.tokens:
  #  print t.token+'['+t.lexeme.decode('utf8').encode('cp950')+']'
  #print 'There are ' + str(len(l.tokens)) + ' tokens.'
  #p = parse_file(r"d:\stxt\stxt\db\timestamp.stx")
  p = parse_file(r"d:\stxt\stxt\db\concurrent_control.stx")
  p.parse()
  p.tree.print_type_tree()
  print '-' * 10
  p.tree.print_postfix_tree()
  #print to_html(p.tree).decode('utf8').encode('cp950')

  #with open(r'd:\stxt\html\db.html', 'w') as f:
  #  f.write('<html>\n'+to_html(p.tree)+'</html>')
  #unittest.main()
