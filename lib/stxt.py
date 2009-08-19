from __future__ import with_statement
import os, re, unittest
class Token:
  def __init__(self, token, lexeme, value, pos, len):
    self.token, self.lexeme, self.value = token, lexeme, value
    self.pos, self.len = pos, len
  def error_msg(self):
    return self.token+'('+str(self.pos)+')'+ \
        '['+self.lexeme.decode('utf8').encode('cp950')+']'
class ParseTreeNode:
  def __init__(self, type, token):
    self.type, self.token = type, token
#
# book     = book sect1
# sect1    = HEAD1 content1
# content1 = content1 sect2 | content1 block
# sect2    = HEAD2 content2
# content2 = content2 block
# block    = PARA | code | list EMPTYLINE
# list     = list LISTITEM | LISTITEM 
# code     = CODEHEAD CODEBLOCK   
# ::
# Removing left recursive
# ======================= 
# book     = sect1 book_ | ''
# book_    = sect1 book_ | ''
# sect1    = HEAD1 content1
# content1 = content1_
# content1_= '' | sect2 content1_ | block content1_ 
# sect2    = HEAD2 content2
# content2 = content2_
# content2_= '' | block content2_
# block    = PARA | code | list
# list     = LISTITEM list_ 
# list_    = '' | LISTITEM list_ 
# code     = CODEHEAD  CODEBLOCK   
class Parser:
  def __init__(self, src):
    self.src, self.pos = src, 0
    l = Lexer(src='abcd')
    l.run()
    self.tokens = l.tokens
    self.advance()
  def advance(self):
    try:
      self.lookahead = self.tokens[self.pos]
      self.pos += 1
    except:
      self.lookahead = None
  def accept(self, token):
    if self.lookahead.token == token:
      print 'accept ' + self.lookahead.error_msg()
      self.advance()
      return True
    return False
  def expect(self, token):
    if not self.accept(token):
      raise ValueError("expect " + token + ", but was " + \
      self.lookahead.error_msg())
  def parse(self):
    self.book()
    return True
  # book = sect1 book_ | ''
  def book(self):
    if self.lookahead == None:
      return
    elif self.lookahead.token == 'HEAD1':
      self.sect1()
      self.book_()
  # book_ = sect1 book_ | ''
  def book_(self):
    if self.lookahead == None:
      return
    elif self.lookahead.token == 'HEAD1':
      self.sect1()
      self.book_()
  # sect1 = HEAD1 content1
  def sect1(self):
    self.expect('HEAD1')
    self.content1()
  # content1 = sect2 content1_ | block content1_
  def content1(self):
    if self.lookahead.token == 'HEAD2':
      self.sect2()
      self.content1_()
    else:
      self.block()
      self.content1_()
  # content1_= sect2 content1_ | block content1_  | ''
  def content1_(self):
    if self.lookahead == None:
      return
    elif self.lookahead.token == 'HEAD2':
      self.sect2()
      self.content1_()
    else:
      self.block()
      self.content1_()
  # sect2 = HEAD2 content2
  def sect2(self):
    if self.expect('HEAD2'):
      self.content2()
  # content2 = content2_
  def content2(self):
    return self.content2_()
  # content2_= '' | block content2_
  def content2_(self):
    if self.block():
      return self.content2_()
    return True
  # block = PARA | code | list EMPTYLINE
  def block(self):
    if self.lookahead.token == 'PARA':
      self.expect('PARA')
    elif self.lookahead.token == 'CODEHEAD':
      self.code()
    elif self.lookahead.token == 'LISTITEM':
      self.list()
      self.expect('EMPTYLINE')
  # code = CODEHEAD CODEBLOCK   
  def code(self):
    self.expect('CODEHEAD')
    self.expect('CODEBLOCK')
  # list = LISTITEM list_ 
  def list(self):
    self.expect('LISTITEM')
    self.list_()
  # list_ = LISTITEM list_ | ''
  def list_(self):
    if self.lookahead.token == 'LISTITEM':
      self.expect('LISTITEM')
      self.list_()
class Lexer:
  def __init__(self, src):
    self.src=src
    self.grammar=[(re.compile(r'^\S*\n=+\n', re.M), 
                   self.HEAD1),
                  (re.compile(r'^code\..*\n', re.M), 
                   self.CODEHEAD),
                  (re.compile(r'(.+\n)+(^::\n)', re.M), 
                   self.CODEBLOCK),
                  (re.compile(r'\* .*\n', re.M), 
                   self.LISTITEM),
                  (re.compile(r'(.+\n)+(\n|\Z)', re.M), 
                   self.PARA),
                  (re.compile(r'^\s*$\n', re.M), 
                   self.EMPTYLINE),
                  (re.compile(r'.*',re.M),
                   self.LEXERROR)
                 ]
  def run(self):
    with open(r"d:\stxt\stxt\db\timestamp.stx") as f:
      self.src, self.cur, self.tokens = f.read(), 0, []
      while self.cur < len(self.src):
        for p in self.grammar:
          m = p[0].match(self.src, self.cur)
          if m:
            p[1](m)
            self.cur += len(m.group(0))
            break
  def HEAD1(self,m):
    lexeme = m.group(0)
    tok = Token('HEAD1', lexeme, 0, self.cur+1, len(lexeme))
    self.tokens.append(tok)
  def CODEHEAD(self,m):
    lexeme = m.group(0)
    tok = Token('CODEHEAD', lexeme, 0, self.cur+1, len(lexeme))
    self.tokens.append(tok)
  def CODEBLOCK(self,m):
    lexeme = m.group(0)
    tok = Token('CODEBLOCK', lexeme, 0, self.cur+1, len(lexeme))
    self.tokens.append(tok)
  def PARA(self,m):
    lexeme = m.group(0)
    tok = Token('PARA', lexeme, 0, self.cur+1, len(lexeme))
    self.tokens.append(tok)
  def LISTITEM(self,m):
    lexeme = m.group(0)
    tok = Token('LISTITEM', lexeme, 0, self.cur+1, len(lexeme))
    self.tokens.append(tok)
  def EMPTYLINE(self,m):
    lexeme = m.group(0)
    tok = Token('EMPTYLINE', lexeme, 0, self.cur+1, len(lexeme))
    self.tokens.append(tok)
  def LEXERROR(self,m):
    raise ValueError('LexError at (' + 
          str(self.src[self.cur:]).decode('utf8').encode('cp950')+')')
class RTxtLexerTest(unittest.TestCase):
  def testSource(self):
    l = Lexer(src='abcd')
    self.assertEqual('abcd', l.src)
    #l.run()
    #for t in l.tokens:
    #  print t.token+'['+t.lexeme.decode('utf8').encode('cp950')+']'
    #print 'There are ' + str(len(l.tokens)) + ' tokens.'
class STXTParserTest(unittest.TestCase):
  def testSource(self):
    p = Parser(src='abcd')
    self.assertEqual('abcd', p.src)
  def testParse(self):
    p = Parser(src='abcd')
    self.assert_(p.parse())
if __name__ == '__main__':
  p = Parser(src='abcd')
  p.parse()
  #unittest.main()
