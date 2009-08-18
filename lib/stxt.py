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
  def __init__(self, type):
    self.token, self.lexeme, self.value = token, lexeme, value
    self.pos, self.len = pos, len
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
# book     = book_
# book_    = '' | sect1 book_
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
    self.current_token = self.tokens[self.pos]
  def next_token(self):
    self.pos += 1
    self.current_token = self.tokens[self.pos]
  def accept(self, token):
    if self.current_token.token == token:
      print 'accept' + self.current_token.error_msg()
      self.next_token()
      return True
    return False
  def expect(self, token):
    if self.accept(token):
      return True
    raise ValueError("expect " + token + ", but was " +
      self.current_token.error_msg())
  def parse(self):
    return self.book()
  def book(self):
    return self.book_()
  def book_(self):
    if self.sect1():
      return self.book_()
    else:
      return True
  def sect1(self):
    if self.expect('HEAD1'):
      return self.content1()
  # content1 = content1_
  def content1(self):
    return self.content1_()
  # content1_= '' | sect2 content1_ | block content1_ 
  def content1_(self):
    if self.sect2():
      return self.content1_()
    elif self.block():
      return self.content1_()
    return True
  # sect2    = HEAD2 content2
  def sect2(self):
    if self.accept('HEAD2'):
      return self.content2()
    return False
  # content2 = content2_
  def content2(self):
    return self.content2_()
  # content2_= '' | block content2_
  def content2_(self):
    if self.block():
      return self.content2_()
    return True
  # block    = PARA | code | list EMPTYLINE
  def block(self):
    if self.accept('PARA'):
      return True
    elif self.code():
      return True
    elif self.list():
      return self.expect('EMPTYLINE')
    return False
  # list = LISTITEM list_ 
  def list(self):
    if self.accept('LISTITEM'):
      return self.list_()
    return False
  # list_ = '' | LISTITEM list_ 
  def list_(self):
    if self.accept('LISTITEM'):
      return self.list_()
    return True
  # code     = CODEHEAD CODEBLOCK   
  def code(self):
    if self.accept('CODEHEAD'):
      if self.accept('CODEBLOCK'):
        return True
    return False
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
    l.run()
    for t in l.tokens:
      print t.token+'['+t.lexeme.decode('utf8').encode('cp950')+']'
    print 'There are ' + str(len(l.tokens)) + ' tokens.'
class STXTParserTest(unittest.TestCase):
  def testSource(self):
    p = Parser(src='abcd')
    self.assertEqual('abcd', p.src)
  def testParse(self):
    p = Parser(src='abcd')
    self.assert_(p.parse())
if __name__ == '__main__':
  unittest.main()
