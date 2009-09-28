# coding=utf8
#from stxt_tree import DocTreeNode
#import sys, lex, yacc, unicodedata, unittest
import sys, unicodedata, unittest
# Lexer
def vlen(str):
  "visual width len: a chinese character'len is two times of ascii's"
  #str = unicode(str)
  len = 0
  for c in str:
    if unicodedata.east_asian_width(c) == 'W':
      len += 2
    else:
      len += 1
  return len

def vsubstr(str, start, end):
  #str = unicode(str)
  w = ''
  p = 0
  for c in str:
    if p == start and p < end:
      w += c
      p += vlen(c)
  return w

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
    self.assertEqual(vlen('時間'.decode('utf8')), vlen('===='))
    self.assertEqual(vlen(u'交易A      '), vlen('==========='))
    self.assertNotEqual(len(u'時間'), len('===='))

  def testvsubstr(self):
    self.assertEqual(u'時', vsubstr(u'時間', 0, 2))
    self.assertEqual(vsubstr(u'時間 交易A       交易B', ),
                     vsubstr(u'時間', 0, 2))
    

    #print vsubstr(unicode('時間'.decode('utf8')), 0, 2)

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
  #u = unicode(u)
  #for c in u:
  #  print c
  #  print unicodedata.category(c)
  #  print unicodedata.name(c)
  #  print unicodedata.east_asian_width(c)
