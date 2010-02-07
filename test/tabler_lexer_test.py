# coding=utf8
import os, sys, unittest
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from lib.stxt.tabler import lexer
from lib.stxt.tabler import vsubstr
from lib.stxt.tabler import vlen
class VisualLenthTest(unittest.TestCase):
    def testVLen(self):
        self.assertEqual(vlen('時間'.decode('utf8')), vlen(u'===='))
        self.assertEqual(vlen(u'交易A      '), vlen(u'==========='))
        self.assertNotEqual(len(u'時間'), len(u'===='))

    def testVSubstr(self):
        self.assertEqual(vsubstr(u'時間 交易A       交易B',0, 5), u'時間 ')
        self.assertEqual(vsubstr(u'時間 交易A       交易B',5, 10), u'交易A')

class LexerTest(unittest.TestCase):
    def testCOLSEP(self):
        case = '=== ====\n'
        lexer.input(case)
        tok = lexer.token()
        self.assertEqual(3, tok.value[0].length)
        self.assertEqual(4, tok.value[1].length)

        case = '========\n'
        lexer.input(case)
        tok = lexer.token()
        self.assertEqual('COLSEP', tok.type)
        self.assertEqual(8, tok.value[0].length)

    def testROWSEP(self):
#    testcase = '-- ---\n'
#        lexer.input(testcase)
#        tok = lexer.token()
#        self.assertEqual('ROWSEP', tok.type)
#        self.assertEqual('-- ---', tok.value)
#        testcase = '    -- ---\n'
#        lexer.input(testcase)
#        tok = lexer.token()
#        self.assertEqual('ROWSEP', tok.type)
#        self.assertEqual('    -- ---', tok.value)
        pass

    def testLINE(self):
        testcase = '時間 交易A       交易B\n'
        lexer.input(testcase)
        tok = lexer.token()
        self.assertEqual('LINE', tok.type)
        self.assertEqual('時間 交易A       交易B', tok.value)

if __name__ == '__main__':
    unittest.main()
