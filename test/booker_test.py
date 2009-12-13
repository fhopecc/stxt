# coding=utf8
import os, sys, unittest
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from lib.booker import lexer

class UnitTest(unittest.TestCase):
    def setUp(self):
        lexer.begin('INITIAL')

    def testEMPTYLINE(self):
        case = '''

    
'''
        lexer.input(case)
        t = lexer.token()
        self.assertEqual(2, t.lexer.lineno)
        self.assertEqual('EMPTYLINE', t.type)

        t = lexer.token()
        self.assertEqual(3, t.lexer.lineno)
        self.assertEqual('EMPTYLINE', t.type)


        t = lexer.token()
        self.assertEqual(4, t.lexer.lineno)
        self.assertEqual('EMPTYLINE', t.type)

    def testLine(self):
        case = '普通行'
        lexer.input(case)
        t = lexer.token()
        self.assertEqual(1, t.lexer.lineno)
        self.assertEqual('LINE', t.type)
        self.assertEqual('普通行', t.value)

    def testIndentLine(self):        
        case = '''普通行
  一層縮排
    二層縮排
      三層縮排
        四層縮排
          五層縮排
        '''
        lexer.input(case)
        t = lexer.token()
        self.assertEqual(1, t.lexer.lineno)
        self.assertEqual('LINE', t.type)
        self.assertEqual('普通行', t.value)

        t = lexer.token()
        self.assertEqual(2, t.lexer.lineno)
        self.assertEqual('L1LINE', t.type)
        self.assertEqual('一層縮排', t.value)

        t = lexer.token()
        self.assertEqual(3, t.lexer.lineno)
        self.assertEqual('L2LINE', t.type)
        self.assertEqual('二層縮排', t.value)

        t = lexer.token()
        self.assertEqual(4, t.lexer.lineno)
        self.assertEqual('L3LINE', t.type)
        self.assertEqual('三層縮排', t.value)

        t = lexer.token()
        self.assertEqual(5, t.lexer.lineno)
        self.assertEqual('L4LINE', t.type)
        self.assertEqual('四層縮排', t.value)

        t = lexer.token()
        self.assertEqual(6, t.lexer.lineno)
        self.assertEqual('L5LINE', t.type)
        self.assertEqual('五層縮排', t.value)

if __name__ == '__main__':
    unittest.main()
