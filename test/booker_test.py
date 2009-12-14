# coding=utf
import os, sys, unittest
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from lib.booker import lexer

class UnitTest(unittest.TestCase):
    def setUp(self):
        lexer.begin('INITIAL')

    def testHSEP(self):
        case = '''=========
---------
~~~~~~~~~
*********
^^^^^^^^^
'''
        lexer.input(case)
        t = lexer.token()
        self.assertEqual('H1SEP', t.type)

        t = lexer.token()
        self.assertEqual('H2SEP', t.type)

        t = lexer.token()
        self.assertEqual('H3SEP', t.type)

        t = lexer.token()
        self.assertEqual('H4SEP', t.type)

        t = lexer.token()
        self.assertEqual('H5SEP', t.type)

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

    def testInclude(self):
        case = r'<d:\stxt\lib\db\sql.stx>'
        lexer.input(case)
        self.assertRaises(IOError, lexer.token)

        case = r'<test.stx>'
        lexer.input(case)
        tok = lexer.token()
        self.assertEqual('test.stx', tok.lexer.file)
        self.assertEqual('LINE', tok.type)
        tok = lexer.token()
        self.assertEqual('H1SEP', tok.type)

        # lexer.input should reset include_lexer as None
        case = '普通行'
        lexer.input(case)
        t = lexer.token()
        self.assertEqual(1, t.lexer.lineno)
        self.assertEqual('LINE', t.type)
 
    def testLI(self):        
        case = '''* 普通條列
  * 一層縮排條列
    * 二層縮排條列
      * 三層縮排條列
        * 四層縮排條列
          * 五層縮排條列
        '''
        lexer.input(case)
        t = lexer.token()
        self.assertEqual(1, t.lexer.lineno)
        self.assertEqual('LI', t.type)
        self.assertEqual('普通條列', t.value.value)

        t = lexer.token()
        self.assertEqual(2, t.lexer.lineno)
        self.assertEqual('L1LI', t.type)
        self.assertEqual('一層縮排條列', t.value.value)

        t = lexer.token()
        self.assertEqual(3, t.lexer.lineno)
        self.assertEqual('L2LI', t.type)
        self.assertEqual('二層縮排條列', t.value.value)

        t = lexer.token()
        self.assertEqual(4, t.lexer.lineno)
        self.assertEqual('L3LI', t.type)
        self.assertEqual('三層縮排條列', t.value.value)

        t = lexer.token()
        self.assertEqual(5, t.lexer.lineno)
        self.assertEqual('L4LI', t.type)
        self.assertEqual('四層縮排條列', t.value.value)

        t = lexer.token()
        self.assertEqual(6, t.lexer.lineno)
        self.assertEqual('L5LI', t.type)
        self.assertEqual('五層縮排條列', t.value.value)

    def testLine(self):        
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
