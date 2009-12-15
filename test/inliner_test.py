# coding=utf8
import os, sys, unittest
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from lib.inliner import lexer, parser

class UnitTest(unittest.TestCase):
    def setUp(self):
        lexer.begin('INITIAL')

    def testREFERENCE(self):
        case = '[test.sql]'
        lexer.input(case)
        t = lexer.token()
        self.assertEqual(1, t.lexer.lineno)
        self.assertEqual('REFERENCE', t.type)
        self.assertEqual('test.sql', t.value.value)

        case = '字元塊[test.sql]*強調詞*'
        lexer.input(case)
        t = lexer.token()
        self.assertEqual(1, t.lexer.lineno)
        self.assertEqual('CBLOCK', t.type)
        self.assertEqual('字元塊', t.value)

        t = lexer.token()
        self.assertEqual(1, t.lexer.lineno)
        self.assertEqual('REFERENCE', t.type)
        self.assertEqual('test.sql', t.value.value)

        t = lexer.token()
        self.assertEqual(1, t.lexer.lineno)
        self.assertEqual('EMPHASIS', t.type)
        self.assertEqual('強調詞', t.value.value)

    def testEMPHASIS(self):
        case = '*強調詞*'
        lexer.input(case)
        t = lexer.token()
        self.assertEqual(1, t.lexer.lineno)
        self.assertEqual('EMPHASIS', t.type)
        self.assertEqual('強調詞', t.value.value)

        case = '字元塊*強調詞*'
        lexer.input(case)
        t = lexer.token()
        self.assertEqual(1, t.lexer.lineno)
        self.assertEqual('CBLOCK', t.type)
        self.assertEqual('字元塊', t.value)

        t = lexer.token()
        self.assertEqual(1, t.lexer.lineno)
        self.assertEqual('EMPHASIS', t.type)
        self.assertEqual('強調詞', t.value.value)

    def testCBLOCK(self):
        case = '字元塊'
        lexer.input(case)
        t = lexer.token()
        self.assertEqual(1, t.lexer.lineno)
        self.assertEqual('CBLOCK', t.type)
        self.assertEqual('字元塊', t.value)

    def testNEWLINE(self):
        case = '''字元塊1
字元塊2
  [test.sql]
'''
        lexer.input(case)
        t = lexer.token()
        self.assertEqual(1, t.lexer.lineno)
        self.assertEqual('CBLOCK', t.type)
        self.assertEqual('字元塊1', t.value)

        t = lexer.token()
        self.assertEqual(2, t.lexer.lineno)
        self.assertEqual('CBLOCK', t.type)
        self.assertEqual('字元塊2', t.value)

        t = lexer.token()
        self.assertEqual(3, t.lexer.lineno)
        self.assertEqual('CBLOCK', t.type)
        self.assertEqual('  ', t.value)

        t = lexer.token()
        self.assertEqual(3, t.lexer.lineno)
        self.assertEqual('REFERENCE', t.type)
        self.assertEqual('test.sql', t.value.value) 

    def test_cblock(self):           
        case = '字元塊'
        d = parser.parse(case)
        self.assertEqual('para', d.type)
        self.assertEqual(1, len(d.children))
        cb = d.children[0]
        self.assertEqual('cblock', cb.type)
        self.assertEqual('字元塊', cb.value)

        case = '''字元塊1
字元塊2
'''
        d = parser.parse(case)
        self.assertEqual('para', d.type)
        self.assertEqual(1, len(d.children))
        cb = d.children[0]
        self.assertEqual('cblock', cb.type)
        self.assertEqual('字元塊1字元塊2', cb.value)

    def test_para(self):
        case = '''字元塊1
字元塊2
  [test.sql]
'''
        d = parser.parse(case)
        self.assertEqual('para', d.type)
        self.assertEqual(2, len(d.children))

        cb = d.children[0]
        self.assertEqual('cblock', cb.type)
        self.assertEqual('字元塊1字元塊2  ', cb.value)

        ref = d.children[1]
        self.assertEqual('reference', ref.type)
        self.assertEqual('test.sql', ref.value)

if __name__ == '__main__':
#    unittest.main()

    tests = unittest.TestSuite()
    tests.addTest(UnitTest("testEMPTYLINE"))
    runner = unittest.TextTestRunner()
    runner.run(tests)
