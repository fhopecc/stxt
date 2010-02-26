# coding=utf8
import os, sys, unittest
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from lib.stxt.inliner import lexer
from lib.stxt.inliner import parser

class UnitTest(unittest.TestCase):
    def setUp(self):
        lexer.begin('INITIAL')
        lexer.lineno = 1

    def testBACKSLASH(self):
        case = r'符號={數字, +, -, \*, /}'
        lexer.input(case)
        t = lexer.token()
        self.assertEqual('CBLOCK', t.type)

        t = lexer.token()
        self.assertEqual('BACKSLASH', t.type)

        t = lexer.token()
        self.assertEqual('STAR', t.type)

        t = lexer.token()
        self.assertEqual('CBLOCK', t.type)

    def testSQURE(self):
        case = '[test.sql]'
        lexer.input(case)
        t = lexer.token()
        self.assertEqual(1, t.lexer.lineno)
        self.assertEqual('LSQUARE', t.type)

        t = lexer.token()
        self.assertEqual('CBLOCK', t.type)
        self.assertEqual('test.sql', t.value)

        t = lexer.token()
        self.assertEqual('RSQUARE', t.type)

    def testSTAR(self):
        case = '字元塊[test.sql]*強調詞*'
        lexer.input(case)
        t = lexer.token()
        self.assertEqual(1, t.lexer.lineno)
        self.assertEqual('CBLOCK', t.type)
        self.assertEqual('字元塊', t.value)

        t = lexer.token()
        self.assertEqual(1, t.lexer.lineno)
        self.assertEqual('LSQUARE', t.type)

        t = lexer.token()
        self.assertEqual('CBLOCK', t.type)
        self.assertEqual('test.sql', t.value)

        t = lexer.token()
        self.assertEqual('RSQUARE', t.type)

        t = lexer.token()
        self.assertEqual(1, t.lexer.lineno)
        self.assertEqual('STAR', t.type)

        t = lexer.token()
        self.assertEqual('CBLOCK', t.type)
        self.assertEqual('強調詞', t.value)

        t = lexer.token()
        self.assertEqual(1, t.lexer.lineno)
        self.assertEqual('STAR', t.type)

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
        self.assertEqual('LSQUARE', t.type)

        t = lexer.token()
        self.assertEqual('CBLOCK', t.type)
        self.assertEqual('test.sql', t.value)

        t = lexer.token()
        self.assertEqual('RSQUARE', t.type)

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

    def testEscapeChar(self):
        case = r'''*\*字元塊*'''
        d = parser.parse(case)
        self.assertEqual('para', d.type)
        self.assertEqual(1, len(d.children))
        self.assertEqual('emphasis', d[0].type)
        self.assertEqual('*字元塊', d[0].value)

    def testPara(self):
        case = '''字元塊1
字元塊2
[test.sql][label:name:type][name:type]
'''
        d = parser.parse(case)
        self.assertEqual('para', d.type)
        self.assertEqual(4, len(d.children))

        cb = d.children[0]
        self.assertEqual('cblock', cb.type)
        self.assertEqual('字元塊1字元塊2', cb.value)

        ref = d.children[1]
        self.assertEqual('reference', ref.type)
        self.assertEqual('test.sql', ref.refname)

        ref = d.children[2]
        self.assertEqual('reference', ref.type)
        self.assertEqual('label', ref.label)
        self.assertEqual('name', ref.refname)
        self.assertEqual('type', ref.reftype)

        ref = d.children[3]
        self.assertEqual('reference', ref.type)
        self.assertEqual('name', ref.refname)
        self.assertEqual('type', ref.reftype)

if __name__ == '__main__':
    '''unittest.main()'''
    tests = unittest.TestSuite()
    tests.addTest(UnitTest("testSQURE"))
    tests.addTest(UnitTest("testBACKSLASH"))
    tests.addTest(UnitTest("testSTAR"))
    tests.addTest(UnitTest("testNEWLINE"))
    tests.addTest(UnitTest("testPara"))
    tests.addTest(UnitTest("testEscapeChar"))
    runner = unittest.TextTestRunner()
    runner.run(tests)
