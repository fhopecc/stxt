# coding=utf8
import os, sys, unittest
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from lib.stxt.inliner import lexer
from lib.stxt.inliner import parser
from lib.stxt import inliner

class UnitTest(unittest.TestCase):
    def setUp(self):
        lexer.begin('INITIAL')
        lexer.lineno = 1
        lexer.sourcefile = '__string__'

    def testESCAPESTRING(self):
        case = "''[[test.sql]]''"
        lexer.input(case)
        t = lexer.token()
        self.assertEqual(1, t.lexer.lineno)
        self.assertEqual('ESCAPESTRING', t.type)
        self.assertEqual('[[test.sql]]', t.value)

        case = '''字元塊1''[[test.sql]]''
字元塊2
'''
        lexer.input(case)
        t = lexer.token()
        self.assertEqual(1, t.lexer.lineno)
        self.assertEqual('CBLOCK', t.type)
        self.assertEqual('字元塊1', t.value)

        t = lexer.token()
        self.assertEqual(1, t.lexer.lineno)
        self.assertEqual('ESCAPESTRING', t.type)
        self.assertEqual('[[test.sql]]', t.value)

    def testREFERENCE(self):
        case = '[[test.sql]]'
        lexer.input(case)
        t = lexer.token()
        self.assertEqual(1, t.lexer.lineno)
        self.assertEqual('REFERENCE', t.type)

    def testCBLOCK(self):
        case = r'符號={數字, +, -, *, /}'
        lexer.input(case)
        t = lexer.token()
        self.assertEqual('CBLOCK', t.type)

    def testNEWLINE(self):
        case = '''字元塊1
字元塊2
[[test.sql]]
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
        self.assertEqual('REFERENCE', t.type)
        self.assertEqual('test.sql', t.value.refname)

    def testPARA(self):
        case = '字元塊[[test.sql]]__強調詞__'
        lexer.input(case)
        t = lexer.token()
        self.assertEqual(1, t.lexer.lineno)
        self.assertEqual('CBLOCK', t.type)
        self.assertEqual('字元塊', t.value)

        t = lexer.token()
        self.assertEqual(1, t.lexer.lineno)
        self.assertEqual('REFERENCE', t.type)
        self.assertEqual('test.sql', t.value.refname)

        t = lexer.token()
        self.assertEqual('EMPHASIS', t.type)
        self.assertEqual('強調詞', t.value.value)

    def testReference(self):
        case = '''字元塊1
字元塊2
[[test.sql]][[label:name:type]][[name:type]]
'''
        d = inliner.parse(case)
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

    def testCblock(self):           
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

        case = '''字元塊1
字元塊2_3_4_5
[test.sql][label:name:type][name:type]
'''
        d = inliner.parse(case)
        self.assertEqual('para', d.type)
        self.assertEqual(1, len(d.children))
        cb = d.children[0]                                    
        self.assertEqual('cblock', cb.type)
        self.assertEqual(case.replace('\n',''), cb.value)

        case = '''字元塊1''[[test.sql]]''
字元塊2
'''
        d = inliner.parse(case)
        self.assertEqual('para', d.type)
        self.assertEqual(1, len(d.children))
        cb = d.children[0]                                    
        self.assertEqual('cblock', cb.type)
        self.assertEqual('字元塊1[[test.sql]]字元塊2', cb.value)

if __name__ == '__main__':
    unittest.main()
    '''tests = unittest.TestSuite()
    tests.addTest(UnitTest("testREFERENCE"))
    tests.addTest(UnitTest("testCBLOCK"))
    tests.addTest(UnitTest("testPARA"))
    tests.addTest(UnitTest("testReference"))
    tests.addTest(UnitTest("testNEWLINE"))
    runner = unittest.TextTestRunner()
    runner.run(tests)'''
