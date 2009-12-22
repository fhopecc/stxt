# coding=utf8
import os, sys, unittest
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from lib.booker import parser


class UnitTest(unittest.TestCase):
    def testIMAGEHEAD(self):
        case = 'image[name].this is a image title'
        book = parser.parse(case)
        self.assertEqual(book.type, 'book')
        self.assertEqual(book.children[0].type, 'image')
        self.assertEqual(book.children[0].name, 'name')
        self.assertEqual(book.children[0].title, 'this is a image title')

        # imagehead must having name block
        #case = 'image.this is a image title'
        #lexer.input(case)
        #tok = lexer.token()
        #self.assertEqual(tok.type, 'IMAGEHEAD')
        #self.assertEqual(tok.value.name, None)
        #self.assertEqual(tok.value.title, 'this is a image title')

    def testDefine(self):
        case ='''define[reflective].反身性規則
若 a 是一個欄位集，且 a 包含 b，則 a → b。
'''
        book = parser.parse(case)
        self.assertEqual(book.type, 'book')
        theorem = book.children[0]
        self.assertEqual(theorem.type, 'define')
        self.assertEqual(theorem.name, 'reflective')
        self.assertEqual(theorem.title, '反身性規則')
        self.assertEqual(1, len(theorem.children))
        para1 = theorem.children[0]
        self.assertEqual(para1.type, 'para')

    def testTheoremWithAnswer(self):
        case ='''theorem[decomposition].分解
若 a → bc，則 a → b 且 a → c 。
proof.
* bc 包含 b，bc → b，引用<%=xref 'reflective'%>。
* bc 包含 c，bc → c，引用<%=xref 'reflective'%>。
* a → bc 且 bc → b，則 a → b，引用<%=xref 'transitivity'%>。
* a → bc 且 bc → c，則 a → c，引用<%=xref 'transitivity'%>。
'''
        book = parser.parse(case)
        self.assertEqual(book.type, 'book')
        theorem = book.children[0]
        self.assertEqual(theorem.type, 'theorem')
        self.assertEqual(theorem.name, 'decomposition')
        self.assertEqual(theorem.title, '分解')
        self.assertEqual(2, len(theorem.children))
        para = theorem.children[0]
        self.assertEqual(para.type, 'para')
        proof = theorem.children[1]
        self.assertEqual(proof.type, 'proof')
        self.assertEqual(len(proof.children), 1)
        para1 = proof.children[0]
        self.assertEqual(para1.type, 'list')

    def testTable(self):
        testcase = '''table.交易
時間 交易A       交易B
==== =========== ===========
t1   A.read(p)
t2   A.update(p)
t3               B.read(p)
t4               B.update(p)
==== =========== ===========
'''
        book = parser.parse(testcase)
        self.assertEqual(book.type, 'book')
        d = book.children[0]
        self.assertEqual('table', d.type)
        self.assertEqual('交易', d.title)
        header = d.children[0]
        self.assertEqual('tr', header.type)
        th1 = header.children[0]
        self.assertEqual('th', th1.type)
        self.assertEqual(u'時間', th1.value)
        th2 = header.children[1]
        self.assertEqual(u'交易A', th2.value)

        r2 = d.children[1]
        self.assertEqual('tr', r2.type)
        td1 = r2.children[0]
        self.assertEqual('td', td1.type)
        self.assertEqual('t1', td1.value)
        td2 = r2.children[1]
        self.assertEqual('A.read(p)', td2.value)

    def testInliner(self):
        case = '''字元塊1
字元塊2
[test.sql]
'''
        d = parser.parse(case)
        self.assertEqual('book', d.type)
        self.assertEqual(1, len(d.children))

        p = d.children[0]
        self.assertEqual('para', p.type)
        self.assertEqual(2, len(p.children))

        cb = p.children[0]
        self.assertEqual('cblock', cb.type)
        self.assertEqual('字元塊1字元塊2', cb.value)

        ref = p.children[1]
        self.assertEqual('reference', ref.type)
        self.assertEqual('test.sql', ref.value)

#    def testTableSubparserError(self):
#        testcase = '''table.交易
#時間 交易A       交易B
#==== =========== ===========ddd
#t1   A.read(p)
#t2   A.update(p)
#t3               B.read(p)
#t4               B.update(p)
#==== =========== ===========
#'''
#        book = parser.parse(testcase)
#        self.assertEqual(book.type, 'book')
#        d = book.children[0]
#        self.assertEqual('table', d.type)
#        self.assertEqual('交易', d.title)

#        block = '''時間 交易A       交易B
#==== =========== ===========ddd
#t1   A.read(p)
#t2   A.update(p)
#t3               B.read(p)
#t4               B.update(p)
#==== =========== ===========
#'''
#        self.assertEqual(block, d.value)

if __name__ == '__main__':
    unittest.main()
#    tests = unittest.TestSuite()
#    tests.addTest(UnitTest("testInliner"))
#    runner = unittest.TextTestRunner()
#    runner.run(tests)
