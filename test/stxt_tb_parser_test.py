# coding=utf8
import os, sys, unittest
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from lib.stxt_tb_parser import parser
from lib.stxt_tb_parser import lexer
from lib.stxt_tb_parser import parse_row
class ParserTest(unittest.TestCase):
    def testParseRow(self):
        testcase = u'==== =========== ===========\n'
        lexer.input(testcase)
        tok = lexer.token()
        cols = tok.value
        vals = parse_row(cols, u't3               B.read(p)')
        self.assertEqual(u't3', vals[0])
        self.assertEqual(u'', vals[1])
        self.assertEqual(u'B.read(p)', vals[2])

    def testSingleRowTable(self):
        case = '''name
======
金叔分
曹晶蓮
李美紅
======
'''
        t = parser.parse(case.decode('utf8'))
        self.assertEqual('table', t.type)
        header = t.children[0]
        self.assertEqual('tr', header.type)
        th1 = header.children[0]
        self.assertEqual('th', th1.type)
        self.assertEqual(u'name', th1.value)

        r2 = t.children[1]
        self.assertEqual('tr', r2.type)
        td1 = r2.children[0]
        self.assertEqual('td', td1.type)
        self.assertEqual(u'金叔分', td1.value)

        self.assertEqual(4, len(t.children))


    def testParser(self):
        testcase = '''時間 交易A       交易B
==== =========== ===========
t1     A.read(p)
t2   A.update(p)
t3               B.read(p)
t4               B.update(p)
==== =========== ===========
'''
        d = parser.parse(testcase.decode('utf8'))
        self.assertEqual('table', d.type)
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

    def testParseError(self):
        testcase = '''時間 交易A       交易B
==== =========== ===========中文'''
        self.assertRaises(SyntaxError, parser.parse, testcase.decode('utf8'))

if __name__ == '__main__':
    #unittest.main()
