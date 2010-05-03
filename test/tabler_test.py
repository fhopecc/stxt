# coding=utf8
from __future__ import with_statement
import os, sys, unittest
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from lib.stxt import tabler
from lib.stxt import inliner
    
class UnitTest(unittest.TestCase):
    def testSimpleTable(self):
        case = '''時間 交易A       交易B
==== =========== ===========
t1   A.read(p)
t2   A.update(p)
t3               B.read(p)
t4               B.update(p)
==== =========== ===========
'''
        doc = tabler.parse(case.decode('utf8'))
        header = doc.children[0]
        self.assertEqual('tr', header.type)
        th1 = header.children[0]
        self.assertEqual('th', th1.type)
        self.assertEqual(u'時間', th1.value)

        th = header.children[1]
        self.assertEqual(u'交易A', th.value)

        p = th[0]
        self.assertEqual('para', p.type)
        self.assertEqual(u'交易A', p.value)

        r2 = doc.children[1]
        self.assertEqual('tr', r2.type)

        td = r2.children[0]
        self.assertEqual('td', td.type)
        self.assertEqual('t1', td.value)


        td = r2.children[1]
        self.assertEqual('A.read(p)', td.value)

        p = td[0]
        self.assertEqual('para', p.type)
        self.assertEqual('A.read(p)', p.value)


if __name__ == '__main__':
    #unittest.main()
    tests = unittest.TestSuite()
    tests.addTest(UnitTest("testSimpleTable"))
    runner = unittest.TextTestRunner()
    runner.run(tests)
    #tests.debug()
