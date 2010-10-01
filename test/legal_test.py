# coding=utf-8
from __future__ import with_statement
import os, sys, unittest
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
import lib.stxt.legal as legal

class UnitTest(unittest.TestCase):

    def testLITERAL(self):
        case = '''資料交換管理程序
--------

機密等級:內部使用
文件編號:PHG-ISMS-03-011
版次:1.0
發行日期:97年11月4日
權責單位:計畫室

3.3.1.確保防毒機制有效運作。

防毒軟體使用不正常，或影響應用系統、主機維運，應協請防毒系統管理者處理。
防毒軟體使用不正常，或影響應用系統、主機維運，應協請防毒系統管理者處理。
'''
        tokens = legal.Lexer().tokenize(case)

        #import pdb; pdb.set_trace()
        t = tokens[0]
        self.assertEqual('line', t.type)
        self.assertEqual('資料交換管理程序', t.value)

        t = tokens[1]
        self.assertEqual('titlesep', t.type)

        t = tokens[2]
        self.assertEqual('emptyline', t.type)

        t = tokens[3]
        self.assertEqual('docattr', t.type)
        self.assertEqual('機密等級:內部使用', t.value)

        t = tokens[4]
        self.assertEqual('docattr', t.type)
        self.assertEqual('文件編號:PHG-ISMS-03-011', t.value)

        t = tokens[5]
        self.assertEqual('docattr', t.type)

        t = tokens[6]
        self.assertEqual('docattr', t.type)

        t = tokens[7]
        self.assertEqual('docattr', t.type)

        t = tokens[8]
        self.assertEqual('emptyline', t.type)

        t = tokens[9]
        self.assertEqual('secnumber', t.type)
        self.assertEqual('3.3.1.', t.value)

        t = tokens[10]
        self.assertEqual('line', t.type)
        self.assertEqual('確保防毒機制有效運作。', t.value)

        t = tokens[11]
        self.assertEqual('emptyline', t.type)

        t = tokens[12]
        self.assertEqual('line', t.type)
        self.assertEqual('防毒軟體使用不正常，或影響應用系統、主機維運，應協請防毒系統管理者處理。', t.value)

        t = tokens[13]
        self.assertEqual('line', t.type)
        self.assertEqual('防毒軟體使用不正常，或影響應用系統、主機維運，應協請防毒系統管理者處理。', t.value)


if __name__ == '__main__':
    '''unittest.main()'''
    tests = unittest.TestSuite()
    tests.addTest(UnitTest("testLITERAL"))
    #tests.debug()
    runner = unittest.TextTestRunner()
    runner.run(tests)
