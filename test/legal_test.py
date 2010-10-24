# coding=utf-8
from __future__ import with_statement
import os, sys, unittest
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
import lib.stxt.legal as legal

class UnitTest(unittest.TestCase):

    def testLexer(self):
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

    def testParser(self):
        case = '''資料交換管理程序
--------

機密等級:內部使用
文件編號:PHG-ISMS-03-011
版次:1.0
發行日期:97年11月4日
權責單位:計畫室

3.權責

3.1.網路管理者

依據「PHG-ISMS-04-030系統權限申請單」申請，
開放適當權限。
        
3.1.1.

合作廠商或各專案相關外部人員如有資訊設備連線、存取之需求，
須由專案承辦人填寫「PHG-ISMS-04-030系統權限申請單」，
經權責主管核准後，由相關網路管理者執行權限開放。

狂合作廠商或各專案相關外部人員如有資訊設備連線、存取之需求，
須由專案承辦人填寫「PHG-ISMS-04-030系統權限申請單」，
經權責主管核准後，由相關網路管理者執行權限開放。

'''

        tokens = legal.Lexer().tokenize(case)

        doc = legal.Parser().parse(tokens)
        
        self.assertEqual('doc', doc.type)
        self.assertEqual('資料交換管理程序', doc.title)

        self.assertEqual('內部使用', doc.attrs['機密等級'])

        self.assertEqual(3, len(doc.kids()))
        
        sect = doc[0]
        self.assertEqual('sect', sect.type)

        self.assertEqual(1, len(sect.kids()))

        para = sect[0]
        self.assertEqual('para', para.type)
        self.assertEqual('權責', para.value)


if __name__ == '__main__':
    '''unittest.main()'''
    tests = unittest.TestSuite()
    tests.addTest(UnitTest("testLexer"))
    tests.addTest(UnitTest("testParser"))
    #tests.debug()
    runner = unittest.TextTestRunner()
    runner.run(tests)
