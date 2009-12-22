# coding=utf8
import os, sys, unittest
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from lib.booker_lexer import lexer
from lib import booker

class UnitTest(unittest.TestCase):
    def testPara(self):
        case = '''本書源自我的私人筆記，
而我希望以嚴謹的數學方法來組織這個筆記，
所以資料庫理論都是以定義為起始，
再附帶用各種定理描述理論的結果，
而每個定理希望都能給出形式化證明。
'''
        doc = booker.parse(case)
        self.assertEqual(doc.type, 'doc')
        para = doc.children[0]
        self.assertEqual('para', para.type)
        self.assertEqual(case.replace('\n', ''), para.value)

    def testQuestion(self):
        case ='''question[96p2-4].96警2-4
為確保資料庫內的資料能正確被處理，
遵循完整性法則(Integrity Rules)是有其必要的。
請回答下列各問題：

寫出實體與參考完整性法則內涵。(8 分)

DBMS 除了支援參考完整性外，還可能支援包括預設值、
檢查範圍與NULL等完整性控制。
試寫出後面三個完整性控制的主要用途，
並利用SQL CREATE TABLE statement 舉出實例。(9 分)

根據下面兩個關聯表(Crime_BK 與 Crime_Case)，
利用 SQL CREATE TABLE statement 來建立相對應的資料表，
以確保資料庫資料的完整性。(註：資料型別與長度可自訂)(8 分)

Crime_BK = (ID, CName, Gender, Birth_of_Date)

Crime_Case = (CrimeNo, CrimeType, Crime_Event_Date, ID,
              Crim_BK.ID, Crime_Case.ID)
'''
        doc = booker.parse(case)
        self.assertEqual(doc.type, 'doc')
        questions = doc.children[0]
        self.assertEqual(questions.type, 'questions')
        question = questions.children[0]
        self.assertEqual(question.type, 'question')
        self.assertEqual(question.name, '96p2-4')
        self.assertEqual(question.title, '96警2-4')
        self.assertEqual(6, len(question.children))
        para1 = question.children[0]
        self.assertEqual(para1.type, 'para')

    def testQuestionWithAnswer(self):
        case ='''question[96h2-3].96高2-3
在關聯式資料庫的綱要(Schema)中，
有鍵值限制(Key Constraint)、
個體整合限制(Entity Integrity Constraint)以及參考整合限制(Referential Integrity Constraint)三種，試分別說明之。(30 分)
answer.
鍵值限制請參閱[key_constraint]。

個體整合限制請參閱[entity_integrity]。
'''
        doc = booker.parse(case)
        self.assertEqual(doc.type, 'doc')
        questions = doc.children[0]
        self.assertEqual(questions.type, 'questions')
        question = questions.children[0]
        self.assertEqual(question.type, 'question')
        self.assertEqual(question.name, '96h2-3')
        self.assertEqual(question.title, '96高2-3')
        self.assertEqual(2, len(question.children))
        para = question.children[0]
        self.assertEqual(para.type, 'para')
        answer = question.children[1]
        self.assertEqual(answer.type, 'answer')
        self.assertEqual(2, len(answer.children))
        para1 = answer.children[0]
        self.assertEqual(para1.type, 'para')

    def testTheorem(self):
        case ='''theorem[reflective].反身性規則
若 a 是一個欄位集，且 a 包含 b，則 a → b。
'''
        doc = booker.parse(case)
        self.assertEqual('doc', doc.type)
        theorem = doc.children[0]
        self.assertEqual('theorem', theorem.type)
        self.assertEqual(theorem.name, 'reflective')
        self.assertEqual(theorem.title, '反身性規則')
        self.assertEqual(1, len(theorem.children))
        para1 = theorem.children[0]
        self.assertEqual(para1.type, 'para')

if __name__ == '__main__':
#    unittest.main()
    tests = unittest.TestSuite()
    tests.addTest(UnitTest("testPara"))
    runner = unittest.TextTestRunner()
    runner.run(tests)
