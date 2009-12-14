# coding=utf8
import os, sys, unittest
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from lib.stxt_parser import parser

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
        book = parser.parse(case)
        self.assertEqual(book.type, 'book')
        questions = book.children[0]
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
        book = parser.parse(case)
        self.assertEqual(book.type, 'book')
        questions = book.children[0]
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
        book = parser.parse(case)
        self.assertEqual(book.type, 'book')
        theorem = book.children[0]
        self.assertEqual(theorem.type, 'theorem')
        self.assertEqual(theorem.name, 'reflective')
        self.assertEqual(theorem.title, '反身性規則')
        self.assertEqual(1, len(theorem.children))
        para1 = theorem.children[0]
        self.assertEqual(para1.type, 'para')

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
