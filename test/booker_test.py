# coding=utf8
import os, sys, unittest
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from lib.booker_lexer import lexer
from lib import booker
from lib.lex3 import LexError

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

    def testList(self):
        case = '''衍生運算子包含：
* 交集
* 合併
* 除法
'''
        doc = booker.parse(case)
        self.assertEqual(doc.type, 'doc')
        para = doc.children[0]
        self.assertEqual('para', para.type)
        self.assertEqual('衍生運算子包含：', para.value)

        list = doc.children[1]
        self.assertEqual('list', list.type)
        self.assertEqual(3, len(list.children))
        l1 = list.children[0]
        self.assertEqual('交集', l1.value)

        case = '''在上述條件下，若用戶端與伺服端要相互認證，其步驟如下：

2.用戶端首先必須與 KDC 進行相互認證的工作，
  此一工作即是使用一對一認證。
#.用戶端與 KDC 在確認彼此的身份之後，
  用戶端即送出 Msg1 訊息給 KDC：

  Msg1：“用戶端要與伺服端進行認證”
Msg1：“用戶端要與伺服端進行認證”
'''
        doc = booker.parse(case)

        list = doc.children[1]
        self.assertEqual(2, len(list.children))
        l1 = list.children[0]
        self.assertEqual('olistitem', l1.type)
        self.assertEqual(2, l1.number)
        l2 = list.children[1]
        self.assertEqual('用戶端與 KDC 在確認彼此的身份之後，' \
                         '用戶端即送出 Msg1 訊息給 KDC：', 
                         l2.children[0].value)
        self.assertEqual(2, len(l2.children))
        l2p = l2.children[1]
        self.assertEqual('Msg1：“用戶端要與伺服端進行認證”', l2p.value)

    def testMList(self): 
        case='''#.安全性方法(IPsec  Security Methods)：
  #.高(High)：使用ESP協定。
    * 何謂 ESP

      是吧
      
      多層
      * 果然為 ESP
        #.其實還滿簡單的
          * OK啦
            這是真的，
            多層的
  #.中(Medium)：使用AH協定。
  #.自訂(Custom)：若要同時使用AH或ESP，可在此自訂安全性方法。
'''
        doc = booker.parse(case)

        list = doc.children[0]
        l1 = list.children[0]
        self.assertEqual('安全性方法(IPsec  Security Methods)：' , 
                          l1.value)
        self.assertEqual(2, len(l1.children))

        l1list = l1.children[1]
        l1l1 = l1list.children[0]
        self.assertEqual('高(High)：使用ESP協定。' , 
                          l1l1.value)
        self.assertEqual(2, len(l1l1.children))

        l2list = l1l1.children[1]
        self.assertEqual('list', l2list.type)

        l2l1 = l2list.children[0]
        self.assertEqual(4, len(l2l1.children))

        para = l2l1.children[0]
        self.assertEqual('para', para.type)

        para = l2l1.children[1]
        self.assertEqual('para', para.type)

        l3list = l2l1.children[3]
        self.assertEqual('list', l3list.type)

        l3l1 = l3list.children[0]
        self.assertEqual(2, len(l3l1.children))

        l4list = l3l1.children[1]
        self.assertEqual('olist', l4list.type)

        l4l1 = l4list.children[0]
        self.assertEqual(2, len(l4l1.children))
 
        l5list = l4l1.children[1]
        self.assertEqual('list', l5list.type)

        l5l1 = l5list.children[0]
        self.assertEqual(1, len(l5l1.children))
        self.assertEqual('OK啦這是真的，多層的',
                          l5l1.children[0].value)
        
    def testTheorem(self):
        case ='''theorem[reflective].反身性規則
  若 a 是一個欄位集，且 a 包含 b，則 a → b。
'''
        doc = booker.parse(case)
        theorem = doc.children[0]
        self.assertEqual(theorem.type, 'theorem')
        self.assertEqual(theorem.name, 'reflective')
        self.assertEqual(theorem.title, '反身性規則')
        self.assertEqual(1, len(theorem.children))
        l1para = theorem.children[0]
        self.assertEqual('para', l1para.type)

        case ='''theorem[decomposition].分解
  若 a → bc，則 a → b 且 a → c 。
proof.
  #.bc 包含 b，bc → b，引用[reflective]。
  #.bc 包含 c，bc → c，引用[reflective]。
  #.a → bc 且 bc → b，則 a → b，引用[transitivity]。
  #.a → bc 且 bc → c，則 a → c，引用[transitivity]。
'''
        doc = booker.parse(case)
        theorem = doc.children[0]
        self.assertEqual(theorem.type, 'theorem')
        self.assertEqual(theorem.name, 'decomposition')
        self.assertEqual(theorem.title, '分解')
        self.assertEqual(2, len(theorem.children))
        para = theorem.children[0]
        self.assertEqual(para.type, 'para')
        proof = theorem.children[1]
        self.assertEqual(proof.type, 'proof')
        self.assertEqual(len(proof.children), 1)
        l1list = proof.children[0]
        self.assertEqual(l1list.type, 'olist')
        l1l3 = l1list.children[2]
        self.assertEqual('a → bc 且 bc → b，則 a → b，引用[transitivity]。', l1l3.value)

    def testDefine(self):
        case ='''define[reflective].反身性規則
  若 a 是一個欄位集，且 a 包含 b，則 a → b。
'''
        doc = booker.parse(case)
        define = doc.children[0]
        self.assertEqual(define.type, 'define')
        self.assertEqual(define.name, 'reflective')
        self.assertEqual(define.title, '反身性規則')
        self.assertEqual(1, len(define.children))
        l1para = define.children[0]
        self.assertEqual(l1para.type, 'para')

    def testQuestion(self):
        case ='''question[96h2-3].96高2-3
  在關聯式資料庫的綱要(Schema)中，
  有鍵值限制(Key Constraint)、個體整合限制(Entity Integrity Constraint)
  以及參考整合限制(Referential Integrity Constraint)三種，試分別說明之。(30 分)
answer.
  鍵值限制請參閱[key_constraint]。

  個體整合限制請參閱[entity_integrity]。
'''
        doc = booker.parse(case)
        question = doc.children[0]
        self.assertEqual(question.type, 'question')
        self.assertEqual(question.name, '96h2-3')
        self.assertEqual(question.title, '96高2-3')
        self.assertEqual(2, len(question.children))
        para = question.children[0]
        self.assertEqual(para.type, 'para')
        answer = question.children[1]
        self.assertEqual(answer.type, 'answer')
        self.assertEqual(2, len(answer.children))
        ap = answer.children[0]
        self.assertEqual(ap.type, 'para')

    def testCode(self):
        case = '''code[dep_id_not_unique.sql].非單人科室員工名單
select name 
from employees
where dep_id in (select dep_id
                 from employees 
                 group by dep_id 
                 having count(*) > 1)
::
'''
        doc = booker.parse(case)
        code = doc.children[0]
        self.assertEqual('code', code.type)
        self.assertEqual('dep_id_not_unique.sql', code.name)
        self.assertEqual('非單人科室員工名單', code.title)
        self.assertEqual('''select name 
from employees
where dep_id in (select dep_id
                 from employees 
                 group by dep_id 
                 having count(*) > 1)''', code.value)

    def testTable(self):
        case = '''table.交易
時間 交易A       交易B
==== =========== ===========
t1   A.read(p)
t2   A.update(p)
t3               B.read(p)
t4               B.update(p)
==== =========== ===========
'''
        doc = booker.parse(case)
        t = doc.children[0]
        self.assertEqual('table', t.type)
        self.assertEqual('交易', t.title)
        header = t.children[0]
        self.assertEqual('tr', header.type)
        th1 = header.children[0]
        self.assertEqual('th', th1.type)
        self.assertEqual(u'時間', th1.value)
        th2 = header.children[1]
        self.assertEqual(u'交易A', th2.value)

        r2 = t.children[1]
        self.assertEqual('tr', r2.type)
        td1 = r2.children[0]
        self.assertEqual('td', td1.type)
        self.assertEqual('t1', td1.value)
        td2 = r2.children[1]
        self.assertEqual('A.read(p)', td2.value)

    def testSect1(self):
        case = '''[db]簡明資料庫實務、理論及考試參考
==================================
本書源自我的私人筆記，
而我希望以嚴謹的數學方法來組織這個筆記，
所以資料庫理論都是以定義為起始，
再附帶用各種定理描述理論的結果，
而每個定理希望都能給出形式化證明。
'''
        doc = booker.parse(case)
        s1 = doc.children[0]
        self.assertEqual('sect1', s1.type)
        self.assertEqual('db', s1.name)
        self.assertEqual('簡明資料庫實務、理論及考試參考', s1.value)

        self.assertEqual(1, len(s1.children))
        
        p = s1.children[0]
        self.assertEqual('para', p.type)

    def testMSect1(self):
        case = '''資料庫管理
==========
資料庫表格空間使用與管理相關作業紀錄
------------------------------------
資料庫使用申請單（近一年）、授權（近一年）及稽核機制（近三個月）
---------------------------------------------------------------
備份策略
~~~~~~~~~~
備份策略
'''
        doc = booker.parse(case)
        s1 = doc.children[0]
        self.assertEqual('sect1', s1.type)
        self.assertEqual(2, len(s1.children))
        
        s2 = s1.children[1]
        self.assertEqual('sect2', s2.type)
        self.assertEqual(1, len(s2.children))
    
        s3 = s2.children[0]
        self.assertEqual('sect3', s3.type)
        self.assertEqual(1, len(s3.children))

        p = s3.children[0]
        self.assertEqual('para', p.type)

    def testMSect2(self):
        case = '''資料庫管理
==========
內容

資料庫表格空間使用與管理相關作業紀錄
------------------------------------
內容

內容
內容

資料庫使用申請單（近一年）、授權（近一年）及稽核機制（近三個月）
---------------------------------------------------------------
內容

備份策略
~~~~~~~~~~
備份策略
'''
        doc = booker.parse(case)
        s1 = doc.children[0]
        self.assertEqual('sect1', s1.type)
       
        p1 = s1.children[0]
        self.assertEqual('para', p1.type)

        s2 = s1.children[2]
        self.assertEqual('sect2', s2.type)

        p3 = s2.children[0]
        self.assertEqual('para', p3.type)

        s3 = s2.children[1]
        self.assertEqual('sect3', s3.type)

        p4 = s3.children[0]
        self.assertEqual('para', p4.type)

        self.assertEqual(3, len(s1.children))
        self.assertEqual(2, len(s2.children))
        self.assertEqual(1, len(s3.children))

    def testTerm(self):
        case = '''名詞解釋
  解釋內容
  第二行解釋內容

'''
        doc = booker.parse(case)
        t = doc.children[0]
        self.assertEqual('term', t.type)
        p = t.children[0]
        self.assertEqual('para', p.type)

    def testError(self):
        case = '#資料庫管理'
        self.assertRaises(LexError, booker.parse, case)
        try:
            doc = booker.parse(case)
        except LexError, e:
            expect = 'illegal char(35) "#" at __string__:1:1'
            msg = e.args[0]
            self.assertEqual(expect, msg)

        case = '''#.第一層正確條列
  #第二層錯誤條列
'''
        self.assertRaises(LexError, booker.parse, case)
        try:
            doc = booker.parse(case)
        except LexError, e:
            expect = 'illegal char(35) "#" at __string__:2:3'
            msg = e.args[0]
            self.assertEqual(expect, msg)

if __name__ == '__main__':
    unittest.main()
    tests = unittest.TestSuite()
    # TABLE parsing will failed in yacc debug mode    
    '''tests.addTest(UnitTest("testTable"))
    tests.addTest(UnitTest("testSect1"))
    tests.addTest(UnitTest("testDefine"))
    tests.addTest(UnitTest("testCode"))
    tests.addTest(UnitTest("testPara"))
    tests.addTest(UnitTest("testTheorem"))
    tests.addTest(UnitTest("testQuestion"))
    tests.addTest(UnitTest("testList"))
    tests.addTest(UnitTest("testMList"))
    tests.addTest(UnitTest("testMSect1"))
    tests.addTest(UnitTest("testMSect2"))'''
    tests.addTest(UnitTest("testTerm"))
    runner = unittest.TextTestRunner()
    runner.run(tests)
