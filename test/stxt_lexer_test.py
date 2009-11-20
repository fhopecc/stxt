# coding=utf8
import os, sys, unittest
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from lib.stxt_lexer import lexer
class UnitTest(unittest.TestCase):
    def testCODEBLOCK(self):
        case = '''code[dep_id_not_unique.bnf].非單人科室員工名單
select name 
from employees
where dep_id in (select dep_id
                 from employees 
                 group by dep_id 
                 having count(*) > 1)
::
'''
        lexer.input(case)
        ch = lexer.token()
        self.assertEqual(1, ch.lineno)
        self.assertEqual('CODEHEAD', ch.type)
        self.assertEqual('dep_id_not_unique.bnf', ch.value.name)
        self.assertEqual('非單人科室員工名單', ch.value.title)
        cb = lexer.token()
        self.assertEqual(2, cb.lineno)
        self.assertEqual('CODEBLOCK', cb.type)

        case2 = '''code.建立正式機資料庫相關目錄
cd /data/eltdb/admin
mkdir ELTUD
cd ELTUD
mkdir bdump  cdump  udump
cd /data/eltdb/oradata
mkdir ELTUD
cd /data/eltbat
mkdir -p arch/ELTUD
mkdir -p oradata/ELTUD
::
# 於測試機上建立正式機資料庫相關目錄：(時間09:20)
# 登入 VERITAS 主機並切換目錄至 c:\VERITAS\NetBackup\db\altnames
code.test
mkdir -p oradata/ELTUD
::
'''
        lexer.input(case2)
        ch = lexer.token()
        self.assertEqual(1, ch.lineno)
        self.assertEqual('CODEHEAD', ch.type)
        self.assertEqual(None, ch.value.name)
        self.assertEqual('建立正式機資料庫相關目錄', 
                ch.value.title)
        cb = lexer.token()
        self.assertEqual(2, cb.lineno)
        self.assertEqual('CODEBLOCK', cb.type)
        self.assertEqual('cd ', cb.value[0:3])
        ol = lexer.token()
        self.assertEqual(12, ol.lineno)
        self.assertEqual('OL', ol.type)

    def testInclude(self):
        case = r'<d:\stxt\lib\db\sql.stx>'
        lexer.input(case)
        self.assertRaises(IOError, lexer.token)

        case = r'<test.stx>'
        lexer.input(case)
        tok = lexer.token()
        self.assertEqual(tok.type, 'HEAD1')
        tok = lexer.token()
        self.assertEqual(tok.type, 'LINE')

    def testIMAGEHEAD(self):
        case = 'image[name].this is a image title'
        lexer.input(case)
        tok = lexer.token()
        self.assertEqual(tok.type, 'IMAGEHEAD')
        self.assertEqual(tok.value.name, 'name')
        self.assertEqual(tok.value.title, 'this is a image title')

        # imagehead must having name block
        case = 'image.this is a image title'
        lexer.input(case)
        tok = lexer.token()
        self.assertEqual(tok.type, 'IMAGEHEAD')
        self.assertEqual(tok.value.name, None)
        self.assertEqual(tok.value.title, 'this is a image title')

    def testQUESTION(self):
        case = 'question[name].this is a question title'
        lexer.input(case)
        tok = lexer.token()
        self.assertEqual('QUESTION', tok.type,)
        self.assertEqual(tok.value.name, 'name')
        self.assertEqual(tok.value.title, 'this is a question title')

    def testANSWER(self):
        case = 'answer.'
        lexer.input(case)
        tok = lexer.token()
        self.assertEqual(tok.type, 'ANSWER')

    def testTHEOREM(self):
        case = 'theorem[name].this is a theorem title'
        lexer.input(case)
        tok = lexer.token()
        self.assertEqual('THEOREM', tok.type,)
        self.assertEqual(tok.value.name, 'name')
        self.assertEqual(tok.value.title, 'this is a theorem title')

    def testDEFINE(self):
        case = 'define[name].this is a define title'
        lexer.input(case)
        tok = lexer.token()
        self.assertEqual('DEFINE', tok.type,)
        self.assertEqual(tok.value.name, 'name')
        self.assertEqual(tok.value.title, 'this is a define title')

    def testPROOF(self):
        case = 'proof.'
        lexer.input(case)
        tok = lexer.token()
        self.assertEqual(tok.type, 'PROOF')

    def testTable2(self):
        case = '''table.關聯式模型架構
物件     完整性 
======== ============
值       第一標準式
型態     型態完整性
空值     空值完整性
超鍵     唯一性
候選鍵   最小性
主鍵     實體完整性  
表       鍵限制
外鍵     參考完整性
業務邏輯 第二標準式
         第三標準式
         BCNF 標準式
         第四標準式
         第五標準式
======== ============
'''
        lexer.input(case)     
        th = lexer.token()
        self.assertEqual(1, th.lineno)
        self.assertEqual(2, th.lexer.lineno)
        self.assertEqual('TABLEHEAD', th.type)
        self.assertEqual(None, th.value.name)
        self.assertEqual('關聯式模型架構', th.value.title)

        tb = lexer.token()
        self.assertEqual('TABLEBLOCK', tb.type)
        self.assertEqual(2, tb.lineno)
        self.assertEqual(17, tb.lexer.lineno)
        # len(case) - 1, the 1 specified eof char
        self.assertEqual(len(case) - 1, tb.lexer.lexpos)

        t  = tb.value
        self.assertEqual('table', t.type)
        header = t.children[0]
        self.assertEqual('tr', header.type)
        th1 = header.children[0]
        self.assertEqual('th', th1.type)
        self.assertEqual(u'物件', th1.value)

        r1 = t.children[1]
        self.assertEqual('tr', r1.type)
        td1 = r1.children[0]
        self.assertEqual('td', td1.type)
        self.assertEqual(u'值', td1.value)

    def testSingleColumnTable(self):
        case = '''table[angles].angles
name
======
金叔分
曹晶蓮
李美紅
======
'''
        lexer.input(case)     
        th = lexer.token()
        self.assertEqual(1, th.lineno)
        self.assertEqual(2, th.lexer.lineno)
        self.assertEqual('TABLEHEAD', th.type)
        self.assertEqual('angles', th.value.name)
        self.assertEqual('angles', th.value.title)

        tb = lexer.token()
        self.assertEqual('TABLEBLOCK', tb.type)
        self.assertEqual(2, tb.lineno)
        self.assertEqual(7, tb.lexer.lineno)
        # len(case) - 1, the 1 specified eof char
        self.assertEqual(len(case) - 1, tb.lexer.lexpos)

        t  = tb.value
        self.assertEqual('table', t.type)
        header = t.children[0]
        self.assertEqual('tr', header.type)
        th1 = header.children[0]
        self.assertEqual('th', th1.type)
        self.assertEqual(u'name', th1.value)

        r1 = t.children[1]
        self.assertEqual('tr', r1.type)
        td1 = r1.children[0]
        self.assertEqual('td', td1.type)
        self.assertEqual(u'金叔分', td1.value)

    def testTABLEBLOCK(self):
        case = '''table[name].title
時間 交易A       交易B
==== =========== ===========
t1   A.read(p)    
t2   A.update(p)  
t3               B.read(p)   
t4               B.update(p)
==== =========== ===========
'''
        lexer.input(case)     
        d = lexer.token()
        self.assertEqual('TABLEHEAD', d.type)
        self.assertEqual('name', d.value.name)
        self.assertEqual('title', d.value.title)
        d = lexer.token().value
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

if __name__ == '__main__':
    unittest.main()
