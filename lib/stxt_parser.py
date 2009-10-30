# coding=utf8
import sys, yacc, unittest
from stxt_lexer import *
# Parser
def p_book(p):
    '''book : sect1
            | book sect1
            | questions
            | content
            | book content'''
    if len(p) == 2:
        p[0] = DocTreeNode('book', '')
        p[0].append(p[1])
    else:
        p[1].append(p[2])
        p[0] = p[1]
#def p_error_sect2(p):
#    r'book : error sect2'
#    print 'sect2 cannot be directly embeded in book.'
def p_sect1(p):
    '''sect1 : HEAD1 content1s
             | HEAD1 content1s questions'''
    p[0] = p[1]
    for c in p[2]:
        p[0].append(c)
    if len(p) == 4:
        p[0].append(p[3])

def p_sect1_only_questions(p):
    r'sect1 : HEAD1 questions'
    p[0] = p[1]
    p[0].append(p[2])

def p_sect1title(p):
    'sect1 : HEAD1 EMPTYLINE'
    p[0] = p[1]

def p_content1s(p):
    '''content1s : sect2
                 | content
                 | content1s sect2
                 | content1s content'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[1].append(p[2])
        p[0] = p[1]

def p_sect2(p):
    '''sect2 : HEAD2 content2s
             | HEAD2 content2s questions'''
    p[0] = p[1]
    p[0] = p[1]
    for c in p[2]:
        p[0].append(c)
    if len(p) == 4:
        p[0].append(p[3])

def p_sect2_only_questions(p):
    r'sect2 : HEAD2 questions'
    p[0] = p[1]
    p[0].append(p[2])

def p_sect2title(p):
    'sect2 : HEAD2 EMPTYLINE'
    p[0] = p[1]

def p_content2s(p):
    '''content2s : sect3
                 | content
                 | content2s sect3
                 | content2s content'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[1].append(p[2])
        p[0] = p[1]

def p_sect3(p):
    r'sect3 : HEAD3 contents'
    p[0] = p[1]
    for c in p[2]:
        p[0].append(c)

def p_contents(p):
    '''contents : content
                | contents content'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[1].append(p[2])
        p[0] = p[1]

def p_content(p):
    '''content : para
               | para EMPTYLINE
               | code
               | code EMPTYLINE
               | table
               | table EMPTYLINE
               | image
               | image EMPTYLINE
               | list
               | list EMPTYLINE
               | footnotes EMPTYLINE
               | theorem
               | define
    '''
    p[0] = p[1]

def p_dlisthead(p):
    '''dlisthead : DL
                 | DL l2para
    '''
    p[0] = p[1]
    if len(p) == 3 and p[2]:
        l2p = p[0].children[0]
        l2p.value = l2p.value + p[2].value

def p_listhead(p):
    '''listhead : LI
                | OL
                | dlisthead
                | LI l2para
                | OL l2para
    '''
    p[0] = p[1]
    if len(p) == 3 and p[2]:
        p[0].value += p[2].value

def p_listitem(p):
    '''listitem : listhead
                | listitem EMPTYLINE
                | listitem EMPTYLINE l2para
    '''
    p[0] = p[1]
    if len(p) == 4 and p[3]:
        #for l2para in p[2]:
        #p[0].append(l2para)
        p[0].append(p[3])

def p_list(p):
    '''list : listitem
            | list listitem
    '''
    if len(p) == 2:
        if p[1].type == 'listitem':
            p[0] = DocTreeNode('list', '')
        elif p[1].type == 'olistitem':
            p[0] = DocTreeNode('olist', '')
        elif p[1].type == 'dlistitem':
            p[0] = DocTreeNode('dlist', '')
        p[0].append(p[1])
    else:
        p[0] = p[1].append(p[2])

def p_code(p):
    r'code : CODEHEAD CODEBLOCK'
    p[0] = p[1]
    p[0].value = p[2]

def p_image(p):
    r'image : IMAGEHEAD'
    p[0] = p[1]

def p_para(p):
    '''para : LINE
          | para LINE
    '''
    if len(p) == 2:
        p[0] = DocTreeNode('para', p[1])
    else:
        if isinstance(p[1], str):
            p[1] = DocTreeNode('para', p[1])
        p[1].value +=    p[2]
        p[0] = p[1]

def p_l2para(p):
    '''l2para : L2LINE
            | l2para L2LINE
    '''
    if len(p) == 2:
        p[0] = DocTreeNode('l2para', p[1])
    else:
        p[1].value += p[2]
        p[0] = p[1]
#def p_l2paras(p): # nested 2 level paragraph
#  '''l2paras : l2para
#             | l2para EMPTYLINE
#             | l2paras l2para EMPTYLINE
#  '''
#  if len(p) in (2, 3):
#    p[0] = [p[1]]
#  elif len(p) == 4:
#    p[1].append(p[2])
#    p[0] = p[1]
def p_table(p): # nested 2 level paragraph
    '''table : TABLEHEAD TABLEBLOCK'''
    #p[1].value = p[2]
    if type(p[2]) == str:
        p[1].value = p[2]
        p[0] = p[1]
    else:
        p[2].title = p[1].title
        p[2].name = p[1].name
        p[0] = p[2]

def p_footnotes(p):
    '''footnotes : FOOTNOTE
                 | footnotes FOOTNOTE'''
    if len(p) == 2:
        p[0] = DocTreeNode('footnotes', '')
        p[0].append(p[1])
    else:
        p[1].append(p[2])
        p[0] = p[1]

def p_question(p):
    r'question : QUESTION contents'
    p[0] = p[1]
    for c in p[2]:
        p[0].append(c)

def p_question_with_answer(p):
    r'question : QUESTION contents ANSWER contents'
    p[0] = p[1]
    for c in p[2]:
        p[0].append(c)
    for c in p[4]:
        p[3].append(c)
    p[0].append(p[3])

def p_questions(p):
    '''questions : question
                 | questions question'''
    if len(p) == 2:
        p[0] = DocTreeNode('questions', '')
        p[0].append(p[1])
    else:
        p[1].append(p[2])
        p[0] = p[1]

def p_theorem(p):
    r'theorem : THEOREM contents'
    p[0] = p[1]
    for c in p[2]:
        p[0].append(c)

def p_theorem_with_proof(p):
    r'theorem : THEOREM contents PROOF contents'
    p[0] = p[1]
    for c in p[2]:
        p[0].append(c)
    for c in p[4]:
        p[3].append(c)
    p[0].append(p[3])

def p_define(p):
    r'define : DEFINE contents'
    p[0] = p[1]
    for c in p[2]:
        p[0].append(c)

def p_error(t):
    print ("Parse Error:\n%s" \
                    % str(t)).decode('utf8').encode('cp950')

parser = yacc.yacc()

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

    def testTableSubparserError(self):
        testcase = '''table.交易
時間 交易A       交易B
==== =========== ===========ddd
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

        block = '''時間 交易A       交易B
==== =========== ===========ddd
t1   A.read(p)
t2   A.update(p)
t3               B.read(p)
t4               B.update(p)
==== =========== ===========
'''
        self.assertEqual(block, d.value)

if __name__ == '__main__':
    unittest.main()
