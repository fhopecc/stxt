# coding=utf8
import sys, lex, unittest
from stxt_tree import DocTreeNode
import stxt_tb_parser
# Lexer
tokens = [
          'INCLUDE', 
          'HEAD1', 
          'HEAD2', 
          'HEAD3', 
          'CODEHEAD', 
          'CODEBLOCK', 
          'TABLEHEAD', 
          'TABLEBLOCK', 
          'IMAGEHEAD', 
          'QUESTION', 
          'ANSWER', 
          'FOOTNOTE', 
          'LI',           # list start
          #'L2LI',         # level2 list start
          'OL', 
          #'L2OL', 
          'DL', 
          'EMPTYLINE', 
          #'L3LINE', 
          'L2LINE', 
          'LINE'] 
def t_INCLUDE(t):
    r'^<(?P<file>.*)>\n'
    t.lexer.lineno += t.lexeme.count('\n')
    t.lexer.include_lexer = t.lexer.clone()
    t.lexer.include_lexer.read(t.lexer.lexmatch.group('file'))
    return t.lexer.include_lexer.token()

def t_HEAD1(t):
    r'^(\[(?P<name>.*)\])?(?P<title>.*)\n=+\n'
    t.lexer.lineno += t.lexeme.count('\n')
    t.value = DocTreeNode('sect1') 
    m = t.lexer.lexmatch
    t.value.name = m.group('name')
    t.value.title = m.group('title')
    return t

def t_HEAD2(t):
    r'^(\[(?P<name>.*)\])?(?P<title>.*)\n-+\n'
    t.lexer.lineno += t.lexeme.count('\n')
    t.value = DocTreeNode('sect2') 
    m = t.lexer.lexmatch
    t.value.name = m.group('name')
    t.value.title = m.group('title')
    return t

def t_HEAD3(t):
    r'^(\[(?P<name>.*)\])?(?P<title>.*)\n~+\n'
    t.lexer.lineno += t.lexeme.count('\n')
    t.value = DocTreeNode('sect3') 
    m = t.lexer.lexmatch
    t.value.name = m.group('name')
    t.value.title = m.group('title')
    return t

def t_IMAGEHEAD(t):
    r'^image(\[(?P<name>.*)\])?\.(?P<title>.*)(\n|$)'
    t.lexer.lineno += t.lexeme.count('\n')
    t.value = DocTreeNode('image') 
    m = t.lexer.lexmatch
    t.value.name = m.group('name')
    t.value.title = m.group('title')
    return t

def t_QUESTION(t):
    r'^question(\[(?P<name>.*)\])?\.(?P<title>.*)(\n|$)'
    t.lexer.lineno += t.lexeme.count('\n')
    t.value = DocTreeNode('question') 
    m = t.lexer.lexmatch
    t.value.name = m.group('name')
    t.value.title = m.group('title')
    return t

def t_ANSWER(t):
    r'^answer\.(\n|$)'
    t.lexer.lineno += t.lexeme.count('\n')
    t.value = DocTreeNode('answer') 
    return t

def t_TABLEHEAD(t):
    r'^table(\[(?P<name>.*)\])?\.(?P<title>.*)\n'
    t.lexer.lineno += t.lexeme.count('\n')
    t.value = DocTreeNode('table') 
    m = t.lexer.lexmatch
    t.value.name = m.group('name')
    t.value.title = m.group('title')
    return t

def t_TABLEBLOCK(t):
    r'(.+\n)+=[= ]+\n'
    t.lexer.lineno += t.lexeme.count('\n')
    m = t.lexer.lexmatch
    try:                                        
        t.value = stxt_tb_parser.parse(m.group(0).decode('utf8'))
    except SyntaxError:
        print >>sys.stderr, "SyntaxError:" + str(t)
        t.value = m.group(0)
    return t

def t_CODEHEAD(t):
    r'^code(\[(?P<name>.*)\])?\.(?P<title>.*)\n'
    t.lexer.lineno += t.lexeme.count('\n')
    t.value = DocTreeNode('code') 
    m = t.lexer.lexmatch
    t.value.name = m.group('name')
    t.value.title = m.group('title')
    return t

def t_CODEBLOCK(t):
    r'(?P<code>(.+\n)+)(^::\n)'
    t.lexer.lineno += t.lexeme.count('\n')
    m = t.lexer.lexmatch
    t.value = m.group('code')
    return t

def t_OL(t):
    r'# (?P<content>.*)\n'
    t.lexer.lineno += t.lexeme.count('\n')
    t.value = t.lexer.lexmatch.group('content')
    t.value = DocTreeNode('olistitem', t.value)
    return t
def t_LI(t):
    r'\* (?P<content>.*)\n'
    t.lexer.lineno += t.lexeme.count('\n')
    t.value = t.lexer.lexmatch.group('content')
    t.value = DocTreeNode('listitem', t.value)
    return t
def t_DL(t):
    r'^(?P<content>(?!(table|code|# |\* |  )).+)\n  ((?P<line>.*)\n)'
    t.lexer.lineno += t.lexeme.count('\n')
    t.value = t.lexer.lexmatch.group('content')
    t.value = DocTreeNode('dlistitem', t.value)
    line = t.lexer.lexmatch.group('line')
    if line:
        t.value.append(DocTreeNode('l2para', line))
    return t
#def t_L2OL(t):
#    r'    # (?P<content>.*)\n'
#    t.lexer.lineno += t.lexeme.count('\n')
#    t.value = t.lexer.lexmatch.group('content')
#    t.value = DocTreeNode('olistitem', t.value)
#    return t
#def t_L2LI(t):
#    r'  \* (?P<content>.*)\n'
#    t.lexer.lineno += t.lexeme.count('\n')
#  t.value = t.lexer.lexmatch.group('content')
#    t.value = DocTreeNode('listitem', t.value)
#    return t
def t_FOOTNOTE(t):
    r'^\.\. \[#] (?P<content>.+)\n'
    t.lexer.lineno += t.lexeme.count('\n')
    t.value = DocTreeNode('FOOTNOTE', 
                          t.lexer.lexmatch.group('content'))
    return t

def t_EMPTYLINE(t):
    r'^\n'
    t.lexer.lineno += t.lexeme.count('\n')
    return t

def t_L2LINE(t):
    r'^  (?P<content>.*)\n'
    t.lexer.lineno += t.lexeme.count('\n')
    t.value = t.lexer.lexmatch.group('content')
    return t

def t_LINE(t):
    r'^[^ ](.+)\n'
    t.lexer.lineno += t.lexeme.count('\n')
    return t

def t_error(t):
    print >> sys.stderr, "Lexer Error:" + \
                 str(t).decode('utf8').encode('cp950')
    sys.exit()
#lexer = lex.lex(debug=True)
lexer = lex.lex()

class UnitTest(unittest.TestCase):
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
        self.assertEqual(tok.type, 'QUESTION')
        self.assertEqual(tok.value.name, 'name')
        self.assertEqual(tok.value.title, 'this is a question title')

    def testANSWER(self):
        case = 'answer.'
        lexer.input(case)
        tok = lexer.token()
        self.assertEqual(tok.type, 'ANSWER')

    def testTABLEBLOCK(self):
        testcase = '''時間 交易A       交易B
==== =========== ===========
t1   A.read(p)    
t2   A.update(p)  
t3               B.read(p)   
t4               B.update(p)
==== =========== ===========
'''
        lexer.input(testcase)     
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
        lexer.input(testcase)     
        d = lexer.token()
        d = lexer.token()
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
