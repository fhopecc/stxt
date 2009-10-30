# coding=utf8
import sys, lex
from stxt_tree import DocTreeNode
import stxt_tb_parser, logger
# Lexer
tokens = [
#         'INCLUDE', 
          'HEAD1', 
          'HEAD2', 
          'HEAD3', 
          'CODEHEAD', 
          'CODEBLOCK', 
          'TABLEHEAD', 
          'TABLEBLOCK', 
          'IMAGEHEAD', 
          'DEFINE', 
          'THEOREM', 
          'PROOF', 
          'QUESTION', 
          'ANSWER', 
          'FOOTNOTE', 
          'LI',            # list start
          #'L2LI',         # level2 list start
          'OL', 
          #'L2OL', 
          'DL', 
          'EMPTYLINE', 
          #'L3LINE', 
          'L2LINE', 
          'LINE'] 

def find_column(input,token):
    last_cr = input.rfind('\n',0,token.lexpos)
    if last_cr < 0:
	last_cr = 0
    column = (token.lexpos - last_cr) + 1
    return column

def t_INCLUDE(t):
    r'^<(?P<file>.*)>(\n|$)'
    t.lexer.lineno += t.lexeme.count('\n')
    t.lexer.include_lexer = t.lexer.clone()
    column = find_column(t.lexer.lexdata, t)
    file = t.lexer.lexmatch.group('file')
    try:
        t.lexer.include_lexer.read(file)
    except IOError:
        raise IOError("(%s:%i:%i): include file %s doesn't exist" % \
               (t.lexer.file, t.lexer.lineno, column, file))
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

def t_THEOREM(t):
    r'^theorem(\[(?P<name>.*)\])?\.(?P<title>.*)(\n|$)'
    t.lexer.lineno += t.lexeme.count('\n')
    t.value = DocTreeNode('theorem') 
    m = t.lexer.lexmatch
    t.value.name = m.group('name')
    t.value.title = m.group('title')
    return t

def t_DEFINE(t):
    r'^define(\[(?P<name>.*)\])?\.(?P<title>.*)(\n|$)'
    t.lexer.lineno += t.lexeme.count('\n')
    t.value = DocTreeNode('define') 
    m = t.lexer.lexmatch
    t.value.name = m.group('name')
    t.value.title = m.group('title')
    return t

def t_PROOF(t):
    r'^proof\.(\n|$)'
    t.lexer.lineno += t.lexeme.count('\n')
    t.value = DocTreeNode('proof') 
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
    logger.info('%s:%s:%s encounter illegal character [%s]' % (
                t.lexer.file, t.lexer.lineno, 
                find_column(t.lexer.lexdata, t), t.value[0]
               ))
#t.lexer.skip(1)
    sys.exit(1)
#lexer = lex.lex(debug=True)
lexer = lex.lex()

import unittest
class UnitTest(unittest.TestCase):
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
