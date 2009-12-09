# coding=utf8
import sys, lex
from stxt_tree import DocTreeNode
import stxt_tb_parser, logger
# Lexer
states = (
    ('table', 'exclusive'),
    ('code', 'exclusive'),
)

tokens = [
          'CBLOCK', 
          'EMPHASIS', 
          'STRONG', 
          'REFERENCE', 
          'HYPERLINK' 
         ] 

def find_column(input,token):
    last_cr = input.rfind('\n',0,token.lexpos)
    if last_cr < 0:
	last_cr = 0
    column = (token.lexpos - last_cr) + 1
    return column

def t_INSERT(t):
    r'^(table|image)[(?P<name>[^\]]*)](\n|$)'
    t.lexer.lineno += t.lexeme.count('\n')

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
    if len(t.value.title) < 1:
        t.value.title = t.value.name
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

def t_table(t):
    r'^table(\[(?P<name>.*)\])?\.(?P<title>.*)\n'
    t.lexer.lineno += t.lexeme.count('\n')
    t.value = DocTreeNode('table') 
    m = t.lexer.lexmatch
    t.type = 'TABLEHEAD'
    t.value.name = m.group('name')
    t.value.title = m.group('title')
    if len(t.value.title) < 1:
        t.value.title = t.value.name
    t.lexer.block_start = t.lexer.lexpos# + len(m.group(0))
    t.lexer.read_head_sep = False
    t.lexer.table_block_lineno = t.lexer.lineno
    t.lexer.begin('table')
    return t

def t_table_sep(t):
    r'=[= ]*'
    if t.lexer.read_head_sep:
        m = t.lexer.lexmatch
        block_end = t.lexer.lexpos #+ len(m.group(0))
        t.block = t.lexer.lexdata[t.lexer.block_start:block_end]
        try:
            t.value = stxt_tb_parser.parse(t.block.decode('utf8'))
        except SyntaxError:
            logger.info('state(table)%s:%s:%s:%s illegal char [%s] block is [%s]' % (
                t.lexer.file, t.lexer.lineno, 
                find_column(t.lexer.lexdata, t), t.lexer.lexpos, \
                t.lexer.lexdata[t.lexpos], t.block
               ))
        except UnicodeDecodeError:
            print >>sys.stderr, "DecodeError:" + t.value
            #sys.exit(1)
        t.lexer.begin('INITIAL') 
        t.type = 'TABLEBLOCK'
        t.lineno = t.lexer.table_block_lineno 
        return t
    else:
        t.lexer.read_head_sep = True

def t_table_line(t):
    r'[^\n]+'
    pass 

def t_table_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value) 

def t_table_error(t):
    logger.info('state(table)%s:%s:%s encounter illegal character [%s]' % (
                t.lexer.file, t.lexer.lineno, 
                find_column(t.lexer.lexdata, t), t.value[0]
               ))
    sys.exit(1)

def t_code(t):
    r'^code(\[(?P<name>.*)\])?\.(?P<title>.*)\n'
    t.lexer.lineno += t.lexeme.count('\n')
    t.type = 'CODEHEAD'
    t.value = DocTreeNode('code') 
    m = t.lexer.lexmatch
    t.value.name = m.group('name')
    t.value.title = m.group('title')
    if not t.value.title: 
        t.value.title = t.value.name
    t.lexer.block_start = t.lexer.lexpos# + len(m.group(0))
    t.lexer.block_start_lineno = t.lexer.lineno
    t.lexer.begin('code')
    return t

def t_code_sep(t):
    r'::\n'
    t.lexer.lineno += 1
    block_end = t.lexer.lexpos - 3
    t.block = t.lexer.lexdata[t.lexer.block_start:block_end]
    t.type = 'CODEBLOCK'
    t.value = t.block
    t.lineno = t.lexer.block_start_lineno 
    t.lexer.begin('INITIAL')
    return t

def t_code_line(t):
    r'[^\n]+'
    pass 

def t_code_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value) 
 
def t_code_error(t):
    msg = 'state(code)%s:%s:%s illegal char [%s]' % (
                t.lexer.file, t.lexer.lineno, 
                find_column(t.lexer.lexdata, t), t.value[0]
               )
    logger.info(msg)
    raise SyntaxError, msg
    
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

def t_NEWLINE(t):
    r'\n'
    t.lexer.lineno += t.lexeme.count('\n')

def t_error(t):
    logger.info('%s:%s:%s illegal char[%s]:%s' % ( \
                t.lexer.file, t.lexer.lineno, \
                find_column(t.lexer.lexdata, t), t.value[0], \
                str(ord(t.value[0]))))
    sys.exit(1)
lexer = lex.lex(debug=1)
