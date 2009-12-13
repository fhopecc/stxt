# coding=utf8
import inliner
import sys, yacc
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
#    if p[1].type == 'para':
#        try:
#            d = inliner.parse(p[1].value)
#            p[1] = d
#        except lex.LexError, le:
#            print le.message
#            e = SyntaxError(le.message)
            #raise e
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
    sys.exit()

parser = yacc.yacc()
