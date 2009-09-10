# coding=utf8 
import sys, yacc
from stxt_lexer import *
# Parser 
def p_book(p):
  '''book : sect1          
          | book sect1'''
  if len(p) == 2:
    p[0] = DocTreeNode('book', '')
    p[0].append(p[1])
  else:
    p[1].append(p[2])
    p[0] = p[1]
def p_sect1(p):
  '''sect1 : HEAD1 content1s'''
  p[0] = p[1]
  for c in p[2]:
    p[0].append(c) 
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
  r'sect2 : HEAD2 content2s'
  p[0] = p[1]
  for c in p[2]:
    p[0].append(c) 
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
  r'sect3 : HEAD3 content3s'
  p[0] = p[1]
  for c in p[2]:
    msg = str(c)
    print msg.decode('utf8').encode('cp950')
    p[0].append(c) 
def p_content3s(p):
  '''content3s : content
               | content3s content'''
  if len(p) == 2:
    p[0] = [p[1]]
  else:
    p[1].append(p[2])
    p[0] = p[1]
def p_content(p):
  '''content : para EMPTYLINE
             | code
             | table
             | list
             | list EMPTYLINE
             | footnotes EMPTYLINE
  '''
  if isinstance(p[1], str):
    p[0] = DocTreeNode('para', p[1])
  else:
    p[0] = p[1]
def p_listhead(p):
  '''listhead : LI 
              | OL 
              | LI l2para EMPTYLINE
              | OL l2para EMPTYLINE 
              | DL l2para EMPTYLINE
  '''
  p[0] = p[1]
  if len(p) == 4 and p[2]:
    p[0].value += p[2].value
def p_listitem(p):
  '''listitem : listhead 
              | listhead l2paras
  '''
  p[0] = p[1]
  if len(p) == 3 and p[2]:
    for l2para in p[2]:
      p[0].append(l2para)
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
def p_para(p):
  '''para : LINE
          | para LINE
  '''
  if len(p) == 2:
    p[0] = DocTreeNode('l2para', p[1])
  else:
    p[1].value +=  p[2]
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
def p_l2paras(p): # nested 2 level paragraph
  '''l2paras : l2para EMPTYLINE
             | l2paras l2para EMPTYLINE
  '''
  if len(p) == 3:
    p[0] = [p[1]]
  elif len(p) == 4:
    p[1].append(p[2])
    p[0] = p[1]
def p_table(p): # nested 2 level paragraph
  '''table : TABLEHEAD LINE ROWSEP EMPTYLINE
  '''
  p[1].value = p[2] + p[3]
  p[0] = p[1]
def p_footnotes(p):
  '''footnotes : FOOTNOTE
               | footnotes FOOTNOTE'''
  if len(p) == 2:
    p[0] = DocTreeNode('footnotes', '')
    p[0].append(p[1])
  else:
    p[1].append(p[2])
    p[0] = p[1]
def p_error(t):
    print ("Syntax error at '%s'" \
          % str(t)).decode('utf8').encode('cp950')
parser = yacc.yacc()
if __name__ == '__main__':
  # Give the lexer some input
  #lexer.writetab('lextab')
  #lexer.read(r"d:\stxt\doc\net\x_25.stx")
  # Tokenize
  #while True:
  #  tok = lexer.token()
  #  if not tok: break      # No more input
  #  print str(tok).decode('utf8').encode('cp950')
  d = parser.read(r"d:\stxt\doc\net\ipsec.stx", debug=1)
  #d = parser.read(r"d:\stxt\doc\net\ipsec.stx")
  d.print_type_tree(3)
  for c in d.dfs():
    print c.type
