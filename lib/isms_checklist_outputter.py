# coding=utf-8
from __future__ import with_statement
import sys, os, re, stxt_parser
def to_html(fn):
  file = fn
  doctree = stxt_parser.parser.read(file)
  doctree.number_children()
  doctree.count_occurence()
  html = ''
  for sect1 in doctree.children:
    html += '<table>'
    html += disp(sect1)
    html += '</table>'
  print f_toc(doctree) + html

def disp(tree):
  return globals()['f_' + tree.type](tree)

def f_toc(tree): 
  html = '<table>'
  for sect1 in tree.children:
    html += '<tr><td>%s</td><td>%s</td></tr>' % \
        (tree.title, len(tree.children[0].children))
  html += '</table>'
  html += '<br/>'
  return html

def f_sect1(tree):
  tds = r'<tr><th>稽核項目</th><th>受檢單位</th>' + \
        r'<th>事實狀況</th><th>改進建議事項</th></tr>'
  html = '<tr><th>%s</th><th colspan="3">%s</th></tr>\n%s'% \
      (tree.section_number(), tree.title, tds)
  html += disp(tree.children[0])
  html += r'<br/>'
  return html

def f_olist(tree):
  html = ''
  sect_num = tree.parent.section_number()
  index = 0
  for li in tree.children:
    index += 1
    html += '<tr><td>%s.%s</td><td>%s</td><td></td><td></td></tr>\n' % \
          (sect_num, index, disp(li))
  return html

def f_olistitem(tree):
  return tree.value

if __name__ == '__main__':
  to_html('doc\isms\checklist_98.stx')
  #usage = os.path.basename(__file__) + " filename"
  #try:
  #  to_html(sys.argv[1])
  #except IndexError:
  #  print usage
  #i = globals().copy()
  #for n in [f for f in i.iterkeys() if f[0:2] == 'f_']:
  #  print n
