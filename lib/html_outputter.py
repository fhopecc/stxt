# coding=utf-8
# for single html
from __future__ import with_statement
import sys, os, re, unittest, stxt_parser
from pygments import highlight
from pygments.lexers import PythonLexer
from pygments.formatters import HtmlFormatter
def disp(tree):
  return globals()['f_' + tree.type](tree)

def to_html(file):
  d = stxt_parser.parser.read(file)
  d.number_children()
  d.count_occurence()
  title = os.path.basename(file)
  with open(r'd:\stxt\template\single_html.html') as tfn:
    t = tfn.read()
    return t % {'title': title,  'content': disp(d) }

def f_book(tree):
  html = ''
  for sect1 in tree.children:
    html += disp(sect1)
  return html

def f_sect1(tree):
  html = '<h1>%s%s</h1>\n'%(f_section_number(tree), tree.title)
  for c in tree.children:
    html += disp(c)
  return html

def f_sect2(tree):
  html = '<h2>%s%s</h2>\n'%(f_section_number(tree), tree.title)
  for c in tree.children:
    html += disp(c)
  return html

def f_sect3(tree):
  html =  '<h3>%s</h3>\n'% tree.title
  for c in tree.children:
    html += disp(c)
  return html

def f_code(tree):
  html  = '<h4>程式碼%s：%s</h4>\n'%(tree.occurence,  tree.title)
  #html += tree.value + '</pre>\n'
  html += '<pre>%s</pre>\n' % tree.value
  #highlight(tree.value.decode('utf8'), PythonLexer(), HtmlFormatter())
  #print highlight(tree.value, PythonLexer(), HtmlFormatter())
  return html

def f_table(tree):
  html  = '<h4>表%s：%s</h4>\n'%(tree.occurence,  tree.title)
  html += '<pre>\n' + tree.value + '</pre>\n'
  return html

def f_para(tree):
  return '<p>\n' + tree.value + '</p>\n'

def f_l2para(tree):
  return '<p>\n' + tree.value + '</p>\n'

def f_list(tree):
  html = '<ul>\n'
  for c in tree.children:
    html += '<li>\n' + c.value 
    for np in c.children:
      html += disp(np)
    html += '</li>\n'
  html += '</ul>\n'
  return html

def f_olist(tree):
  html = '<ol>\n'
  for c in tree.children:
    html += '<li>' + c.value
    for np in c.children:
      html += disp(np)
    html += '</li>\n'
  html += '</ol>\n'
  return html

def f_dlist(tree):
  html = '<dl>\n'
  for c in tree.children:
    html += '<dt>%s</dt>\n' % c.value
    html += '<dd>'
    for np in c.children:
      html += disp(np)
    html += '</dd>'
  html += '</dl>\n'
  return html

def f_footnotes(tree):
  html = '<ul>\n'
  for c in tree.children:
    html += '<li>' + c.value+ '</li>\n'
  html += '</ul>\n'
  return html

def f_section_number(tree):
  ns = tree.section_number(3)
  w = ''
  for n in ns:
    w += str(n) + '.'
  return w

def f_filename(tree):
  fn = tree.section_number()[0]
  if tree.name:
    fn = tree.name
  return '%s.html' % fn

if __name__ == '__main__':
  usage = os.path.basename(__file__) + " filename"
  try:
    print to_html(sys.argv[1])
  except IndexError:
    print usage
