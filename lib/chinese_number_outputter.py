# coding=utf-8
from __future__ import with_statement
import sys, os, re, unittest, stxt_parser
from pygments import highlight
from pygments.lexers import PythonLexer
from pygments.formatters import HtmlFormatter
def disp(tree):
  return globals()['f_' + tree.type](tree)

def make_sect_list(doctree):
  o = '主題列表<br/>'
  for sect1 in doctree.children:
    o += r'<a href="%s.html">%s%s</a><br/>' %\
         (f_section_number(sect1), f_section_number(sect1), sect1.title)
  return o

def to_html(file):
  d = stxt_parser.parser.read(file)
  d.number_children()
  d.count_occurence()
  title = os.path.basename(file)
  with open(r'd:\stxt\template\single_html.html') as tfn:
    t = tfn.read()
    return t % {'title': title, 'content': disp(d)}

def f_section_number(tree):
  ns = tree.section_number(0)[0]
  cdigits = ['零','一','二','三','四','五','六','七','八','九']
  if tree.type == 'sect2':
    return cdigits[ns] + '.'
  elif tree.type == 'sect3':
    return '(' + cdigits[ns] + ')' + ' '

def f_book(tree):
  html = ''
  for sect1 in tree.children:
    html += disp(sect1)
  return html

def f_sect1(tree):
  html = '<h1>%s</h1>\n' % tree.title
  for c in tree.children:
    html += disp(c)
  return html

def f_sect2(tree):
  html = '<h2>%s%s</h2>\n' % (f_section_number(tree), tree.title)
  for c in tree.children:
    html += disp(c)
  return html

def f_sect3(tree):
  html =  '<h3>%s%s</h3>\n' % (f_section_number(tree), tree.title)
  for c in tree.children:
    html += disp(c)
  return html

def f_code(tree):
  html  = '<h4>表%s：%s</h4>\n'%(tree.occurence,  tree.title)
  html += '<pre>' + tree.value + '</pre>\n'
  #html += highlight(tree.value, PythonLexer(),
      #HtmlFormatter())
  #.decode('ascii').encode('utf8')
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

if __name__ == '__main__':
  usage  = 'USAGE:' + os.path.basename(__file__) + " stxt" + '\n'
  try:
    print to_html(sys.argv[1])
  except IndexError:
    print usage
