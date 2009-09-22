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
    o += r'<a href="%s.html">%s.%s</a><br/>' %\
         (sect1.section_number(), sect1.section_number(), sect1.title)
  return o

def to_web(file):
  # make doctree
  d = stxt_parser.parser.read(file)
  d.number_children()
  d.count_occurence()
  with open(r'd:\stxt\template\web_section.html') as tfn:
    t = tfn.read()
    for sect1 in d.children:
      m = re.match(r".*\\([^\\]*)\\.*$", file)
      bookdir = m.group(1)
      fn = r'd:\stxt\structedtext\%s\%s.html' % \
            (bookdir, sect1.section_number())
      with open(fn, 'w') as f:
        f.write(t % \
            {'title': sect1.title, 
             'sect_list': make_sect_list(d), 
             'content': to_html(sect1)
            })

def to_html(tree):
  return disp(tree)

def f_sect1(tree):
  html = '<h1>%s.%s</h1>\n'%(tree.section_number(), tree.title)
  for c in tree.children:
    html += to_html(c)
  return html

def f_sect2(tree):
  html = '<h2>%s.%s</h2>\n'%(tree.section_number(), tree.title)
  for c in tree.children:
    html += to_html(c)
  return html

def f_sect3(tree):
  html =  '<h3>%s</h3>\n'% tree.title
  for c in tree.children:
    html += to_html(c)
  return html

def f_code(tree):
  html  = '<h4>程式碼%s：%s</h4>\n'%(tree.occurence,  tree.title)
  #html += tree.value + '</pre>\n'
  html += highlight(tree.value, PythonLexer(),
      HtmlFormatter()).decode('ascii').encode('utf8')
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
      html += to_html(np)
    html += '</li>\n'
  html += '</ul>\n'
  return html

def f_olist(tree):
  html = '<ol>\n'
  for c in tree.children:
    html += '<li>' + c.value
    for np in c.children:
      html += to_html(np)
    html += '</li>\n'
  html += '</ol>\n'
  return html

def f_dlist(tree):
  html = '<dl>\n'
  for c in tree.children:
    html += '<dt>%s</dt>\n' % c.value
    html += '<dd>'
    for np in c.children:
      html += to_html(np)
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
  usage = os.path.basename(__file__) + " filename"
  try:
    to_web(sys.argv[1])
  except IndexError:
    print usage
