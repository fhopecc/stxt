# coding=utf-8
from __future__ import with_statement
import sys, os, re, unittest, stxt_parser
#from pygments import highlight
#from pygments.lexers import PythonLexer
#from pygments.formatters import HtmlFormatter
def disp(tree):
  return globals()['f_' + tree.type](tree)

def make_sect_list(doctree):
  o = '主題列表<br/>'
  for sect1 in doctree.children:
    o += r'<a href="%s">%s%s</a><br/>' %\
         (f_filename(sect1), f_section_number(sect1), sect1.title)
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
      fn = r'd:\stxt\structedtext\%s\%s' % \
            (bookdir, f_filename(sect1))
      with open(fn, 'w') as f:
        f.write(t % \
            {'title': sect1.title, 
             'sect_list': make_sect_list(d), 
             'content': to_html(sect1)
            })

def to_html(tree):
  return disp(tree)

def f_sect1(tree):
  html = '<h1>%s%s</h1>\n'%(f_section_number(tree), tree.title)
  for c in tree.children:
    html += to_html(c)
  return html

def f_sect2(tree):
  html = '<h2>%s%s</h2>\n'%(f_section_number(tree), tree.title)
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
  html += '<pre>%s</pre>\n' % tree.value
  #highlight(tree.value.decode('utf8'), PythonLexer(), HtmlFormatter())
  #print highlight(tree.value, PythonLexer(), HtmlFormatter())
  return html

def f_table(tree):
  html  = '<h4>表%s：%s</h4>\n'%(tree.occurence,  tree.title)
  html += '<pre>\n' + tree.value + '</pre>\n'
  return html

def f_image(tree):
  html  = '<h4>圖%s：%s</h4>\n'%(tree.occurence,  tree.title)
  html += '<img src="images/%s" alt="%s"' % (tree.name, tree.title)
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
    to_web(sys.argv[1])
  except IndexError:
    print usage
