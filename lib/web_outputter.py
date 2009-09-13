# coding=utf-8
from __future__ import with_statement
import sys, os, re, unittest, stxt_parser
from pygments import highlight
from pygments.lexers import PythonLexer
from pygments.formatters import HtmlFormatter

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

class Outputter:
  def __init__(self, file):
    #d = stxt.parser.read(r"d:\stxt\stxt\db\concurrent_control.stx")
    self.file = file
    #d = stxt.parser.read(file)
    d = stxt_parser.parser.read(file)
    d.number_children()
    d.count_occurence()
    self.doctree = d
  def output(self):
    print self.doctree.print_tree().decode('utf8').encode('cp950')
class WEBOutputter(Outputter):
  def make_sect_list(self):
    o = '主題列表<br/>'
    for sect1 in self.doctree.children:
      o += r'<a href="%s.html">%s.%s</a><br/>' %\
           (sect1.section_number(), sect1.section_number(), sect1.title)
    return o
  def to_web(self):
    for sect1 in self.doctree.children:
      t = ''
      tfn = r'd:\stxt\template\web_section.html'
      with open(tfn) as f:
        t = f.read()
      m = re.match(r".*\\([^\\]*)\\.*$", self.file)
      lastdir = m.group(1)
      fn = r'd:\stxt\structedtext\%s\%s.html' % \
            (lastdir, sect1.section_number())
      with open(fn, 'w') as f:
        f.write(t % \
            {'title': sect1.title, 
             'sect_list': self.make_sect_list(), 
             'content': self.to_html(sect1)
            })
  def to_html(self, tree):
    html = ''
    if tree.type == 'sect1':
      html =  '<h1>%s.%s</h1>\n'%(tree.section_number(), tree.title)
      for c in tree.children:
        html += self.to_html(c)
    elif tree.type == 'sect2':
      html =  '<h2>%s.%s</h2>\n'%(tree.section_number(), tree.title)
      for c in tree.children:
        html += self.to_html(c)
    elif tree.type == 'sect3':
      html =  '<h3>%s</h3>\n'% tree.title
      for c in tree.children:
        html += self.to_html(c)
    elif tree.type == 'code':
      return f_code(tree)
    elif tree.type == 'table':
      return f_table(tree)
    elif tree.type in ('para', 'l2para'):
      html = '<p>\n' + tree.value + '</p>\n'
    elif tree.type in ('list'):
      html += '<ul>\n'
      for c in tree.children:
        html += '<li>\n' + c.value 
        for np in c.children:
          html += self.to_html(np)
        html += '</li>\n'
      html += '</ul>\n'
    elif tree.type == 'olist':
      html += '<ol>\n'
      for c in tree.children:
        html += '<li>' + c.value
        for np in c.children:
          html += self.to_html(np)
        html += '</li>\n'
      html += '</ol>\n'
    elif tree.type == 'dlist':
      html += '<dl>\n'
      for c in tree.children:
        html += '<dt>%s</dt>\n' % c.value
        html += '<dd>'
        for np in c.children:
          html += self.to_html(np)
        html += '</dd>'
      html += '</dl>\n'
    elif tree.type == 'footnotes':
      html += '<ul>\n'
      for c in tree.children:
        html += '<li>' + c.value+ '</li>\n'
      html += '</ul>\n'
    else:
      raise ValueError, 'No output function for ' + tree.type
    return html
    #html = ' ' * tree.height() + tree.type + '\n'
    #for c in tree.children:
    #  if c.type == 'list':
    #    html += '<ul>\n'
    #    for i in c.children:
    #      html += '<li>' + i.value + '</li>\n'
    #    html += '</ul>\n'
    #  else:
    #    html += to_html(c)
    #return html
def to_web(fn):
  o = WEBOutputter(fn)
  o.to_web()

if __name__ == '__main__':
  usage = os.path.basename(__file__) + " filename"
  try:
    #o = WEBOutputter(r"d:\stxt\stxt\db\concurrent_control.stx")
    to_web(sys.argv[1])
  except IndexError:
    print usage
