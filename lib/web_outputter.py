# coding=utf-8
from __future__ import with_statement
import os, re, unittest, stxt
class Outputter:
  def __init__(self, file):
    p = stxt.parse_file(r"d:\stxt\stxt\db\concurrent_control.stx")
    p.parse()
    self.doctree = p.tree.to_doctree()
  def output(self):
    print self.doctree.print_tree().decode('utf8').encode('cp950')
class WEBOutputter(Outputter):
  def to_web(self):
    for sect1 in self.doctree.children:
      t = ''
      tfn = r'd:\stxt\template\web_section.html'
      with open(tfn) as f:
        t = f.read()
      fn = r'd:\stxt\public\db\%s.html' % sect1.section_number() 
      with open(fn, 'w') as f:
        f.write(t % 
            {'title': sect1.title, 
             'content':self.to_html(sect1)
            })
  def to_html(self, tree):
    html = ''
    if tree.type == 'sect1':
      html =  '<h1>' + tree.title +'</h1>\n'
      for c in tree.children:
        html += self.to_html(c)
    elif tree.type == 'sect2':
      html =  '<h2>' + tree.title +'</h2>\n'
      for c in tree.children:
        html += self.to_html(c)
    elif tree.type == 'code':
      html  = '<h4>程式碼：' + tree.title +'</h4>\n'
      html += '<pre>\n' + tree.children[0].value + '</pre>\n'
    elif tree.type == 'PARA':
      html = '<p>\n' + tree.value + '</p>\n'
    elif tree.type == 'list':
      html += '<ul>\n'
      for c in tree.children:
        html += '<li>' + c.value + '</li>\n'
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
if __name__ == '__main__':
  o = WEBOutputter(r"d:\stxt\stxt\db\concurrent_control.stx")
  #o.output()
  o.to_web()
