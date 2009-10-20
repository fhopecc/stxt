# coding=utf-8
from __future__ import with_statement
import sys, os, re, unittest, stxt_parser
#from pygments import highlight
#from pygments.lexers import PythonLexer
#from pygments.formatters import HtmlFormatter
def disp(tree):
    return globals()['f_' + tree.type](tree)

def to_slides(dir, file):
    # make doctree
    d = stxt_parser.parser.read(file)
    d.number_children()
    d.count_occurence()
    for sect1 in d.children:
        disp(sect1)

def f_sect1(tree):
    html = '<h1>%s%s</h1>\n'%(f_section_number(tree), tree.title)

    t = ''
    with open(r'd:\stxt\template\slides.html') as tfn:
        t = tfn.read()

    sect2s = [sect2 for sect2 in tree.children if sect2.type == 'sect2']
    for sect2 in sect2s:
        fn = r'd:\stxt\structedtext\%s\%s' % (dir, f_filename(sect2))
        with open(fn, 'w') as f:
            f.write(t % \
                    {'title': sect2.title, 
                     'content': disp(sect2)
                    })

def f_sect2(tree):
    html =  '<h2>%s</h2>\n'% tree.title
    html += '<div id="h2child">'
    for c in tree.children:
        html += disp(c)
    html += '</div>'
    return html

def f_sect3(tree):
    html = '<h3>%s%s</h3>\n'% (f_section_number(tree), tree.title)
    for c in tree.children:
        html += disp(c)
    return html

def f_code(tree):
    html    = '<h4>程式碼%s：%s</h4>\n'%(tree.occurence,    tree.title)
    #html += tree.value + '</pre>\n'
    html += '<pre>%s</pre>\n' % tree.value
    #highlight(tree.value.decode('utf8'), PythonLexer(), HtmlFormatter())
    #print highlight(tree.value, PythonLexer(), HtmlFormatter())
    return html

def f_table(tree):
    html = '<h4>表%s：%s</h4>\n'%(tree.occurence,    tree.title)
    if tree.children:
        html += '<table>\n'
        for row in tree.children:
            html += '<tr>\n' 
            for col in row.children:
                html += '<td>%s</td>\n' % col.value.encode('utf8')
            html += '</tr>\n' 
        html += '</table>\n'
    else:
        html += '<pre>%s</pre>\n' % tree.value 
    return html

def f_image(tree):
    html = '<img src="images/%s" alt="%s"' % (tree.name, tree.title)
    html += '<h4>%s</h4>\n' % tree.title
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
    fn = '_'.join([str(n) for n in tree.section_number(3)])
    if tree.name:
        fn = tree.name
    return '%s.html' % fn

if __name__ == '__main__':
    usage = os.path.basename(__file__) + " dir filename"
    try:
        dir, filename = sys.argv[1], sys.argv[2]
        to_slides(sys.argv[1], sys.argv[2])
    except IndexError:
        print usage
