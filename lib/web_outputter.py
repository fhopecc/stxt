# coding=utf-8
from __future__ import with_statement
import sys, os, re, unittest, stxt_parser, logger
#from pygments import highlight
#from pygments.lexers import PythonLexer
#from pygments.formatters import HtmlFormatter
sourcefile = ""
template   = ""
webdir     = ""

def disp(tree):
    return globals()['f_' + tree.type](tree)

def make_sect1_list(tree):
    o = ''
    for sect1 in tree.children:
        o += r'<a href="%s">%s%s</a>' %\
                 (f_filename(sect1), f_section_number(sect1), sect1.title)
    return o

def make_sect2_list(tree):
    o = '主題列表<br/>'
    sect2s = [sect2 for sect2 in tree.children if sect2.type == 'sect2']
    for sect2 in sect2s:
        o += r'<a href="%s">%s%s</a><br/>' %\
                 (f_filename(sect2), f_section_number(sect2), sect2.title)
    return o

def to_web(file):
    # make doctree
    global sourcefile
    global template
    global webdir

    sourcefile = file
    m = re.match(r".*\\([^\\]*)\\.*$", file)
    webdir = m.group(1)

    with open(r'd:\stxt\template\web_section.html') as tfn:
        template = tfn.read()

    d = stxt_parser.parser.read(file)
    d.number_children()
    d.count_occurence()
    
    html = ''
    for sect1 in d.children:
        html += '<h1><a href="%s">%s%s</a></h1>\n' % \
               (f_filename(sect1), f_section_number(sect1), sect1.title)
        disp(sect1)
    f_index(d)

def f_index(tree):
    o = ''
    for sect1 in tree.children:
        o += '<h4><a href="%s">%s%s</a></h4>\n' % \
             (f_filename(sect1), f_section_number(sect1), sect1.title)
        for sect2 in (c for c in sect1.children if c.type == 'sect2'):
            o += r'<h4><a href="%s">%s%s</a><h4/>' % \
                 (f_filename(sect2), f_section_number(sect2), sect2.title)

    # write index.html
    index = r'd:\stxt\structedtext\%s\%s' % (webdir, 'index.html')
    with open(index, 'w') as f:
        f.write(template % \
                {'title': '主索引', 
                 'sect1_list': "",
                 'sect2_list': "",
                 'content': o
                })
    logger.info('generate %s' % index)

def f_sect1(tree):
    global webdir, template
    html    = '<h1>%s</h1>\n' % tree.title
    content = [c for c in tree.children if c.type != 'sect2']
    for c in content:
        html += disp(c)

    sect2s = [sect2 for sect2 in tree.children if sect2.type == 'sect2']
    for sect2 in sect2s:
        disp(sect2)

    fn = r'd:\stxt\structedtext\%s\%s' % (webdir, f_filename(tree))
    with open(fn, 'w') as f:
        f.write(template % \
                {'title': tree.title, 
                 'sect1_list': make_sect1_list(tree.parent), 
                 'sect2_list': make_sect2_list(tree), 
                 'content': html
                })

def f_sect2(tree):
    html = '<h2>%s%s</h2>\n'%(f_section_number(tree), tree.title)
    for c in tree.children:
        html += disp(c)

    t = '' # template string
    with open(r'd:\stxt\template\web_section.html') as tfn:
        t = tfn.read()

    m = re.match(r".*\\([^\\]*)\\.*$", sourcefile)
    bookdir = m.group(1)

    fn = r'd:\stxt\structedtext\%s\%s' % \
                (bookdir, f_filename(tree))

    with open(fn, 'w') as f:
        f.write(t % \
                {'title': tree.title, 
                 'sect1_list': make_sect1_list(tree.parent.parent), 
                 'sect2_list': make_sect2_list(tree.parent), 
                 'content': html
                })
    logger.info('generate %s' % fn)
    return ''

def f_sect3(tree):
    html =    '<h3>%s%s</h3>\n'% (f_section_number(tree), tree.title)
    for c in tree.children:
        html += disp(c)
    return html

def f_code(tree):
    html    = '<h4>程式碼%s：%s</h4>\n'%(tree.occurence,  tree.title)
    #html += tree.value + '</pre>\n'
    html += '<pre>%s</pre>\n' % tree.value
    #highlight(tree.value.decode('utf8'), PythonLexer(), HtmlFormatter())
    #print highlight(tree.value, PythonLexer(), HtmlFormatter())
    return html

def f_table(tree):
    html = '<h4>表%s：%s</h4>\n'%(tree.occurence,  tree.title)
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
    html    = '<h4>圖%s：%s</h4>\n'%(tree.occurence,  tree.title)
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
    return '.'.join([str(n) for n in tree.section_number(3)]) + '.'

def f_filename(tree):
    fn = '_'.join([str(n) for n in tree.section_number(3)])
    if tree.name:
        fn = tree.name
    return '%s.html' % fn

def f_questions(tree):
    html = '<h3>習題</h3>\n'
    for i in range(0,len(tree.children)):
        c1 = tree.children[i]
        html += '<h4>題%s：%s</h4>\n' % (i+1, c1.title)
        for c2 in c1.children:
            html += disp(c2)
    return html

def f_answer(tree):
    html = '<h4>答：</h4>\n'
    for c in tree.children:
        html += disp(c)
    return html

def f_define(tree):
    html = '<h4>定義%s：%s</h4>\n'%(tree.occurence,  tree.title)
    for c in tree.children:
        html += disp(c)
    return html

def f_theorem(tree):
    html = '<h4>定理%s：%s</h4>\n'%(tree.occurence,  tree.title)
    for c in tree.children:
        html += disp(c)
    return html

def f_proof(tree):
    html = '<h4>證明：</h4>\n'
    for c in tree.children:
        html += disp(c)
    return html

def usage():
    usage = os.path.basename(__file__) + " filename\n"
    usage += 'filename: structed text file'
    return usage

if __name__ == '__main__':
    try:
        sourcefile = sys.argv[1]
        to_web(sys.argv[1])
    except IndexError:
        logger.info(usage())
