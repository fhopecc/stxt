# coding=utf-8
# for single html
from __future__ import with_statement
import sys, os, re, unittest, booker
#from pygments import highlight
#from pygments.lexers import PythonLexer
#from pygments.formatters import HtmlFormatter
def disp(tree):
    type = tree.type

    head  = r'image|question|answer|define|theorem|'
    head += r'proof'

    if re.match(r'sect\d', tree.type):
        type = 'sect'
    elif re.match(head, tree.type):
        type = 'element'
    return globals()['f_' + type](tree)

def to_html(file):
    d = None
    with open(file) as f:
        d = booker.parse(f.read())
    d.number_children()
    d.count_occurence()
    title = os.path.basename(file)
    with open(r'd:\stxt\template\single_html.html') as tfn:
        t = tfn.read()
        return t % {'title': title, 'content': disp(d)}

def f_doc(tree):
    html = ''
    for e in tree.children:
        html += disp(e)
    return html

def f_sect(tree):
    html = '<div class="title">%s%s</div>\n' % \
            (f_section_number(tree), tree.value)
    for c in tree.children:
        html += disp(c)
    html = '<div class="%s">\n%s\n</div>' % (tree.type, html)
    return html

def f_term(tree):
    html = '<div class="title">%s</div>\n' % tree.value
    for c in tree.children: html += disp(c)
    html = '<div class="%s">\n%s\n</div>' % (tree.type, html)
    return html

def f_type_label(tree):
    return {'define'  : '定義', 
            'theorem' : '定理', 
            'proof'   : '證明', 
            'question': '題', 
            'answer'  : '答', 
            'image'   : '圖' 
           }[tree.type]

def f_element(tree):
    html = '<div class="title">%s%s：%s</div>\n' % \
            (f_type_label(tree), tree.order() + 1, tree.value)
    for c in tree.children: html += disp(c)
    html = '<div class="%s">\n%s\n</div>' % (tree.type, html)
    return html

def f_image(tree):
    html = '<div class="title">圖%s：%s</div>\n' % \
            (tree.order(), tree.value)

    html += '<img src="images/%s" alt="%s"' % (tree.name, tree.title)
    return html

def f_answer(tree):
    html = '<h4>答：</h4>\n'
    for c in tree.children:
        html += disp(c)
    return html

def f_sect2(tree):
    html = '<h2>%s%s</h2>\n'%(f_section_number(tree), tree.title)
    for c in tree.children:
        html += disp(c)
    return html

def f_sect3(tree):
    html =    '<h3>%s</h3>\n'% tree.title
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
    html    = '<h4>表%s：%s</h4>\n'%(tree.occurence,    tree.title)
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

def f_footnotes(tree):
    html = '<ul>\n'
    for c in tree.children:
        html += '<li>' + c.value+ '</li>\n'
    html += '</ul>\n'
    return html

def f_section_number(tree):
    level = tree.level
    n = '%s.' % (tree.order() + 1)
    for l in range(1, level):
        tree = tree.parent
        if not tree: break
        n = '%s.%s' % (tree.order() + 1, n)
    return n

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
