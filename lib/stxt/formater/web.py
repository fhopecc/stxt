# coding=utf-8
from __future__ import with_statement
import sys, os, re
from html import *

def disp(tree):
    if re.match(r'sect[345]', tree.type):
        return f_titled_container(tree)
    elif tree.type in ('question', 'answer', 'define', 'theorem'):
        return f_titled_container(tree)
    elif tree.type in ('proof', 'term'):
        return f_titled_container(tree)
    elif tree.type in ('para'):
        return f_element(tree)
    elif tree.type in ('list', 'listitem', 'olistitem'):
        return f_container(tree)
    else: return globals()['f_' + tree.type](tree)

def to_doc(tree):
    for sect1 in tree.children:
        disp(sect1)

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
    console.info('generate %s' % index)

def f_sect1(tree):
    html = ''
    content = [c for c in tree.children if c.type != 'sect2']
    for c in content:
        html += disp(c)

    sect1s = tree.root().children
    sect2s = [sect2 for sect2 in tree.children if sect2.type == 'sect2']
 
    with open(f_path(tree), 'w') as f:
        f.write(str(render.web_section(tree, html, 
                                       sect1s, sect2s)))

    print 'render %s' % f_path(tree)

    sect2s = [sect2 for sect2 in tree.children if sect2.type == 'sect2']
    for sect2 in sect2s:
        disp(sect2)

def f_sect2(tree):
    sect1s = tree.root().children
    sect1 = [t for t in tree.path() if t.type == 'sect1'][0]
    sect2s = [sect2 for sect2 in sect1.children if sect2.type == 'sect2']

    with open(f_path(tree), 'w') as f:
        f.write(str(render.web_section(tree, 
                        str(f_container(tree)), 
                        sect1s, sect2s)))

    print 'render %s' % f_path(tree)

def f_filename(tree):
    'The filename for specified node'
    section_number = tree.order_path()
    section_number = section_number[1:]
    fn = '_'.join([str(n+1) for n in section_number])
    if tree.name:
        fn = tree.name
    return '%s.html' % fn

def f_path(tree):
    'The file path for specified node'
    m = re.match(r".*\\([^\\]*)\\.*$", tree.root().file)
    webdir = m.group(1)
    return os.path.join("structedtext", webdir, f_filename(tree))

def usage():
    usage = os.path.basename(__file__) + " filename\n"
    usage += 'filename: structed text file'
    return usage

funcs = {'f_filename':f_filename,
         'f_section_number':f_section_number}

render = template.render('template', globals=funcs)

if __name__ == '__main__':
    try:
        sourcefile = sys.argv[1]
        tree = parser.read(sourcefile)
        to_doc(tree)
    except IndexError:
        console.info(usage())
