# coding=utf-8
from __future__ import with_statement
import sys, os, re, booker, template
from template import Template
from html_outputter import *

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
    render = template.render('template')
    for sect1 in tree.children:
        disp(sect1)

def sect1_list(tree):
    o = ''
    root = tree.root()
    for sect1 in root.children:
        o += r'<a href="%s">%s%s</a>' %\
                 (f_filename(sect1), f_section_number(sect1), sect1.title)
    return o

def sect2_list(tree):
    o = '主題列表<br/>'
    sect1 = [t for t in tree.path() if t.type == 'sect1'][0]
    sect2s = [sect2 for sect2 in sect1.children if sect2.type == 'sect2']
    for sect2 in sect2s:
        o += r'<a href="%s">%s%s</a><br/>' %\
                 (f_filename(sect2), f_section_number(sect2), sect2.title)
    return o

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
    render = template.render('template')
    html = ''
    content = [c for c in tree.children if c.type != 'sect2']
    for c in content:
        html += disp(c)

    print 'render %s' % f_path(tree)
    with open(f_path(tree), 'w') as f:
        f.write(str(render.web_section(tree.name, html, 
                                       sect1_list(tree), sect2_list(tree))))

    sect2s = [sect2 for sect2 in tree.children if sect2.type == 'sect2']
    for sect2 in sect2s:
        disp(sect2)

def f_sect2(tree):
    render = template.render('template')
    print 'render %s' % f_path(tree)
    with open(f_path(tree), 'w') as f:
        f.write(str(render.web_section(tree.name, 
                        str(f_titled_container(tree)), 
                        sect1_list(tree), sect2_list(tree))))

def f_filename(tree):
    fn = '_'.join([str(n) for n in tree.section_number(3)])
    if tree.name:
        fn = tree.name
    return '%s.html' % fn

def f_path(tree):
    m = re.match(r".*\\([^\\]*)\\.*$", tree.root().file)
    webdir = m.group(1)
    return os.path.join("structedtext", webdir, f_filename(tree))

def usage():
    usage = os.path.basename(__file__) + " filename\n"
    usage += 'filename: structed text file'
    return usage

if __name__ == '__main__':
    try:
        sourcefile = sys.argv[1]
        tree = booker.read(sourcefile)
        to_doc(tree)
    except IndexError:
        logger.info(usage())
